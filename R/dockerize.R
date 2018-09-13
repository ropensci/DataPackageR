

.getSessionInfo <- function(){
  sessionInfo <- sessionInfo()
  sessionInfo
}

.saveSessionInfo <- function(){
  sessionInfo <- .getSessionInfo()
  save(sessionInfo, file = file.path(project_extdata_path(),"sessionInfo.RData"))
}

#' @importFrom containerit Cmd dockerfile
.buildCommand <- function() {
  cmd <- containerit::Cmd(exec = "/bin/bash", params = c("-c", "Rscript -e 'DataPackageR::package_build()';cp ../*.tar.gz /host"))
  cmd
}

.buildDockerFile <- function(exclude = NULL){
  cmd <- .buildCommand()
  .saveSessionInfo()
  df <- .dp_dockerfile(from = project_extdata_path("sessionInfo.RData"), copy = "script_dir", cmd = cmd, exclude = exclude)
  df@instructions[[1]]@commands[2] <- paste0(df@instructions[[1]]@commands[2]," \\\n\tzlib1g-dev")
  df
}

.writeDockerFile <- function(exclude = NULL) {
  df <- .buildDockerFile(exclude = exclude)
  containerit::write(df,file = file.path(project_path(),"Dockerfile"))
  return(df)
}

#'@importFrom containerit parseFrom extract_session_file clean_session LabelMaintainer Workdir getImageForVersion Copy addInstruction getRVersionTag
#'@importFrom stringr str_detect
.dp_dockerfile <- function(from = utils::sessionInfo(), image = containerit::getImageForVersion(containerit::getRVersionTag(from)), 
          maintainer = Sys.info()[["user"]], save_image = FALSE, envir = .GlobalEnv, 
          env = list(generator = paste("containerit", utils::packageVersion("containerit"))), 
          soft = FALSE, offline = FALSE, copy = NA, container_workdir = "/payload/", 
          cmd = Cmd("R"), entrypoint = NULL, add_self = FALSE, silent = FALSE, 
          predetect = TRUE, versioned_libs = FALSE, versioned_packages = FALSE, 
          filter_baseimage_pkgs = FALSE, exclude = NULL) 
{
  if (silent) {
    invisible(futile.logger::flog.threshold(futile.logger::WARN))
  }
  .dockerfile <- NA
  .originalFrom <- class(from)
  if (is.character(image)) {
    image <- containerit::parseFrom(image)
  }
  futile.logger::flog.debug("Creating a new Dockerfile from '%s' with base image %s", 
                            from, toString(image))
  if (is.character(maintainer)) {
    .label <- containerit::Label_Maintainer(maintainer)
    futile.logger::flog.debug("Turning maintainer character string '%s' into label: %s", 
                              maintainer, toString(.label))
    maintainer <- .label
  }
  if (!inherits(x = cmd, "Cmd")) {
    stop("Unsupported parameter for 'cmd', expected an object of class 'Cmd', given was :", 
         class(cmd))
  }
  if (!is.null(entrypoint) && !inherits(x = entrypoint, "Entrypoint")) {
    stop("Unsupported parameter for 'entrypoint', expected an object of class 'Entrypoint', given was :", 
         class(entrypoint))
  }
  workdir <- NULL
  if (!is.null(container_workdir)) {
    if (!is.character(container_workdir)) {
      stop("Unsupported parameter for 'container_workdir', expected a character string or NULL")
    }
    else {
      if (!stringr::str_detect(container_workdir, "/$")) {
        container_workdir <- paste0(container_workdir, 
                                    "/")
        futile.logger::flog.info("Appended trailing slash, workdir is '%s'", 
                                 container_workdir)
      }
      workdir <- containerit::Workdir(container_workdir)
    }
  }
  image_name <- image@image
  if (!image_name %in% containerit:::.supported_images) {
    warning("Unsupported base image. Proceed at your own risk. The following base images are supported:\n", 
            paste(containerit:::.supported_images, collapse = "\n"))
  }
  .dockerfile <- methods::new("Dockerfile", instructions = list(), 
                              maintainer = maintainer, image = image, entrypoint = entrypoint, 
                              cmd = cmd)
  if (is.null(from)) {
    futile.logger::flog.debug("from is NULL, not deriving any information at all")
    if (!is.null(workdir)) 
      containerit::addInstruction(.dockerfile) <- workdir
  }
  else if (inherits(x = from, "sessionInfo")) {
    futile.logger::flog.debug("Creating from sessionInfo object")
    .dockerfile <- dockerfileFromSession.sessionInfo(session = from, 
                                         dockerfile = .dockerfile, soft = soft, offline = offline, 
                                         add_self = add_self, versioned_libs = versioned_libs, 
                                         versioned_packages = versioned_packages, filter_baseimage_pkgs = filter_baseimage_pkgs, 
                                         workdir = workdir, exclude = exclude)
  }
  else if (inherits(x = from, "character")) {
    futile.logger::flog.debug("Creating from character string '%s'", 
                              from)
    if (dir.exists(from)) {
      futile.logger::flog.debug("'%s' is a directory", 
                                from)
      .originalFrom <- from
      .dockerfile <- containerit:::dockerfileFromWorkspace(path = from, 
                                             dockerfile = .dockerfile, soft = soft, offline = offline, 
                                             add_self = add_self, copy = copy, silent = silent, 
                                             predetect = predetect, versioned_libs = versioned_libs, 
                                             versioned_packages = versioned_packages, filter_baseimage_pkgs = filter_baseimage_pkgs, 
                                             workdir = workdir)
    }
    else if (file.exists(from)) {
      futile.logger::flog.debug("'%s' is a file", from)
      .originalFrom <- from
      .dockerfile <- dockerfileFromFile(file = from, dockerfile = .dockerfile, 
                                        soft = soft, offline = offline, add_self = add_self, 
                                        copy = copy, silent = silent, predetect = predetect, 
                                        versioned_libs = versioned_libs, versioned_packages = versioned_packages, 
                                        filter_baseimage_pkgs = filter_baseimage_pkgs, 
                                        workdir = workdir, exclude = exclude)
    }
    else {
      stop("Unsupported string for 'from' argument (not a file, not a directory): ", 
           from)
    }
  }
  else if (is.expression(from) || (is.list(from) && all(sapply(from, 
                                                               is.expression)))) {
    futile.logger::flog.debug("Creating from expession: '%s' with a clean session", 
                              toString(from))
    .sessionInfo <- containerit::clean_session(expr = from)
    .dockerfile <- containerit:::dockerfileFromSession(session = .sessionInfo, 
                                         dockerfile = .dockerfile, soft = soft, offline = offline, 
                                         add_self = add_self, versioned_libs = versioned_libs, 
                                         versioned_packages = versioned_packages, filter_baseimage_pkgs = filter_baseimage_pkgs, 
                                         workdir = workdir)
  }
  else {
    stop("Unsupported 'from': ", class(from), " ", from)
  }
  .filename = ".RData"
  if ("save_image_filename" %in% names(save_image)) {
    .filename <- save_image$save_image_filename
  }
  if (isTRUE(save_image)) {
    futile.logger::flog.debug("Saving image to file %s with %s and adding COPY instruction using environment %s", 
                              .filename, toString(ls(envir = envir)), utils::capture.output(envir))
    save(list = ls(envir = envir), file = .filename, envir = envir)
    containerit::addInstruction(.dockerfile) <- containerit::Copy(src = .filename, 
                                        dest = .filename)
  }
  else if (is.list(save_image)) {
    futile.logger::flog.debug("Saving image using to file %s and adding COPY instruction based on %s", 
                              .filename, toString(save_image))
    save(list = unlist(save_image[names(save_image) != "save_image_filename"]), 
         file = .filename, envir = envir)
    containerit::addInstruction(.dockerfile) <- containerit::Copy(src = .filename, 
                                        dest = .filename)
  }
  futile.logger::flog.info("Created Dockerfile-Object based on %s", 
                           .originalFrom)
  return(.dockerfile)
}





dockerfileFromFile <- function(file, dockerfile, soft, copy, offline, add_self, vanilla, 
          silent, predetect, versioned_libs, versioned_packages, filter_baseimage_pkgs, 
          workdir, exclude = NULL) 
{
  futile.logger::flog.debug("Creating from file")
  context = normalizePath(getwd())
  file = normalizePath(file)
  futile.logger::flog.debug("Working with file %s in working directory %s", 
                            file, context)
  len = stringr::str_length(context)
  substr = stringr::str_sub(context, end = len)
  if (context != substr) 
    stop("The given file is not inside the context directory!")
  rel_path <- containerit:::.makeRelative(file, context)
  if (stringr::str_detect(string = file, pattern = stringr::regex(".R$", 
                                                                  ignore_case = TRUE))) {
    futile.logger::flog.info("Processing R script file '%s' locally.", 
                             rel_path)
    sessionInfo <- containerit::clean_session(script_file = file, echo = !silent, 
                                 predetect = predetect)
  }
  else if (stringr::str_detect(string = file, pattern = stringr::regex(".rmd$", 
                                                                       ignore_case = TRUE))) {
    futile.logger::flog.info("Processing Rmd file '%s' locally using rmarkdown::render(...)", 
                             rel_path)
    sessionInfo <- containerit::clean_session(rmd_file = file, echo = !silent, 
                                 predetect = predetect)
  }
  else if (stringr::str_detect(string = file, pattern = stringr::regex(".rdata$", 
                                                                       ignore_case = TRUE))) {
    futile.logger::flog.info("Extracting session object from RData file %s", 
                             rel_path)
    sessionInfo <- containerit::extract_session_file(file)
  }
  else {
    futile.logger::flog.info("The supplied file %s has no known extension. containerit will handle it as an R script for packaging.", 
                             rel_path)
  }
  .dockerfile <- dockerfileFromSession.sessionInfo(session = sessionInfo, 
                                       dockerfile = dockerfile, soft = soft, offline = offline, 
                                       add_self = add_self, versioned_libs = versioned_libs, 
                                       versioned_packages = versioned_packages, filter_baseimage_pkgs = filter_baseimage_pkgs, 
                                       workdir = workdir, exclude = exclude)
  if (!is.null(copy) && !is.na(copy)) {
    copy = unlist(copy)
    if (!is.character(copy)) {
      stop("Invalid argument given for 'copy'")
    }
    else if (length(copy) == 1 && copy == "script") {
      rel_path_dest <- stringr::str_replace_all(rel_path, 
                                                pattern = "\\\\", replacement = "/")
      containerit::addInstruction(.dockerfile) <- containerit::Copy(rel_path, rel_path_dest)
    }
    else if (length(copy) == 1 && copy == "script_dir") {
      script_dir <- normalizePath(dirname(file))
      rel_dir <- containerit:::.makeRelative(script_dir, context)
      rel_dir_dest <- stringr::str_replace_all(rel_dir, 
                                               pattern = "\\\\", replacement = "/")
      if (!stringr::str_detect(rel_dir_dest, "/$")) 
        rel_dir_dest <- paste0(rel_dir_dest, "/")
      if (!stringr::str_detect(rel_dir, "/$")) 
        rel_dir <- paste0(rel_dir, "/")
      containerit::addInstruction(.dockerfile) <- containerit::Copy(rel_dir, rel_dir_dest)
    }
    else {
      sapply(copy, function(file) {
        if (file.exists(file)) {
          rel_path <- containerit:::.makeRelative(normalizePath(file), 
                                    context)
          rel_path_dest <- stringr::str_replace_all(rel_path, 
                                                    pattern = "\\\\", replacement = "/")
          if (dir.exists(file) && !stringr::str_detect(rel_path_dest, 
                                                       "/$")) 
            rel_path_dest <- paste0(rel_dir_dest, "/")
          containerit::addInstruction(.dockerfile) <<- containerit::Copy(rel_path, 
                                               rel_path_dest)
        }
        else {
          stop("The file ", file, ", given by 'copy', does not exist! Invalid argument.")
        }
      })
    }
  }
  return(.dockerfile)
}

dockerfileFromSession.sessionInfo <- function(session, dockerfile, soft, offline, add_self, versioned_libs, 
          versioned_packages, filter_baseimage_pkgs, workdir, exclude = NULL) 
{
  futile.logger::flog.debug("Creating from sessionInfo")
  apks <- session$otherPkgs
  lpks <- session$loadedOnly
  pkgs <- append(apks, lpks)
  if (!add_self) {
    futile.logger::flog.debug("Removing self from the list of packages")
    pkgs$containerit <- NULL
  }
  if (!is.null(exclude)) {
    futile.logger::flog.debug(paste0("Removing ",paste(exclude,collapse = ",") ," from the list of packages"))
    for (n in exclude) {
      pkgs[n] <- NULL
    }
  }
  pkgs_list <- lapply(pkgs, function(pkg) {
    if ("Package" %in% names(pkg)) 
      name <- pkg$Package
    else stop("Package name cannot be determined for ", pkg)
    if ("Priority" %in% names(pkg) && stringr::str_detect(pkg$Priority, 
                                                          "(?i)base")) {
      futile.logger::flog.debug("Skipping Priority package %s, is included with R", 
                                name)
      return(NULL)
    }
    else {
      version <- NA
      source <- NA
      if ("Repository" %in% names(pkg) && stringr::str_detect(pkg$Repository, 
                                                              "(?i)CRAN")) {
        source <- "CRAN"
        version <- pkg$Version
      }
      else if ("RemoteType" %in% names(pkg) && stringr::str_detect(pkg$RemoteType, 
                                                                   "(?i)github")) {
        source <- "github"
        version <- getGitHubRef(name, pkgs)
      }
      else {
        warning("Failed to identify a source for package ", 
                name, ". Therefore the package cannot be installed in the Docker image.\n")
      }
      return(list(name = name, version = version, source = source))
    }
  })
  pkgs_list <- pkgs_list[!vapply(pkgs_list, is.null, logical(1))]
  packages_df <- do.call("rbind", lapply(pkgs_list, as.data.frame))
  futile.logger::flog.debug("Found %s packages in sessionInfo", 
                            nrow(packages_df))
  .dockerfile <- containerit:::dockerfileFromPackages(pkgs = packages_df, 
                                        dockerfile = dockerfile, soft = soft, offline = offline, 
                                        versioned_libs = versioned_libs, versioned_packages = versioned_packages, 
                                        filter_baseimage_pkgs = filter_baseimage_pkgs, workdir = workdir)
  return(.dockerfile)
}
