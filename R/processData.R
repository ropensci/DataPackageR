#' DataPackageR
#'
#' A framework to automate the processing, tidying and packaging of raw data into analysis-ready
#' data sets as R packages.
#'
#' DataPackageR will automate running of data processing code,
#' storing tidied data sets in an R package, producing
#' data documentation stubs, tracking data object finger prints (md5 hash)
#' and tracking and incrementing a "DataVersion" string
#' in the DESCRIPTION file of the package when raw data or data
#' objects change.
#' Code to perform the data processing is passed to DataPackageR by the user.
#' The user also specifies the names of the tidy data objects to be stored,
#' documented and tracked in the final package. Raw data should be read from
#' "inst/extdata" but large raw data files can be read from sources external
#' to the package source tree.
#'
#' Configuration is controlled via the config.yml file created at the package root.
#' Its properties include a list of R and Rmd files that are to be rendered / sourced and
#' which read data and do the actual processing.
#' It also includes a list of r object names created by those files. These objects
#' are stored in the final package and accessible via the \code{data()} API.
#' The documentation for these objects is accessible via "?object-name", and md5
#' fingerprints of these objects are created and tracked.
#'
#' The Rmd and R files used to process the objects are transformed into vignettes
#' accessible in the final package so that the processing is fully documented.
#'
#' A DATADIGEST file in the package source keeps track of the data object fingerprints.
#' A DataVersion string is added to the package DESCRIPTION file and updated when these
#' objects are updated or changed on subsequent builds.
#'
#' Once the package is built and installed, the data objects created in the package are accessible via
#' the \code{data()} API, and
#' Calling \code{datapackage_skeleton()} and passing in R / Rmd file names, and r object names
#' constructs a skeleton data package source tree and an associated \code{config.yml} file.
#'
#' Calling \code{build_package()} sets the build process in motion.
#' @examples
#' # A simple Rmd file that creates one data object
#' # named "tbl".
#' if(rmarkdown::pandoc_available()){
#' f <- tempdir()
#' f <- file.path(f,"foo.Rmd")
#' con <- file(f)
#' writeLines("```{r}\n tbl = table(sample(1:10,1000,replace=TRUE)) \n```\n",con=con)
#' close(con)
#'
#' # construct a data package skeleton named "MyDataPackage" and pass
#' # in the Rmd file name with full path, and the name of the object(s) it
#' # creates.
#'
#' pname <- basename(tempfile())
#' datapackage_skeleton(name=pname,
#'    path=tempdir(),
#'    force = TRUE,
#'    r_object_names = "tbl",
#'    code_files = f)
#'
#' # call package_build to run the "foo.Rmd" processing and
#' # build a data package.
#' package_build(file.path(tempdir(), pname), install = FALSE)
#'
#' # "install" the data package
#' devtools::load_all(file.path(tempdir(), pname))
#'
#' # read the data version
#' data_version(pname)
#'
#' # list the data sets in the package.
#' data(package = pname)
#'
#' # The data objects are in the package source under "/data"
#' list.files(pattern="rda", path = file.path(tempdir(),pname,"data"), full = TRUE)
#'
#' # The documentation that needs to be edited is in "/R"
#' list.files(pattern="R", path = file.path(tempdir(), pname,"R"), full = TRUE)
#' readLines(list.files(pattern="R", path = file.path(tempdir(),pname,"R"), full = TRUE))
#' # view the documentation with
#' ?tbl
#' }
#' @docType package
#' @name DataPackageR-package
NULL


.validate_render_root <- function(x) {
  # catch an error if it doesn't exist
  render_root <-
    try(normalizePath(x,
      mustWork = TRUE,
      winslash = "/"
    ), silent = TRUE)
  if (inherits(render_root, "try-error")) {
    .multilog_error(paste0("render_root  = ", x, " doesn't exist."))
    # try creating, even if it's an old temp dir.
    # This isn't ideal. Would like to rather say it's a temporary
    # directory and use the current one..
    return(FALSE)
  }
  return(TRUE)
}


#' Process data generation code in 'data-raw'
#'
#' Assumes .R files in 'data-raw' generate rda files to be stored in 'data'.
#' Sources datasets.R which can source other R files.
#' R files sourced by datasets.R must invoke \code{sys.source('myRfile.R',env=topenv())}.
#' Meant to be called before R CMD build.
#' @name DataPackageR
#' @param arg \code{character} name of the package to build.
#' @param deps \code{logical} should scripts pass data objects to each other (default=TRUE)
#' @return logical TRUE if successful, FALSE, if not.
#' @importFrom desc desc
#' @importFrom rmarkdown render
#' @importFrom utils getSrcref modifyList
#' @importFrom usethis proj_set proj_get
DataPackageR <- function(arg = NULL, deps = TRUE) {
  # requireNamespace("futile.logger")
  pkg_dir <- arg
  pkg_dir <- normalizePath(pkg_dir, winslash = "/")
  cat("\n")
  usethis::proj_set(path = pkg_dir)
  raw_data_dir <- "data-raw"
  target <- normalizePath(file.path(pkg_dir, raw_data_dir), winslash = "/")
  raw_data_dir <- target
  
  #set the option that DataPackageR is building the package. On exit ensures when it leaves, it will set it back to false
  options("DataPackageR_packagebuilding" = TRUE)
  on.exit(options("DataPackageR_packagebuilding" = FALSE))
  
  

  # validate that render_root exists.
  # if it's an old temp dir, what then?

  if (!file.exists(target)) {
    .multilog_fatal(paste0("Directory ", target, " doesn't exist."))
    {
      stop("exiting", call. = FALSE)
    }
  } else {
    logpath <-
      normalizePath(
        file.path(pkg_dir, "inst/extdata"),
        winslash = "/"
      )
    logpath <- file.path(logpath, "Logfiles")

    dir.create(logpath, recursive = TRUE, showWarnings = FALSE)
    # open a log file
    LOGFILE <- file.path(logpath, "processing.log")
    .multilog_setup(LOGFILE)
    .multilog_thresold(console = INFO, logfile = TRACE)
    .multilog_trace(paste0("Logging to ", LOGFILE))
    # we know it's a proper package root, but we want to test if we have the
    # necessary subdirectories
    testme <- file.path(pkg_dir, c("R", "inst", "data", "data-raw"))
    if (!all(utils::file_test(testme, op = "-d"))) {
      .multilog_fatal(paste0(
        "You need a valid package data strucutre.",
        " Missing ./R ./inst ./data or",
        "./data-raw subdirectories."
      ))
      {
        stop("exiting", call. = FALSE)
      }
    }
    .multilog_trace("Processing data")
    # read YAML
    ymlfile <- dir(
      path = pkg_dir, pattern = "^datapackager.yml$",
      full.names = TRUE
    )
    if (length(ymlfile) == 0) {
      .multilog_fatal(paste0("Yaml configuration file not found at ", pkg_dir))
      {
        stop("exiting", call. = FALSE)
      }
    }
    ymlconf <- read_yaml(ymlfile)
    # test that the structure of the yaml file is correct!
    if (!"configuration" %in% names(ymlconf)) {
      .multilog_fatal("YAML is missing 'configuration:' entry")
      {
        stop("exiting", call. = FALSE)
      }
    }
    if (!all(c("files", "objects") %in%
      purrr::map(ymlconf, names)[["configuration"]])) {
      .multilog_fatal("YAML is missing files: and objects: entries")
      {
        stop("exiting", call. = FALSE)
      }
    }
    .multilog_trace("Reading yaml configuration")
    # files that have enable: TRUE
    assert_that("configuration" %in% names(ymlconf))
    assert_that("files" %in% names(ymlconf[["configuration"]]))
    assert_that(!is.null(names(ymlconf[["configuration"]][["files"]])))

    r_files <- unique(names(
      Filter(
        x = ymlconf[["configuration"]][["files"]],
        f = function(x) x$enabled
      )
    ))
    if (length(r_files) == 0) {
      .multilog_fatal("No files enabled for processing!")
      {
        stop("error", call. = FALSE)
      }
    }
    objects_to_keep <- purrr::map(ymlconf, "objects")[["configuration"]]
    render_root <- .get_render_root(ymlconf)
    if (!.validate_render_root(render_root)) {
      .multilog_fatal(paste0(
        "Can't create, or render_root = ",
        render_root, " doesn't exist"
      ))
      stop("error", call. = FALSE)
    } else {
      render_root <- normalizePath(render_root, winslash = "/")
    }

    r_files <- file.path(raw_data_dir, r_files)
    if (all(!file.exists(r_files))) {
      .multilog_fatal(paste0("Can't find any R or Rmd files."))
      .multilog_fatal(paste0(
        "     Cant' find file: ",
        r_files[!file.exists(r_files)]
      ))
      stop("error", call. = FALSE)
    }
    .multilog_trace(paste0("Found ", r_files))
    old_data_digest <- .parse_data_digest(pkg_dir = pkg_dir)
    description_file <- normalizePath(file.path(pkg_dir, "DESCRIPTION"),
      winslash = "/"
    )
    pkg_description <- try(read.description(file = description_file),
      silent = TRUE
    )
    # The test for a valid DESCRIPTION here is no longer needed since
    # we use proj_set().

    # check that we have at least one file
    # This is caught elsewhere

    if (length(objects_to_keep) == 0) {
      .multilog_fatal("You must specify at least one data object.")
      {
        stop("exiting", call. = FALSE)
      }
    }
    # TODO Can we configure documentation in yaml?
    do_documentation <- FALSE
    # This flag indicates success
    can_write <- FALSE
    # environment for the data
    ENVS <- new.env(hash = TRUE, parent = .GlobalEnv)
    object_tally <- 0
    already_built <- NULL
    building <- NULL
    r_dir <- normalizePath(file.path(pkg_dir, "R" ), winslash = "/")
    r_dir_files <- list.files( r_dir )
    r_dir_files <- r_dir_files[ !grepl( pkg_description$Package, 
                                        r_dir_files ) ]
    for (i in seq_along(r_files)) {
      dataenv <- new.env(hash = TRUE, parent = .GlobalEnv)
      for( j in seq_along( r_dir_files ) ){
        curr_path <- normalizePath(file.path(pkg_dir, 
                                             "R",
                                             r_dir_files[j] ), 
                                   winslash = "/")
        source( curr_path, 
                local = dataenv )
      }
      # assign ENVS into dataenv.
      # provide functions in the package to read from it (if deps = TRUE)
      if (deps) {
        assign(x = "ENVS", value = ENVS, dataenv)
      }
      .multilog_trace(paste0(
        "Processing ", i, " of ",
        length(r_files), ": ", r_files[i],
        "\n"
      ))
      # config file goes in the root render the r and rmd files
      ## First we spin then render if it's an R file
      flag <- FALSE
      .isRfile <- function(f) {
        grepl("\\.r$", tolower(f))
      }
      if (flag <- .isRfile(r_files[i])) {
        knitr::spin(r_files[i],
          precious = TRUE,
          knit = FALSE
        )
        r_files[i] <- paste0(tools::file_path_sans_ext(r_files[i]), ".Rmd")
        assert_that(file.exists(r_files[i]),
          msg = paste0("File: ", r_files[i], " does not exist!")
        )
        lines <- readLines(r_files[i])
        # do we likely have a yaml header? If not, add one.
        if (lines[1] != "---") {
          lines <- c(
            "---",
            paste0("title: ", basename(r_files[i])),
            paste0("author: ", Sys.info()["user"]),
            paste0("date: ", Sys.Date()),
            "---",
            "",
            lines
          )
          con <- file(r_files[i])
          writeLines(lines, con = con, sep = "\n")
          close(con)
        }
      }
      rmarkdown::render(
        input = r_files[i], envir = dataenv,
        output_dir = logpath, clean = TRUE, knit_root_dir = render_root,
        quiet = TRUE
      )
      # The created objects
      object_names <- setdiff(ls(dataenv),
                              c("ENVS", already_built)) # ENVS is removed
      object_tally <- object_tally | objects_to_keep %in% object_names
      already_built <- unique(c(already_built,
                                objects_to_keep[objects_to_keep %in% object_names]))
      .multilog_trace(paste0(
        sum(objects_to_keep %in% object_names),
        " data set(s) created by ",
        basename(r_files[i])
      ))
      .done(paste0(
        sum(objects_to_keep %in% object_names),
        " data set(s) created by ",
        basename(r_files[i])
      ))
      if (sum(objects_to_keep %in% object_names) > 0) {
        .add_newlines_to_vector <- function(x) {
          x <- paste0(x, sep = "\n")
          x[length(x)] <- gsub("\n", "", x[length(x)])
          x
        }
        .bullet(
          .add_newlines_to_vector(
            objects_to_keep[which(objects_to_keep %in% object_names)]),
          crayon::red("\u2022")
        )
      }
      .bullet(
        paste0(
          "Built ",
          ifelse(
            sum(object_tally) == length(object_tally),
            " all datasets!",
            paste0(sum(object_tally), " of ",
                   length(object_tally), " data sets.")
          )
        ),
        ifelse(
          sum(object_tally) == length(object_tally),
          crayon::green("\u2618"),
          crayon::green("\u2605")
        )
      )
      if (sum(objects_to_keep %in% object_names) > 0) {
        for (o in objects_to_keep[objects_to_keep %in% object_names]) {
          assign(o, get(o, dataenv), ENVS)
          # write the object to render_root
          o_instance <- get(o,dataenv)
          saveRDS(o_instance, file = paste0(file.path(render_root,o),".rds"))
        }
      }
    }
    # currently environments for each file are independent.
    dataenv <- ENVS
    # Digest each object
    new_data_digest <- .digest_data_env(ls(ENVS), dataenv, pkg_description)
    .newsfile()
    if (!is.null(old_data_digest)) {
      string_check <- .check_dataversion_string(
        old_data_digest,
        new_data_digest
      )
      can_write <- FALSE
      stopifnot(!((!.compare_digests(
        old_data_digest,
        new_data_digest
      )) & string_check$isgreater))
      if (.compare_digests(
        old_data_digest,
        new_data_digest
      ) &
        string_check$isequal) {
        can_write <- TRUE
        .multilog_trace(paste0(
          "Processed data sets match ",
          "existing data sets at version ",
          new_data_digest[["DataVersion"]]
        ))
      } else if ((!.compare_digests(
        old_data_digest,
        new_data_digest
      )) &
        string_check$isequal) {
        updated_version <- .increment_data_version(
          pkg_description,
          new_data_digest
        )
        #TODO what objects have changed?
        changed_objects <- .qualify_changes(new_data_digest,old_data_digest)
        
        .update_news_md(updated_version$new_data_digest[["DataVersion"]],
          interact = getOption("DataPackageR_interact", interactive())
        )
        .update_news_changed_objects(changed_objects)
        pkg_description <- updated_version$pkg_description
        new_data_digest <- updated_version$new_data_digest
        can_write <- TRUE
        .multilog_trace(paste0(
          "Data has been updated and DataVersion ",
          "string incremented automatically to ",
          new_data_digest[["DataVersion"]]
        ))
      } else if (.compare_digests(
        old_data_digest,
        new_data_digest
      ) &
        string_check$isgreater) {
        # edge case that shouldn't happen
        # but we test for it in the test suite
        can_write <- TRUE
        .multilog_trace(paste0(
          "Data hasn't changed but the ",
          "DataVersion has been bumped."
        ))
      } else if (string_check$isless & .compare_digests(
        old_data_digest,
        new_data_digest
      )) {
        # edge case that shouldn't happen but
        # we test for it in the test suite.
        .multilog_trace(paste0(
          "New DataVersion is less than ",
          "old but data are unchanged"
        ))
        new_data_digest <- old_data_digest
        pkg_description[["DataVersion"]] <- new_data_digest[["DataVersion"]]
        can_write <- TRUE
      } else if (string_check$isless & !.compare_digests(
        old_data_digest,
        new_data_digest
      )) {
        updated_version <- .increment_data_version(
          pkg_description,
          new_data_digest
        )
        # TODO what objects have changed?
        changed_objects <- .qualify_changes(new_data_digest,old_data_digest)
        .update_news_md(updated_version$new_data_digest[["DataVersion"]],
          interact = getOption("DataPackageR_interact", interactive())
        )
        .update_news_changed_objects(changed_objects)
        
        pkg_description <- updated_version$pkg_description
        new_data_digest <- updated_version$new_data_digest
        can_write <- TRUE
      }
      if (can_write) {
        .save_data(new_data_digest,
          pkg_description,
          ls(dataenv),
          dataenv,
          old_data_digest = old_data_digest,
          pkg_path = pkg_dir
        )
        do_documentation <- TRUE
      }
    } else {
      .update_news_md(new_data_digest[["DataVersion"]],
        interact = getOption(
          "DataPackageR_interact",
          interactive()
        )
      )
      .save_data(new_data_digest,
        pkg_description,
        ls(dataenv),
        dataenv,
        old_data_digest = NULL,
        pkg_path = pkg_dir
      )
      do_documentation <- TRUE
    }
    if (do_documentation) {
      # Run .doc_autogen #needs to be run when we have a partial build..
      if (!file.exists(file.path(target, "documentation.R"))) {
        .doc_autogen(basename(pkg_dir),
          ds2kp = ls(dataenv),
          env = dataenv,
          path = target
        )
      }
      # parse documentation
      doc_parsed <- .doc_parse(file.path(target, "documentation.R"))
      .identify_missing_docs <- function(environment = NULL,
                                               description = NULL,
                                               docs = NULL) {
        setdiff(
          ls(environment),
          setdiff(
            names(docs),
            description[["Package"]]
          )
        )
      }
      # case where we add an object,
      # ensures we combine the documentation properly
      missing_doc_for_autodoc <- .identify_missing_docs(
        dataenv,
        pkg_description,
        doc_parsed
      )
      if (length(missing_doc_for_autodoc) != 0) {
        tmptarget <- tempdir()
        file.info("Writing missing docs.")
        .doc_autogen(basename(pkg_dir),
          ds2kp = missing_doc_for_autodoc,
          env = dataenv,
          path = tmptarget,
          name = "missing_doc.R"
        )
        missing_doc <- .doc_parse(file.path(tmptarget, "missing_doc.R"))
        doc_parsed <- .doc_merge(
          old = doc_parsed,
          new = missing_doc
        )
        file.info("Writing merged docs.")
        docfile <- file(
          file.path(
            target,
            paste0("documentation", ".R")
          ),
          open = "w"
        )
        for (i in seq_along(doc_parsed)) {
          writeLines(text = doc_parsed[[i]], con = docfile)
        }
      }
      # Partial build if enabled=FALSE for
      # any file We've disabled an object but don't
      # want to overwrite its documentation
      # or remove it The new approach just builds
      # all the docs independent of what's enabled.
      save_docs <- do.call(c, doc_parsed)
      docfile <- file(file.path(pkg_dir, "R",
        pattern = paste0(pkg_description$Package, ".R")
      ),
      open = "w"
      )
      for (i in seq_along(save_docs)) {
        writeLines(text = save_docs[[i]], con = docfile)
      }
      close(docfile)
      .multilog_trace(
        paste0(
          "Copied documentation to ",
          file.path(pkg_dir, "R", paste0(pkg_description$Package, ".R"))
        )
      )
      # TODO test that we have documented
      # everything successfully and that all files
      # have been parsed successfully
      can_write <- TRUE
    }
    eval(expr = expression(rm(list = ls())), envir = dataenv)
    # copy html files to vignettes
    .ppfiles_mkvignettes(dir = pkg_dir)
  }
  .multilog_trace("Done")
  return(can_write)
}


.ppfiles_mkvignettes <- function(dir = NULL) {
  cat("\n")
  if (proj_get() != dir) {
    usethis::proj_set(dir) #nocov
  }
  pkg <- desc::desc(dir)
  pkg$set_dep("knitr", "Suggests")
  pkg$set_dep("rmarkdown", "Suggests")
  pkg$set("VignetteBuilder" = "knitr")
  pkg$write()
  usethis::use_directory("vignettes")
  usethis::use_directory("inst/doc")
  # TODO maybe copy only the files that have both html and Rmd.
  rmdfiles_for_vignettes <-
    list.files(
      path = file.path(dir, "data-raw"),
      pattern = "Rmd$",
      full.names = TRUE,
      recursive = FALSE
    )
  htmlfiles_for_vignettes <-
    list.files(
      path = file.path(dir, "inst/extdata/Logfiles"),
      pattern = "html$",
      full.names = TRUE,
      recursive = FALSE
    )
  pdffiles_for_vignettes <- 
    list.files(
      path = file.path(dir, "inst/extdata/Logfiles"),
      pattern = "pdf$",
      full.names = TRUE,
      recursive = FALSE
    )
  purrr::map(
    htmlfiles_for_vignettes,
    function(x) {
      file.copy(x,
        file.path(
          dir,
          "inst/doc",
          basename(x)
        ),
        overwrite = TRUE
      )
    }
  )
  
  purrr::map(
    pdffiles_for_vignettes,
    function(x) {
      file.copy(x,
                file.path(
                  dir,
                  "inst/doc",
                  basename(x)
                ),
                overwrite = TRUE
      )
    }
  )
  utils::capture.output(purrr::map(
    rmdfiles_for_vignettes,
    function(x) {
      file.copy(x,
        file.path(
          dir,
          "vignettes",
          basename(x)
        ),
        overwrite = TRUE
      )
    }
  ))
  vignettes_to_process <- list.files(
    path = file.path(dir, "vignettes"),
    pattern = "Rmd$",
    full.names = TRUE,
    recursive = FALSE
  )
  write_me_out <- purrr::map(vignettes_to_process, function(x) {
    title <- "Default Vignette Title. Add yaml title: to your document"
    thisfile <- read_file(x)
    stripped_yaml <- gsub("---\\s*\n.*\n---\\s*\n", "", thisfile)
    frontmatter <- gsub("(---\\s*\n.*\n---\\s*\n).*", "\\1", thisfile)
    con <- textConnection(frontmatter)
    fm <- rmarkdown::yaml_front_matter(con)
    if (is.null(fm[["vignette"]])) {
      # add boilerplate vignette yaml
      if (!is.null(fm$title)) {
        title <- fm$title
      }
      fm$vignette <- paste0("%\\VignetteIndexEntry{", title, "}
                           %\\VignetteEngine{knitr::rmarkdown}
                           \\usepackage[utf8]{inputenc}")
    } else {
      # otherwise leave it as is.
    }
    tmp <- fm$vignette
    tmp <- gsub(
      "  $",
      "",
      paste0(
        "vignette: >\n  ",
        gsub(
          "\\}\\s*",
          "\\}\n  ",
          tmp
        )
      )
    )
    fm$vignette <- NULL
    write_me_out <- paste0(
      "---\n",
      paste0(yaml::as.yaml(fm), tmp),
      "---\n\n",
      stripped_yaml
    )
    write_me_out
  })
  names(write_me_out) <- vignettes_to_process
  for (i in vignettes_to_process) {
    writeLines(write_me_out[[i]], con = i)
    writeLines(write_me_out[[i]],
      con = file.path(
        dir,
        "inst/doc",
        basename(i)
      )
    )
  }
}

#' Get DataPackageR Project Root Path
#'
#' @details Returns the path to the data package project root, or
#' constructs a path to a file in the project root from the
#' file argument.
#' @return \code{character}
#' @param file \code{character} or \code{NULL} (default).
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' project_path( file = "DESCRIPTION" )
#' }
project_path <- function(file = NULL) {
  if (is.null(file)) {
    return(usethis::proj_get())
  } else {
    return(normalizePath(file.path(usethis::proj_get(), file), winslash = "/"))
  }
}


#' Get DataPackageR extdata path
#'
#' @details Returns the path to the data package extdata subdirectory, or
#' constructs a path to a file in the extdata subdirectory from the
#' file argument.
#' @return \code{character}
#' @param file \code{character} or \code{NULL} (default).
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' project_extdata_path(file = "mydata.csv")
#' }
project_extdata_path <- function(file = NULL) {
  if (is.null(file)) {
    return(file.path(usethis::proj_get(), "inst", "extdata"))
  } else {
    return(normalizePath(
      file.path(
        usethis::proj_get(),
        "inst", "extdata", file
      ),
      winslash = "/"
    ))
  }
}

#' Get DataPackageR data path
#'
#' @details Returns the path to the data package data subdirectory, or
#' constructs a path to a file in the data subdirectory from the
#' file argument.
#' @return \code{character}
#' @param file \code{character} or \code{NULL} (default).
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' project_data_path( file = "data.rda" )
#' }
project_data_path <- function(file = NULL) {
  if (is.null(file)) {
    return(file.path(usethis::proj_get(), "data"))
  } else {
    return(normalizePath(
      file.path(
        usethis::proj_get(),
        "data", file
      ),
      winslash = "/"
    ))
  }
}

#' @name document
#' @rdname document
#' @title Build documentation for a data package using DataPackageR.
#' @param path \code{character} the path to the data package source root.
#' @param install \code{logical} install and reload the package. (default TRUE)
#' @param ... additional arguments to \code{install}
#' @export
#' @examples
#' # A simple Rmd file that creates one data object
#' # named "tbl".
#' if(rmarkdown::pandoc_available()){
#' f <- tempdir()
#' f <- file.path(f,"foo.Rmd")
#' con <- file(f)
#' writeLines("```{r}\n tbl = table(sample(1:10,100,replace=TRUE)) \n```\n",con=con)
#' close(con)
#'
#' # construct a data package skeleton named "MyDataPackage" and pass
#' # in the Rmd file name with full path, and the name of the object(s) it
#' # creates.
#'
#' pname <- basename(tempfile())
#' datapackage_skeleton(name=pname,
#'    path=tempdir(),
#'    force = TRUE,
#'    r_object_names = "tbl",
#'    code_files = f)
#'
#' # call package_build to run the "foo.Rmd" processing and
#' # build a data package.
#' package_build(file.path(tempdir(), pname), install = FALSE)
#' document(path = file.path(tempdir(), pname), install=FALSE)
#' }
document <- function(path = ".", install = TRUE, ...) {
  cat("\n")
  usethis::proj_set(path = path)
  path <- usethis::proj_get()
  assert_that(file.exists(file.path(path, "data-raw", "documentation.R")))
  desc <- desc::desc(file.path(path, "DESCRIPTION"))
  docfile <- paste0(desc$get("Package"), ".R")
  file.copy(
    from = file.path(path, "data-raw", "documentation.R"),
    to = file.path(path, "R", docfile),
    overwrite = TRUE
  )
  .multilog_trace("Rebuilding data package documentation.")
  devtools::document(pkg = path)
  location <- devtools::build(
    pkg = path, path = dirname(path),
    vignettes = FALSE, quiet = TRUE
  )
  # try to install and then reload the package in the current session
  if (install) {
    devtools::unload(basename(path))
    install.packages(location, repos = NULL, type = "source", ...)
  }
  return(TRUE)
}
