.installScript <- function ()
{
  onWindows <- (.Platform$OS.type == "windows")
  files <- "preprocessData"
  if (onWindows)
    files <- "preprocessData.bat"
  srcDir <- system.file("script", package = "preprocessData")
  srcFile <- file.path(srcDir, files)
  destDir <- file.path(Sys.getenv("R_HOME"), "bin")
  destFile <- file.path(destDir, files)
  alreadyExists <- all(file.exists(destFile))
  if ((!alreadyExists)) {
    res <- FALSE
    suppressWarnings(tryCatch({
      res <- all(file.copy(srcFile, destDir, overwrite = TRUE))
    }, error = function(e)
      res = -1))
    destFiles <- file.path(destDir, basename(srcFile))
    res <- all(file.exists(destFiles))
    if (interactive())
      func <- packageStartupMessage
    else
      func <- message
    if (is.null(res) || !res || res == -1) {
      script <- "preprocessData"
      if (onWindows)
        script <- "preprocessData"
      msg <- strwrap(
        paste(
          "Failed to copy the", paste0("script/",
                                       script), "script to", paste0(file.path(Sys.getenv("R_HOME"),
                                                                              "bin"), "."), "If you want to be able to run 'R CMD preprocessData' you'll",
          "need to copy it yourself to a directory on your PATH,",
          "making sure it is executable."
        )
      )
      for (i in 1:length(msg))
        func(msg[i])
    }
    else {
      func("preprocessData script installed.")
    }
  }
}

.isScriptInstalled <- function ()
{
  if (nchar(Sys.which("preprocessData")))
    return(TRUE)
  onWindows <- (.Platform$OS.type == "windows")
  if (onWindows)
    file <- "preprocessData.bat"
  else
    file <- "preprocessData"
  path <- file.path(Sys.getenv("R_HOME"), "bin")
  all(file.exists(file.path(path, file)))
}

.onLoad <- function (libname, pkgname)
{
  if (.isScriptInstalled())
    return()
  .installScript()
}

.parse_data_digest <- function() {
  digest <- NULL
  if (file.exists("DATADIGEST")) {
    ret <- read.dcf("DATADIGEST")
    digest <- as.list(as.character(ret))
    names(digest) <- colnames(ret)
  }
  return(digest)
}

.digest_data_env <- function(object_names, dataEnv,pkg_description) {
  if (is.null(pkg_description[["DataVersion"]])) {
    stop("DESCRIPTION file must have a DataVersion line. i.e. DataVersion: 0.2.0")
  }
  new_data_digest <- list()
  new_data_digest$DataVersion <- pkg_description$DataVersion
  data_objects <-
    lapply(object_names,function(obj)
      digest::digest(dataEnv[[obj]]))
  names(data_objects) <- object_names
  new_data_digest <- c(new_data_digest,data_objects)
  return(new_data_digest)
}

.save_digest <- function(data_digest) {
  write.dcf(data_digest,"DATADIGEST")
}

.check_dataversion_string <-
  function(old_data_digest,new_data_digest) {
    oldwarn <- options("warn")$warn
    options("warn" = -1)
    oldv <- strsplit(old_data_digest$DataVersion,"\\.")
    newv <- strsplit(new_data_digest$DataVersion,"\\.")
    oldv <- sapply(oldv,as.numeric)
    newv <- sapply(newv,as.numeric)
    if (any(is.na(oldv)) | any(is.na(newv))) {
      options("warn" = oldwarn)
      stop(
        "Invalid DataVersion string found ", old_data_digest$DataVersion," and ",new_data_digest$DataVersion
      )
    }
    greater <- apply(t(cbind(oldv,newv)),2,function(x)
      x[2] > x[1])
    equal <- apply(t(cbind(oldv,newv)),2,function(x)
      x[2] == x[1])
    less <- apply(t(cbind(oldv,newv)),2,function(x)
      x[2] < x[1])
    list(isgreater = ((greater[1]) |
                        (equal[1] &
                           greater[2]) | (equal[1] & equal[2] & greater[3])),isequal = all(equal))
  }

.compare_digests <- function(old_digest,new_digest) {
  valid <- ifelse(length(old_digest) != length(new_digest),FALSE,TRUE)
  n_old <- names(old_digest[-1L])
  n_new <- names(new_digest[-1L])
  valid <-
    ifelse(length(n_old) == length(n_new) &
             length(union(n_old,n_new)) == length(n_new),TRUE,FALSE)
  changed <- NULL
  flag <- FALSE
  if (valid) {
    for (i in names(new_digest)[-1L]) {
      if (new_digest[[i]] != old_digest[[i]]) {
        changed <- c(changed,i)
        valid <- FALSE
      }
    }
  }
  if (!valid) {
    warning("The following data sets have changed: ",changed)
  }
  return(valid)
}


.save_data <-
  function (new_data_digest, pkg_description, object_names, dataEnv) {
    .save_digest(new_data_digest)
    message("Saving to data")
    data_save_rda_path = file.path("data",paste0(pkg_description$Package,".rda"))
    save(list = object_names,file = data_save_rda_path,envir = dataEnv)
  }

#' Get the DataVersion for a package
#'
#' Retreives the DataVersion of a package if available
#' @param pkg \code{character} the package name
#' @param lib.loc \code{character} path to library location.
#' @seealso \link{utils:packageVersion}
#' @import utils
#' @export
dataVersion <- function (pkg, lib.loc = NULL)
{
  res <- suppressWarnings(packageDescription(pkg, lib.loc = lib.loc,
                                             fields = "DataVersion"))
  if (!is.na(res))
    package_version(res)
  else
    stop(gettextf("package %s not found or has no DataVersion string", sQuote(pkg)),
         domain = NA)
}

#' Create a Data Package skeleton for use with preprocessData.
#'
#' Creates a package skeleton for use with preprpocessData. Creates the additional information needed for versioning
#' datasets, namely the DataVersion string in DESCRIPTION, DATADIGEST, and the data-raw directory. Updates Read-and-delete-me
#' to reflect the additional necessary steps.
#' @name datapackage.skeleton
#' @param name \code{character} see \code{\link{utils:package.skeleton}}
#' @param list see \code{\link{utils:package.skeleton}}
#' @param environment see \code{\link{utils:package.skeleton}}
#' @param path see \code{\link{utils:package.skeleton}}
#' @param force see \code{\link{utils:package.skeleton}}
#' @param code_files see \code{\link{utils:package.skeleton}}
#' @export
#' @examples
#' data.package.skeleton(name="MyDataPackage",path="/tmp")
datapackage.skeleton <-
  function(name = "anRpackage", list = character(), environment = .GlobalEnv, path = ".", force = FALSE, code_files = character()) {
    if (length(list) == 0)
      package.skeleton(
        name = name, environment = environment, path = path,force = force,code_file =
          code_files
      )
    else
      package.skeleton(
        name = name, list = list,environment = environment, path = path,force =
          force,code_file = code_files
      )
    #create the rest of the necessary elements in the package
    package_path <- file.path(path,name)
    description <-
      roxygen2:::read.description(file = file.path(package_path,"DESCRIPTION"))
    description$DataVersion <- "0.1.0"
    message("Adding DataVersion string to DESCRIPTION")
    roxygen2:::write.description(description,file = file.path(package_path,"DESCRIPTION"))
    message("Creating data and data-raw directories")
    dir.create(
      file.path(package_path,"data-raw"),showWarnings = FALSE,recursive = TRUE
    )
    dir.create(file.path(package_path,"data"),showWarnings = FALSE,recursive = TRUE)
    dir.create(file.path(package_path,"R"),showWarnings = FALSE, recursive = TRUE)
    dir.create(
      file.path(package_path,"inst/extdata"),recursive = TRUE,showWarnings = FALSE
    )
    con <- file(file.path(package_path,"Read-and-delete-me"),open = "w")
    writeLines(
      c(
        "Edit the DESCRIPTION file to reflect the contents of your package.",
        "Optionally put your raw data under 'inst/extdata'.",
        "If the datasets are large, they may reside elsewhere outside the package.",
        "Copy .R files that do preprocessing of your data to 'data-raw'",
        "Edit 'data-raw/datasets.R' to source your R files.",
        "Document your data sets using roxygen markup (see examples in datasets.R)",
        "NOTES",
        "If your code relies on other packages, add those to the @import tag of the roxygen markup.",
        "The R object names you create must match the roxygen @name tags and \nmust be called out by keepDataObjects() in datasets.R"
      ),con
    )
    close(con)
    con <- file(file.path(package_path,"data-raw","datasets.R"))
    writeLines(
      c(
        "sys.source('myPreprocessingCode.R',envir=topenv())",
        "keepDataObjects('mydataset')",
        "",
        "#' MyDataPackage",
        "#' A data package for study XXXXX",
        "#' @docType package",
        "#' @aliases MyDataPackage-package",
        "#' @title MyDataPackage",
        "#' @name MyDataPackage",
        "#' @description a description of the package.",
        "#' @details Additional details.",
        "#' @import data.table",
        "#' @seealso \\link{mydataset}",
        "NULL",
        "",
        "#' Data from an assay, entitled mydataset",
        "#'@name mydataset",
        "#'@docType data",
        "#'@title Data from an assay.",
        "#'@format a \\code{data.table} containing the following fields",
        "#'\\describe{",
        "#'\\item{column_name}{description}",
        "#'\\item{column_name_2}{description}",
        "#'}",
        "#'@source Describe the source of the data (i.e. lab, etc)",
        "#'@seealso \\link{MyDataPackage}"
      ),con
    )
    close(con)
    oldrdfiles <-
      list.files(
        path = file.path(package_path,"man"),pattern = "Rd",full = TRUE
      )
    nmspc <-
      list.files(path = file.path(package_path),pattern = "NAMESPACE",full = TRUE)
    file.create(nmspc,showWarnings = FALSE) #create a blank NAMESPACE file
    oldrdafiles <-
      list.files(
        path = file.path(package_path,"data"),pattern = "rda",full = TRUE
      )
    oldrfiles <-
      list.files(
        path = file.path(package_path,"R"),pattern = "R",full = TRUE
      )
    file.remove(oldrdafiles)
    file.remove(oldrfiles)
    file.remove(oldrdfiles)
    invisible(NULL)
  }

#' Preprocess, document and build a data package
#'
#' Combines the preprocessing, documentation, and build steps into one.
#'
#' @param packageName \code{character} path to package source directory.
#' @export
buildDataSetPackage <- function(packageName = NULL,vignettes=FALSE) {
  if (is.null(packageName)) {
    packageName = "./"
    # does the current directory hold a description file?
    success = try(roxygen2:::read_pkg_description(packageName),silent=TRUE)
    if(inherits(success,"try-error")){
      stop("Can't find package DESCRIPTION in ",packageName)
    }
  }
  success <- preprocessData:::preprocessData(arg = packageName)
  if (!success) {
    stop("Preprocessing failed. Address the issues above and try again.")
  }
  message("Removing old documentation.")
  manfiles = list.files(path=packageName,pattern = "rd",ignore.case = TRUE,full=TRUE,recursive=TRUE)
  sapply(manfiles,file.remove)
  message("Building documentation")
  roxygen2:::roxygenise(packageName)
  if(vignettes){
    devtools::build_vignettes(packageName)#build vignettes explicitly, ensures they are installed properly
  }
  message("Building package")
  devtools::build(packageName,vignettes = vignettes)
}

#'Specify which data objects to keep
#'
#'Specify the names of the data objects to keep. To be called after all preprocessing code.
#'@param obj \code{character} vector of object names
#'@export
keepDataObjects <- function(obj) {
  #remove everything except the objects specified in obj
  rm(list = setdiff(objects(envir = parent.frame()),obj),envir = parent.frame())
}
