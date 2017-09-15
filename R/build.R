
#' Preprocess, document and build a data package
#'
#' Combines the preprocessing, documentation, and build steps into one.
#'
#' @param packageName \code{character} path to package source directory. Defaults to the current path when NULL.
#' @param vignettes \code{logical} specify whether to build vignettes. Default FALSE.
#' @param masterfile \code{characer} path to file in data-raw that sources processing scripts. Will do 
#' a partial build of the package.
#' @importFrom roxygen2 roxygenise roxygenize
#' @importFrom devtools build_vignettes build
#' @export
buildDataSetPackage <- function(packageName = NULL,vignettes=FALSE,masterfile=NULL) {
  if (is.null(packageName)) {
    packageName = "."
    # does the current directory hold a description file?
    success = try(read_pkg_description(packageName),silent=TRUE)
    if(inherits(success,"try-error")){
      stop("Can't find package DESCRIPTION in ",packageName)
    }
  }
  success <- DataPackageR(arg = packageName, masterfile = masterfile)
  if (!success) {
    stop("Preprocessing failed. Address the issues above and try again.")
  }
  # if(is.null(masterfile)){
  # message("Removing old documentation.")
  # manfiles = list.files(path=packageName,pattern = "\\.Rd$",ignore.case = FALSE,full.names=TRUE,recursive=TRUE)
  # sapply(manfiles,file.remove)
  message("Building documentation")
  roxygenise(packageName,clean=ifelse(is.null(masterfile),TRUE,FALSE))
  # }
  if(vignettes){
    build_vignettes(packageName)#build vignettes explicitly, ensures they are installed properly
  }
  message("Building package")
  build(packageName,vignettes = vignettes)
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

