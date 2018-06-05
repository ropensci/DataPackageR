
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
#' @import rprojroot
#' @importFrom yaml read_yaml
#' @import futile.logger
#' @export
buildDataSetPackage <- function(packageName = NULL, vignettes = FALSE, masterfile = NULL) {
    requireNamespace("rprojroot")
    requireNamespace("futile.logger")
    if (is.null(packageName)) {
        packageName <- "."
        # use normalizePath
        package_path <- normalizePath(packageName)
        packageName = basename(package_path)
        # Is this a package root?
        if (!is_r_package$find_file() == package_path) {
            flog.fatal(paste0(package_path, " is not an R package root directory"))
            # stop('exiting',call. = FALSE)
        }
        # Otherwise is_r_package matches the proposed directory and we know it has a
        # DESCRIPTION file so we're goot to go.  does the current directory hold a
        # description file?  success = try(read_pkg_description(packageName),silent=TRUE)
        # if(inherits(success,'try-error')){ stop('Can't find package DESCRIPTION in
        # ',packageName) }
    }
    package_path = normalizePath(packageName)
    if (!file.exists(package_path)) {
        flog.fatal(paste0("Non existent package ", packageName))
        stop("exiting", call = FALSE)
    }
    # This should always be a proper name of a directory, either current or a
    # subdirectory
    packageName = basename(package_path)
    if (inherits(try(is_r_package$find_file(path = package_path)), "try-error")) {
        flog.fatal(paste0(package_path, " is not a valid R package directory beneath ", 
            getwd()))
        # stop('exiting',call.=TRUE)
    }
    if (!is_r_package$find_file(path = package_path) == package_path) {
        flog.fatal(paste0(package_path, " is not an R package root directory"))
        # stop('exiting',call. = FALSE)
    }
    # Return success if we've processed everything
    success <- DataPackageR(arg = package_path, masterfile = masterfile)
    if (!success) {
        flog.fatal("Preprocessing failed. Something has gone wrong, see the errors above")
        # stop('exiting',call.=FALSE)
    }
    # if(is.null(masterfile)){ message('Removing old documentation.') manfiles =
    # list.files(path=packageName,pattern = '\\.Rd$',ignore.case =
    # FALSE,full.names=TRUE,recursive=TRUE) sapply(manfiles,file.remove)
    flog.info("Building documentation")
    roxygenise(package_path, clean = ifelse(is.null(masterfile), TRUE, FALSE))
    # }
    if (vignettes) {
        build_vignettes(package_path)  #build vignettes explicitly, ensures they are installed properly
    }
    flog.info("Building package")
    build(package_path, path = dirname(package_path), vignettes = vignettes)
}

#'Specify which data objects to keep
#'
#'Specify the names of the data objects to keep. To be called after all preprocessing code.
#'@param obj \code{character} vector of object names
#'@export
keepDataObjects <- function(obj) {
    # remove everything except the objects specified in obj
    rm(list = setdiff(objects(envir = parent.frame()), obj), envir = parent.frame())
}

