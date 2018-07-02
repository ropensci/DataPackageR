

#' Preprocess, document and build a data package
#'
#' Combines the preprocessing, documentation, and build steps into one.
#'
#' @param packageName \code{character} path to package source directory. Defaults to the current path when NULL.
#' @param vignettes \code{logical} specify whether to build vignettes. Default FALSE.
#' @param log log level \code{INFO,WARN,DEBUG,FATAL}
#' @importFrom roxygen2 roxygenise roxygenize
#' @importFrom devtools build_vignettes build parse_deps
#' @importFrom usethis use_build_ignore use_rstudio proj_set use_directory
#' @importFrom rprojroot is_r_package
#' @importFrom yaml read_yaml
#' @importFrom futile.logger flog.debug flog.info flog.warn flog.error flog.fatal flog.appender flog.threshold INFO appender.console appender.tee
#' @importFrom knitr knit
#' @export
package_build <- function(packageName = NULL,
                          vignettes = FALSE,
                          log=INFO) {
  flog.threshold(log)
  flog.appender(appender.console())
  requireNamespace("rprojroot")
  requireNamespace("futile.logger")
  if (is.null(packageName)) {
    packageName <- "."
    # use normalizePath
    package_path <- normalizePath(packageName, winslash = "/")
    packageName <- basename(package_path)
    # Is this a package root?
    if (!is_r_package$find_file() == package_path) {
      flog.fatal(paste0(package_path, " is not an R package root directory"))
      stop("exiting", call. = FALSE)
    }
  } else {
    package_path <- normalizePath(packageName, winslash = "/")
    if (!file.exists(package_path)) {
      flog.fatal(paste0("Non existent package ", packageName))
      stop("exiting", call. = FALSE)
    }
    packageName <- basename(package_path)
  }
  # This should always be a proper name of a directory, either current or a
  # subdirectory
  if (inherits(
    try(is_r_package$find_file(path = package_path))
    , "try-error"
  )) {
    flog.fatal(paste0(
      package_path,
      " is not a valid R package directory beneath ",
      getwd()
    ))
    stop("exiting", call. = FALSE)
  }

  # Return success if we've processed everything
  success <-
    DataPackageR(arg = package_path)
  ifelse(success,
    flog.info("DataPackageR succeeded"),
    flog.warn("DataPackageR failed")
  )
  flog.info("Building documentation")
  roxygenise(package_path,
    clean = TRUE
  )

  flog.info("Building package")
  build(package_path,
    path = dirname(package_path),
    vignettes = vignettes
  )
}

#' These functions are no longer available.
#'
#' @name keepDataObjects-defunct
#' @aliases  keepDataObjects
#' @param ... arguments
#' @rdname keepDataObjects-defunct
#' @export
keepDataObjects <- function(...) {
  .Defunct(msg = "keepDataObjects is defunct as of version 0.12.1 of DataPackageR. \nUse the config.yml file to control packaging.")

}
