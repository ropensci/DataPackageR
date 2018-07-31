

#' Preprocess, document and build a data package
#'
#' Combines the preprocessing, documentation, and build steps into one.
#'
#' @param packageName \code{character} path to package source directory. Defaults to the current path when NULL.
#' @param vignettes \code{logical} specify whether to build vignettes. Default FALSE.
#' @param log log level \code{INFO,WARN,DEBUG,FATAL}
#' @param deps \code{logical} should we pass data objects into subsequent scripts? Default TRUE
#' @param install \code{logical} automatically install and load the package after building. (default TRUE)
#' @importFrom roxygen2 roxygenise roxygenize
#' @importFrom devtools build_vignettes build parse_deps reload
#' @importFrom usethis use_build_ignore use_rstudio proj_set use_directory
#' @importFrom rprojroot is_r_package
#' @importFrom utils install.packages
#' @importFrom yaml read_yaml
#' @importFrom futile.logger flog.debug flog.info flog.warn flog.error flog.fatal flog.appender flog.threshold INFO appender.console appender.tee
#' @importFrom knitr knit spin
#' @export
#' @examples
#'
#' f <- tempdir()
#' f <- file.path(f,"foo.Rmd")
#' con <- file(f)
#' writeLines("```{r}\n tbl = table(sample(1:10,1000,replace=TRUE)) \n```\n",con=con)
#' close(con)
#' pname <- basename(tempfile())
#' datapackage_skeleton(name=pname,
#'    path=tempdir(),
#'    force = TRUE,
#'    r_object_names = "tbl",
#'    code_files = f)
#'
#' package_build(file.path(tempdir(),pname))
package_build <- function(packageName = NULL,
                          vignettes = FALSE,
                          log = INFO,
                          deps = TRUE,
                          install = TRUE) {
  flog.threshold(log)
  flog.appender(appender.console())
  # requireNamespace("futile.logger")
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
    DataPackageR(arg = package_path, deps = deps)
  ifelse(success,
    flog.info("DataPackageR succeeded"),
    flog.warn("DataPackageR failed")
  )
  flog.info("Building documentation")
  roxygen2::roxygenise(package_path,
    clean = TRUE
  )

  flog.info("Building package")
  location <- build(package_path,
    path = dirname(package_path),
    vignettes = vignettes
  )
  # try to install and then reload the package in the current session
  if (install) {
    install.packages(location,repos = NULL, type = "source")
    devtools::reload(package_path)
  }
  return(location)
}

#' These functions are no longer available.
#'
#' @name keepDataObjects-defunct
#' @aliases  keepDataObjects
#' @param ... arguments
#' @rdname keepDataObjects-defunct
#' @export
keepDataObjects <- function(...) {
  .Defunct(msg = "keepDataObjects is defunct as of version 0.12.1 of DataPackageR. \nUse the config.yml file to control packaging.") # nolint
}
