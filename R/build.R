

#' Pre-process, document and build a data package
#'
#' Combines the preprocessing, documentation, and build steps into one.
#'
#' @param packageName \code{character} path to package source directory. Defaults to the current path when NULL.
#' @param vignettes \code{logical} specify whether to build vignettes. Default FALSE.
#' @param log log level \code{INFO,WARN,DEBUG,FATAL}
#' @param deps \code{logical} should we pass data objects into subsequent scripts? Default TRUE
#' @param install \code{logical} automatically install and load the package after building. (default TRUE)
#' @param ... additional arguments passed to \code{install.packages} when \code{install=TRUE}.
#' @importFrom roxygen2 roxygenise roxygenize
#' @importFrom devtools build_vignettes build parse_deps reload
#' @importFrom usethis use_build_ignore use_rstudio proj_set use_directory
#' @importFrom rprojroot is_r_package
#' @importFrom rmarkdown pandoc_available
#' @importFrom utils install.packages
#' @importFrom yaml read_yaml
#' @importFrom futile.logger flog.logger flog.trace appender.file flog.debug flog.info flog.warn flog.error flog.fatal flog.appender flog.threshold INFO TRACE appender.console appender.tee
#' @importFrom knitr knit spin
#' @export
#' @examples
#' if(rmarkdown::pandoc_available()){
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
#' package_build(file.path(tempdir(),pname), install = FALSE)
#' }
package_build <- function(packageName = NULL,
                          vignettes = FALSE,
                          log = INFO,
                          deps = TRUE,
                          install = FALSE,
                          ...) {
  .multilog_setup(LOGFILE = NULL)
  # flog.appender(appender.console())
  # requireNamespace("futile.logger")
  if (is.null(packageName)) {
    packageName <- "."
    # use normalizePath
    package_path <- normalizePath(packageName, winslash = "/")
    packageName <- basename(package_path)
    # Is this a package root?
    if (!is_r_package$find_file() == package_path) {
      flog.fatal(paste0(package_path,
                        " is not an R package root directory"),
                 name = "console")
      stop("exiting", call. = FALSE)
    }
  } else {
    package_path <- normalizePath(packageName, winslash = "/")
    if (!file.exists(package_path)) {
      flog.fatal(paste0("Non existent package ", packageName), name = "console")
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
    ), name = "console")
    stop("exiting", call. = FALSE)
  }

  # Return success if we've processed everything
  success <-
    DataPackageR(arg = package_path, deps = deps)
  ifelse(success,
    .multilog_trace("DataPackageR succeeded"),
    .multilog_warn("DataPackageR failed")
  )
  .multilog_trace("Building documentation")
  roxygen2::roxygenise(package_path,
    clean = TRUE
  )

  .multilog_trace("Building package")
  location <- build(package_path,
    path = dirname(package_path),
    vignettes = vignettes
  )
  # try to install and then reload the package in the current session
  if (install) {
    devtools::unload(packageName)
    install.packages(location, repos = NULL, type = "source", ...)
  }
  .next_steps()
  return(location)
}

.next_steps <- function() {
  cat(crayon::green(crayon::bold("Next Steps")), "\n") # nolint
  cat(crayon::white(crayon::yellow(crayon::bold("1. Update your package documentation.")), "\n")) # nolint
  cat(crayon::white("   - Edit the documentation.R file in the package source", crayon::green("data-raw"), "subdirectory and update the roxygen markup."), "\n") # nolint
  cat(crayon::white("   - Rebuild the package documentation with ", crayon::red("document()"), "."), "\n") # nolint
  cat(crayon::white(crayon::yellow(crayon::bold("2. Add your package to source control.")), "\n")) # nolint
  cat(crayon::white("   - Call ", crayon::red("git init ."), " in the package source root directory."), "\n") # nolint
  cat(crayon::white("   - ", crayon::red("git add"), " the package files."), "\n") # nolint
  cat(crayon::white("   - ", crayon::red("git commit"), " your new package."), "\n") # nolint
  cat(crayon::white("   - Set up a github repository for your pacakge."), "\n") # nolint
  cat(crayon::white("   - Add the github repository as a remote of your local package repository."), "\n") # nolint
  cat(crayon::white("   - ", crayon::red("git push"), " your local repository to gitub."), "\n") # nolint
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
