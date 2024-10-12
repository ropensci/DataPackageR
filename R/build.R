

#' Pre-process, document and build a data package
#'
#' Combines the preprocessing, documentation, and build steps into one.
#'
#' @param packageName \code{character} path to package source directory. Defaults to the current path when NULL.
#' @param vignettes \code{logical} specify whether to build vignettes. Default FALSE.
#' @param log log level \code{INFO,WARN,DEBUG,FATAL}
#' @param deps \code{logical} should we pass data objects into subsequent scripts? Default TRUE
#' @param install \code{logical} automatically install and load the package after building. Default FALSE
#' @param ... additional arguments passed to \code{install.packages} when \code{install=TRUE}.
#' @returns Character vector. File path of the built package.
#' @importFrom usethis use_build_ignore use_rstudio proj_set use_directory
#' @importFrom rprojroot is_r_package
#' @importFrom rmarkdown pandoc_available
#' @importFrom yaml read_yaml
#' @importFrom futile.logger flog.logger flog.trace appender.file flog.debug flog.info flog.warn flog.error flog.fatal flog.appender flog.threshold INFO TRACE appender.console appender.tee
#' @importFrom knitr knit spin
#' @details Note that if \code{package_build} returns an error when rendering an \code{.Rmd}
#' internally, but that same \code{.Rmd} can be run successfully manually using \code{rmarkdown::render},
#' then the following code facilitates debugging. Set \code{options(error = function(){ sink(); recover()})}
#' before running \code{package_build} . This will enable examination of the active function calls at the time of the error,
#' with output printed to the console rather than \code{knitr}'s default sink.
#' After debugging, evaluate \code{options(error = NULL)} to revert to default error handling.
#' See section "22.5.3 RMarkdown" at \url{ https://adv-r.hadley.nz/debugging.html} for more details.
#' @export
#' @examples
#' if(rmarkdown::pandoc_available()){
#' f <- tempdir()
#' f <- file.path(f,"foo.Rmd")
#' con <- file(f)
#' writeLines("```{r}\n tbl = data.frame(1:10) \n```\n",con=con)
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
  tryCatch({is_r_package$find_file(path = package_path)},
           error = function(cond){
             flog.fatal(paste0(
               package_path,
               " is not a valid R package directory beneath ",
               getwd()
             ), name = "console")
             stop("exiting", call. = FALSE)
           }
  )

  # Check that directory name matches package name
  validate_pkg_name(package_path)

  # Return success if we've processed everything
  success <-
    DataPackageR(arg = package_path, deps = deps)
  if (! success) .multilog_warn("DataPackageR failed")
  local({
    on.exit({
      if (packageName %in% names(utils::sessionInfo()$otherPkgs)){
        pkgload::unload(packageName)
      }
    })
    roxygen2::roxygenize(package_path, clean = TRUE)
  })
  location <- pkgbuild::build(path = package_path,
    dest_path = dirname(package_path),
    vignettes = vignettes,
    quiet = ! getOption('DataPackageR_verbose', TRUE)
  )
  # try to install and then reload the package in the current session
  if (install) {
    utils::install.packages(location, repos = NULL, type = "source", ...)
  }
  .next_steps()
  return(location)
}

.next_steps <- function() {
  if (! getOption('DataPackageR_verbose', TRUE)) return(invisible(NULL))
  cat(cli::col_green(cli::style_bold("Next Steps")), "\n") # nolint
  cat(cli::col_white(cli::col_yellow(cli::style_bold("1. Update your package documentation.")), "\n")) # nolint
  cat(cli::col_white("   - Edit the documentation.R file in the package source", cli::col_green("data-raw"), "subdirectory and update the roxygen markup."), "\n") # nolint
  cat(cli::col_white("   - Rebuild the package documentation with ", cli::col_red("document()"), "."), "\n") # nolint
  cat(cli::col_white(cli::col_yellow(cli::style_bold("2. Add your package to source control.")), "\n")) # nolint
  cat(cli::col_white("   - Call ", cli::col_red("git init ."), " in the package source root directory."), "\n") # nolint
  cat(cli::col_white("   - ", cli::col_red("git add"), " the package files."), "\n") # nolint
  cat(cli::col_white("   - ", cli::col_red("git commit"), " your new package."), "\n") # nolint
  cat(cli::col_white("   - Set up a github repository for your pacakge."), "\n") # nolint
  cat(cli::col_white("   - Add the github repository as a remote of your local package repository."), "\n") # nolint
  cat(cli::col_white("   - ", cli::col_red("git push"), " your local repository to gitub."), "\n") # nolint
}

#' Check that pkg name inferred from pkg path is same as pkg name in DESCRIPTION
#'
#' @param package_path Package path
#'
#' @returns Package name (character) if validated
#' @noRd
validate_pkg_name <- function(package_path){
  desc_pkg_name <- desc::desc(
    file = file.path(package_path, 'DESCRIPTION')
  )$get("Package")
  path_pkg_name <- basename(package_path)
  if (desc_pkg_name != path_pkg_name){
    err_msg <- paste("Data package name in DESCRIPTION does not match",
                     "name of the data package directory")
    flog.fatal(err_msg, name = "console")
    stop(err_msg, call. = FALSE)
  }
  desc_pkg_name
}
