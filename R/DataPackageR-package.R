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
#' Calling \code{package_build()} sets the build process in motion.
#' @examples
#' # A simple Rmd file that creates one data object
#' # named "tbl".
#' if(rmarkdown::pandoc_available()){
#' f <- tempdir()
#' f <- file.path(f,"foo.Rmd")
#' con <- file(f)
#' writeLines("```{r}\n tbl = data.frame(1:10) \n```\n",con=con)
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
#' pkgload::load_all(file.path(tempdir(), pname))
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
#' @name DataPackageR-package
#' @keywords internal
'_PACKAGE'

## usethis namespace: start
## usethis namespace: end
NULL

#' Options consulted by DataPackageR
#'
#' @description User-configurable options consulted by DataPackageR, which
#'   provide a mechanism for setting default behaviors for various functions.
#'
#'   If the built-in defaults don't suit you, set one or more of these options.
#'   Typically, this is done in the \code{.Rprofile} startup file, which you can open
#'   for editing with \code{usethis::edit_r_profile()} - this will set the specified
#'   options for all future R sessions. The following setting is recommended to
#'   not be prompted upon each package build for a NEWS update:
#'
#' \code{options(DataPackageR_interact = FALSE)}
#'
#' @section Options for the DataPackageR package:
#'
#' - \code{DataPackageR_interact}: Upon package load, this defaults to the value of
#'   \code{interactive()}, unless the option has been previously set (e.g., in
#'   \code{.Rprofile}). TRUE prompts user interactively for a NEWS update on
#'   \code{package_build()}. See the example above and the
#'   \href{https://ropensci.org/blog/2018/09/18/datapackager/}{rOpenSci blog
#'   post} for more details on how to set this to FALSE, which will never prompt
#'   user for a NEWS update. FALSE is also the setting used for DataPackageR
#'   internal package tests.
#'
#' - \code{DataPackageR_verbose}: Default upon package load is TRUE. FALSE suppresses
#'   all console output and is currently only used for automated
#'   unit tests of the DataPackageR package.
#'
#' - \code{DataPackageR_packagebuilding}: Default upon package load is FALSE. This
#'   option is used internally for package operations and changing it is not
#'   recommended.
#'
#' @name DataPackageR_options
NULL
