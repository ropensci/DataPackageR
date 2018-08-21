#' Add a raw data set to data-raw
#'
#' The file or directory specified by \code{path} will be moved into
#' the data-raw directory.
#'
#' @param path \code{character} path to file or directory.
#'
#' @return invisibly returns TRUE for success. Stops on failure.
#' @importFrom usethis proj_get proj_set create_package use_data_raw
#' @importFrom utils file_test
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' myfile <- tempfile()
#' file <- system.file("extdata", "tests", "extra.rmd",
#'                      package = "DataPackageR")
#' raw_data <- system.file("extdata", "tests", "raw_data",
#'                         package = "DataPackageR")
#' datapackage_skeleton(
#'   name = "datatest",
#'   path = tempdir(),
#'   code_files = file,
#'   force = TRUE,
#'   r_object_names = "data")
#' use_raw_dataset(raw_data)
#' }
use_raw_dataset <- function(path = NULL) {
  if (is.null(path)) {
    stop("You must provide a full path to a file or directory.")
  }
  proj_path <- usethis::proj_get()
  if (!utils::file_test("-d", file.path(proj_path, "inst", "extdata"))) {
    stop(paste0("inst/extdata doesn't exist in ", proj_path), call. = FALSE)
  }
  raw_file <- normalizePath(path)
  if (utils::file_test("-f", raw_file)) {
    file.copy(
      from = raw_file,
      to = file.path(proj_path, "inst", "extdata"),
      overwrite = TRUE
    )
    return(invisible(TRUE))
  } else if (utils::file_test("-d", raw_file)) {
    file.copy(
      from = raw_file,
      to = file.path(proj_path, "inst", "extdata"),
      recursive = TRUE, overwrite = TRUE
    )
    return(invisible(TRUE))
  } else {
    stop("path must be a path to an existing file or directory.")
  }
}
