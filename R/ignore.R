#' Ignore specific files by git and R build.
#'
#' @param file \code{character} File to ignore.
#' @param path \code{character} Path to the file.
#'
#' @return invisibly returns 0.
#' @export
#'
#' @examples
#' datapackage_skeleton(name="test",path = tempdir())
#' use_ignore("foo", ".")
use_ignore <- function(file = NULL, path = NULL){
  if (is.null(file)) {
    message("No file name provided to ignore.")
    return(invisible(0))
  }
  usethis::use_build_ignore(files = file.path(path,basename(file)), escape = TRUE)
  usethis::use_git_ignore(ignores = basename(file), directory = path)
  invisible(0)
}