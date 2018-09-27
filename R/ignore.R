#' Ignore specific files by git and R build.
#'
#' @param file \code{character} File to ignore.
#' @param path \code{character} Path to the file.
#'
#' @return invisibly returns 0.
#' @export
#'
#' @examples
#' use_ignore("foo", ".")
use_ignore <- function(file = NULL, path = NULL){
  if (is.null(file)) {
    message("No file name provided to ignore.")
    invisible(0)
  }
  proj_path <- usethis::proj_get()
  usethis::use_build_ignore(files = file.path(path,file), escape = TRUE)
  usethis::use_git_ignore(ignores = file, directory = path)
  invisible(0)
}