## Defunct functions in DataPackageR
#' @title Defunct functions in package \pkg{DataPackageR}.
#' @description These functions are defunct and no longer supported.
#' Calling them will result in an error.
#'
#' When possible, alternatives are suggested.
#'
#' @name DataPackageR-defunct
#' @param ... All arguments are now ignored.
#' @returns Defunct function. No return value.
NULL

#' @rdname DataPackageR-defunct
#' @export
keepDataObjects <- function(...) {
  .Defunct('datapackager.yml', package = 'DataPackageR')
}
