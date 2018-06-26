
#' Read an object created in a previously run processing script.
#'
#' @param name \code{character} the name of the object. Must be a
#' name available in the configuration objects. Other objects are not saved.
#'
#' @return An R object.
#' @export
#'
#' @examples
datapackager_object_read <- function(name) {
  get(name, get("ENVS", parent.frame()))
}
