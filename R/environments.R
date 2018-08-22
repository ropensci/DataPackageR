
#' Read an object created in a previously run processing script.
#'
#' @param name \code{character} the name of the object. Must be a
#' name available in the configuration objects. Other objects are not saved.
#' @details This function is only accessible within an R or Rmd file processed by DataPackageR.
#' It searches for an environment named \code{ENVS} within the current environment,
#' that holds the object with the given \code{name}. Such an environment is constructed and populated
#' with objects specified in the yaml \code{objects} property and passed along
#' to subsequent R and Rmd files as DataPackageR processes them in order.
#' @return An R object.
#' @export
#' @examples
#' if(rmarkdown::pandoc_available()){
#' ENVS <- new.env() # ENVS would be in the environment
#'                  # where the data processing is run. It is
#'                  # handled automatically by the package.
#' assign("find_me", 100, ENVS) #This is done automatically by DataPackageR
#'
#' datapackager_object_read("find_me") # This would appear in an Rmd processed by
#'                                     # DataPackageR to access the object named "find_me" created
#'                                     # by a previous script. "find_me" would also need to
#'                                     # appear in the objects property of config.yml
#' }
datapackager_object_read <- function(name) {
  get(name, get("ENVS", parent.frame()))
}
