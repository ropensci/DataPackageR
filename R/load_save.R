#' @name load_all_datasets
#' @title Load all datasets in a package
#' @description loads all datasets in a package
#' @param packageName \code{character} the package name
#' @export
load_all_datasets = function(packageName) {
    datalist = try(data(package = packageName))$results[, "Item"]
    data(list = datalist, envir = environment())
}

.save_data <- function(new_data_digest, pkg_description, object_names, dataEnv, old_data_digest, 
    masterfile = NULL) {
    if (!is.null(masterfile)) {
        new_data_digest = .combine_digests(new_data_digest, old_data_digest)
    }
    .save_digest(new_data_digest)
    flog.info("Saving to data")
    # TODO get the names of each data object and save them separately. Provide a
    # function to load all.
    sapply(object_names, function(obj) {
        data_save_rda_path = file.path("data", paste0(obj, ".rda"))
        save(list = obj, file = data_save_rda_path, envir = dataEnv)
    })
    # Update description file
    to_update = desc(file = "DESCRIPTION")
    to_update$set("DataVersion", pkg_description$DataVersion)
    to_update$set("Date", format(Sys.time(),"%Y-%M-%d"))
    to_update$write()
}
