
.save_data <- function(new_data_digest,
                       DataVersion,
                       object_names,
                       dataenv,
                       old_data_digest = NULL,
                       masterfile = NULL,
                       pkg_path = NULL) {
  DataVersion <- validate_DataVersion(DataVersion)
  .save_digest(new_data_digest, path = pkg_path)
  .multilog_trace("Saving to data")
  # TODO get the names of each data object and save them separately. Provide a
  # function to load all.
  for (i in seq_along(object_names)) {
    obj <- object_names[i]
    data_save_rda_path <- file.path(pkg_path, "data", paste0(obj, ".rda"))
    save(list = obj, file = data_save_rda_path, envir = dataenv)
  }
  # Update description file
  to_update <- desc::desc(file = file.path(pkg_path, "DESCRIPTION"))
  to_update$set("DataVersion", DataVersion)
  to_update$set("Date", format(Sys.time(), "%Y-%m-%d"))
  to_update$write()
}
