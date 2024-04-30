.save_digest <- function(data_digest, path = NULL) {
  write.dcf(data_digest, file.path(path, "DATADIGEST"))
}

#' Check dataversion string
#'
#' @param new_data_digest New data digest list with element named "DataVersion"
#'   containing a valid DataVersion
#' @param old_data_digest Old data digest list with element named "DataVersion"
#'   containing a valid DataVersion
#' @returns Character, ("lower", "equal", or "higher"), where new DataVersion is
#'   ____ relative to old DataVersion. version
#' @noRd
.check_dataversion_string <- function(new_data_digest, old_data_digest) {
  new <- validate_DataVersion(new_data_digest[["DataVersion"]])
  old <- validate_DataVersion(old_data_digest[["DataVersion"]])
  comp <- utils::compareVersion(new, old)
  txt <- c(lower = -1L, equal = 0L, higher = 1L)
  names(txt[which(txt == comp)])
}

.compare_digests <- function(old_digest, new_digest) {
  # Returns FALSE when any existing data has is changed, new data is added, or
  # data is removed, else return TRUE. Use .multilog_trace for all changes since
  # this is standard behavior during package re-build, and changes are already
  # output to the console by .qualify_changes()

  old_digest[['DataVersion']] <- NULL
  new_digest[['DataVersion']] <- NULL
  old_digest <- unlist(old_digest)
  new_digest <- unlist(new_digest)
  added <- setdiff(names(new_digest), names(old_digest))
  removed <- setdiff(names(old_digest), names(new_digest))
  common <- intersect(names(old_digest), names(new_digest))
  changed <- common[new_digest[common] != old_digest[common]]
  out <- TRUE
  for(name in changed){
    .multilog_trace(paste(name, "has changed."))
    out <- FALSE
  }

  for(name in removed){
    .multilog_trace(paste(name, "was removed."))
    out <- FALSE
  }

  for(name in added){
    .multilog_trace(paste(name, "was added."))
    out <- FALSE
  }

  return(out)
}

.combine_digests <- function(new, old) {
  intersection <- intersect(names(new), names(old))
  difference <- setdiff(names(new), names(old))
  rdifference <- setdiff(names(old), names(new))
  combined <- c(new[intersection], old[rdifference], new[difference])
  combined[["DataVersion"]] <- new[["DataVersion"]]
  return(combined)
}

.parse_data_digest <- function(pkg_dir = NULL) {
  digest <- NULL
  if (file.exists(file.path(pkg_dir, "DATADIGEST"))) {
    ret <- read.dcf(file.path(pkg_dir, "DATADIGEST"))
    digest <- as.list(as.character(ret))
    names(digest) <- colnames(ret)
  }
  return(digest)
}

.digest_data_env <- function(object_names, dataenv, DataVersion) {
  new_data_digest <- list()
  new_data_digest[["DataVersion"]] <- validate_DataVersion(DataVersion)
  data_objects <- lapply(object_names, function(obj) {
    digest::digest(dataenv[[obj]])
  })
  names(data_objects) <- object_names
  new_data_digest <- c(new_data_digest, data_objects)
  return(new_data_digest)
}


# function .rc() prepends '#'' to a string or character vector
.rc <- function(strvec) {
  paste("#'", strvec)
}
