.save_digest <- function(data_digest, path = NULL) {
  write.dcf(data_digest, file.path(path, "DATADIGEST"))
}

.check_dataversion_string <- function(old_data_digest, new_data_digest) {
  oldwarn <- options("warn")$warn
  suppressWarnings({
    oldv <- strsplit(old_data_digest[["DataVersion"]], "\\.")
    newv <- strsplit(new_data_digest[["DataVersion"]], "\\.")
    oldv <- lapply(oldv, as.numeric)[[1]]
    newv <- lapply(newv, as.numeric)[[1]]
  })
  if (any(is.na(oldv)) | any(is.na(newv))) {
    .multilog_fatal(paste0(
      "Invalid DataVersion string found ",
      old_data_digest[["DataVersion"]],
      " and ", new_data_digest[["DataVersion"]]
    ))
  }
  greater <- apply(t(cbind(oldv, newv)), 2, function(x) x[2] > x[1])
  equal <- apply(t(cbind(oldv, newv)), 2, function(x) x[2] == x[1])
  list(
    isgreater = ((greater[1]) | (equal[1] & greater[2]) |
      (equal[1] & equal[2] &
        greater[3])),
    isequal = all(equal),
    isless = !((greater[1]) | (equal[1] & greater[2]) |
      (equal[1] & equal[2] & greater[3])) & !all(equal)
  )
}

.compare_digests <- function(old_digest, new_digest) {
  # Returns FALSE when any exisiting data has is changed, new data is added, or data is removed, else return TRUE.
  # Use .mutlilog_warn when there is a change and multilog_debug when new data is added.

  existed <- names(new_digest)[names(new_digest) %in% names(old_digest)]
  added <- setdiff(names(new_digest), existed)
  removed <- names(old_digest)[!names(old_digest) %in% names(new_digest)]
  existed <- existed[existed != "DataVersion"]

  if(length(existed) > 0){
    changed <- names(new_digest)[ unlist(new_digest[existed]) != unlist(old_digest[existed]) ]
    if(length(changed) > 0){
      for(name in changed){
        .multilog_warn(paste(name, "has changed."))
      }
      warning()
      return(FALSE)
    }
  }

  for(name in removed){
    .multilog_debug(paste(name, "was removed."))
    return(FALSE)
  }

  for(name in added){
    .multilog_debug(paste(name, "was added."))
    return(FALSE)
  }

  return(TRUE)
};

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

.digest_data_env <- function(object_names, dataenv, pkg_description) {
  if (is.null(pkg_description[["DataVersion"]])) {
    .multilog_fatal(paste0(
      "DESCRIPTION file must have a DataVersion",
      " line. i.e. DataVersion: 0.2.0"
    ))
  }
  new_data_digest <- list()
  new_data_digest[["DataVersion"]] <- pkg_description[["DataVersion"]]
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
