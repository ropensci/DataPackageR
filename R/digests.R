.save_digest <- function(data_digest, path = NULL) {
  write.dcf(data_digest, file.path(path, "DATADIGEST"))
}

.check_dataversion_string <- function(old_data_digest, new_data_digest) {
  oldwarn <- options("warn")$warn
  options(warn = -1)
  oldv <- strsplit(old_data_digest[["DataVersion"]], "\\.")
  newv <- strsplit(new_data_digest[["DataVersion"]], "\\.")
  oldv <- lapply(oldv, as.numeric)[[1]]
  newv <- lapply(newv, as.numeric)[[1]]
  if (any(is.na(oldv)) | any(is.na(newv))) {
    options(warn = oldwarn)
    flog.fatal(paste0(
      "Invalid DataVersion string found ",
      old_data_digest[["DataVersion"]],
      " and ", new_data_digest[["DataVersion"]]
    ))
    # stop("exiting", call. = FALSE)
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
  valid <- ifelse(length(old_digest) != length(new_digest), FALSE, TRUE)
  if (valid) {
    for (i in names(new_digest)[-1L]) {
      if (new_digest[[i]] != old_digest[[i]]) {
        flog.warn(paste0(i, " has changed."))
        valid <- FALSE
      }
    }
    if (!valid) {
      warning()
    }
  } else {
    difference <- setdiff(names(new_digest), names(old_digest))
    intersection <- intersect(names(new_digest), names(old_digest))
    # No existing or new objects are changed
    if (length(difference) == 0) {
      valid <- TRUE
    } else {
      # some new elements exist
      valid <- FALSE
      for (i in difference) {
        flog.debug(paste0(i, " added."))
      }
    }
    for (i in intersection) {
      if (new_digest[[i]] != old_digest[[i]]) {
        flog.debug(paste0(i, " changed"))
        # some new elements are not the same
        valid <- FALSE
      }
    }
  }
  return(valid)
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

.digest_data_env <- function(object_names, dataenv, pkg_description) {
  if (is.null(pkg_description[["DataVersion"]])) {
    flog.fatal(paste0(
      "DESCRIPTION file must have a DataVersion",
      " line. i.e. DataVersion: 0.2.0"
    ))
    # stop("exiting", call. = FALSE)
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
