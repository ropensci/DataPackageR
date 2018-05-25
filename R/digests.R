.save_digest <- function(data_digest) {
  write.dcf(data_digest,"DATADIGEST")
}

.check_dataversion_string <-
  function(old_data_digest,new_data_digest) {
    oldwarn <- options("warn")$warn
    options("warn" = -1)
    oldv <- strsplit(old_data_digest$DataVersion,"\\.")
    newv <- strsplit(new_data_digest$DataVersion,"\\.")
    oldv <- sapply(oldv,as.numeric)
    newv <- sapply(newv,as.numeric)
    if (any(is.na(oldv)) | any(is.na(newv))) {
      options("warn" = oldwarn)
      flog.fatal(paste0(
        "Invalid DataVersion string found ", old_data_digest$DataVersion," and ",new_data_digest$DataVersion
      ))
      stop("exiting",call.=FALSE)
    }
    greater <- apply(t(cbind(oldv,newv)),2,function(x)
      x[2] > x[1])
    equal <- apply(t(cbind(oldv,newv)),2,function(x)
      x[2] == x[1])
    less <- apply(t(cbind(oldv,newv)),2,function(x)
      x[2] < x[1])
    list(isgreater = ((greater[1]) |
                        (equal[1] &
                           greater[2]) | (equal[1] & equal[2] & greater[3])),isequal = all(equal))
  }

.compare_digests <- function(old_digest,new_digest,delta=NULL) {
  if (is.null(delta)) {
    valid <- ifelse(length(old_digest) != length(new_digest), FALSE, TRUE)
    n_old <- names(old_digest[-1L])
    n_new <- names(new_digest[-1L])
    valid <-
      ifelse(length(n_old) == length(n_new) &
               length(union(n_old, n_new)) == length(n_new),
             TRUE,
             FALSE)
    changed <- NULL
    flag <- FALSE
    if (valid) {
      for (i in names(new_digest)[-1L]) {
        if (new_digest[[i]] != old_digest[[i]]) {
          changed <- c(changed, i)
          valid <- FALSE
        }
      }
    }
    if (!valid) {
      warning("The following data sets have changed: ", changed)
    }
  }else{
    difference = setdiff(names(new_digest),names(old_digest))
    intersection = intersect(names(new_digest),names(old_digest))
    #No existing or new objects are changed
    if (length(difference) == 0) {
      valid = TRUE
    } else{
      #some new elements exist
      valid = FALSE
    }
    for(i in intersection){
      if(new_digest[[i]] != old_digest[[i]]){
        # some new elements are not the same
        valid = FALSE
      }
    }
  }
  return(valid)
}


.combine_digests = function(new,old){
  intersection = intersect(names(new),names(old))
  difference = setdiff(names(new),names(old))
  rdifference = setdiff(names(old),names(new))
  combined = c(new[intersection],old[rdifference],new[difference])
  combined[["DataVersion"]] = new[["DataVersion"]]
  return(combined)
}

.parse_data_digest <- function() {
  digest <- NULL
  if (file.exists("DATADIGEST")) {
    ret <- read.dcf("DATADIGEST")
    digest <- as.list(as.character(ret))
    names(digest) <- colnames(ret)
  }
  return(digest)
}

.digest_data_env <- function(object_names, dataEnv,pkg_description) {
  if (is.null(pkg_description[["DataVersion"]])) {
    flog.fatal("DESCRIPTION file must have a DataVersion line. i.e. DataVersion: 0.2.0")
    stop("exiting",call.=FALSE)
  }
  new_data_digest <- list()
  new_data_digest$DataVersion <- pkg_description$DataVersion
  data_objects <-
    lapply(object_names,function(obj)
      digest::digest(dataEnv[[obj]]))
  names(data_objects) <- object_names
  new_data_digest <- c(new_data_digest,data_objects)
  return(new_data_digest)
}


# function .rc() prepends "#'" to a string or character vector
.rc <- function(strVec){paste("#'", strVec)}
