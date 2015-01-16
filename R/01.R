.installScript<-function () 
{
  onWindows <- (.Platform$OS.type == "windows")
  files <- "preprocessData"
  if (onWindows) 
    files <- "preprocessData.bat"
  srcDir <- system.file("script", package = "preprocessData")
  srcFile <- file.path(srcDir, files)
  destDir <- file.path(Sys.getenv("R_HOME"), "bin")
  destFile <- file.path(destDir, files)
  alreadyExists <- all(file.exists(destFile))
  if ((!alreadyExists)) {
    res <- FALSE
    suppressWarnings(tryCatch({
      res <- all(file.copy(srcFile, destDir, overwrite = TRUE))
    }, error = function(e) res = -1))
    destFiles <- file.path(destDir, basename(srcFile))
    res <- all(file.exists(destFiles))
    if (interactive()) 
      func <- packageStartupMessage
    else func <- message
    if (is.null(res) || !res || res == -1) {
      script <- "preprocessData"
      if (onWindows) 
        script <- "preprocessData"
      msg <- strwrap(paste("Failed to copy the", paste0("script/", 
                                                        script), "script to", paste0(file.path(Sys.getenv("R_HOME"), 
                                                                                               "bin"), "."), "If you want to be able to run 'R CMD preprocessData' you'll", 
                           "need to copy it yourself to a directory on your PATH,", 
                           "making sure it is executable."))
      for (i in 1:length(msg)) func(msg[i])
    }
    else {
      func("preprocessData script installed.")
    }
  }
}

.isScriptInstalled<-function () 
{
  if (nchar(Sys.which("preprocessData"))) 
    return(TRUE)
  onWindows <- (.Platform$OS.type == "windows")
  if (onWindows) 
    file <- "preprocessData.bat"
  else file <- "preprocessData"
  path <- file.path(Sys.getenv("R_HOME"), "bin")
  all(file.exists(file.path(path, file)))
}

.onLoad <- function (libname, pkgname) 
{
  if (.isScriptInstalled()) 
    return()
  .installScript()
}

.parse_data_digest <- function(){
  digest<-NULL
  if(file.exists("DATADIGEST")){
    ret<-read.dcf("DATADIGEST")
    digest<-as.list(as.character(ret))
    names(digest)<-colnames(ret)
  }
  return(digest)
}

.digest_data_env <- function(object_names, dataEnv,pkg_description){
  if(is.null(pkg_description[["DataVersion"]])){
    stop("DESCRIPTION file must have a DataVersion line. i.e. DataVersion: 0.2.0")
  }
  new_data_digest<-list()
  new_data_digest$DataVersion<-pkg_description$DataVersion
  data_objects<-lapply(object_names,function(obj)digest::digest(dataEnv[[obj]]))
  names(data_objects)<-object_names
  new_data_digest<-c(new_data_digest,data_objects)
  return(new_data_digest)
}

.save_digest <- function(data_digest){
  write.dcf(data_digest,"DATADIGEST")
}

.check_dataversion_string <- function(old_data_digest,new_data_digest){
  oldwarn<-options("warn")$warn
  options("warn"=-1)
  oldv<-strsplit(old_data_digest$DataVersion,"\\.")
  newv<-strsplit(new_data_digest$DataVersion,"\\.")
  oldv<-sapply(oldv,as.numeric)
  newv<-sapply(newv,as.numeric)
  if(any(is.na(oldv))|any(is.na(newv))){
    options("warn"=oldwarn)
    stop("Invalid DataVersion string found ", old_data_digest$DataVersion," and ",new_data_digest$DataVersion)
  }
  greater<-apply(t(cbind(oldv,newv)),2,function(x)x[2]>x[1])
  equal<-apply(t(cbind(oldv,newv)),2,function(x)x[2]==x[1])
  less<-apply(t(cbind(oldv,newv)),2,function(x)x[2]<x[1])
  list(isgreater=((greater[1])|(equal[1]&greater[2])|(equal[1]&equal[2]&greater[3])),isequal=all(equal))
}

.compare_digests <- function(old_digest,new_digest){
  valid<-ifelse(length(old_digest)!=length(new_digest),FALSE,TRUE)
  n_old <- names(old_digest[-1L])
  n_new <- names(new_digest[-1L])
  valid<-ifelse(length(n_old)==length(n_new)&length(union(n_old,n_new))==length(n_new),TRUE,FALSE)
  changed<-NULL
  flag<-FALSE
  if(valid){
    for(i in names(new_digest)[-1L]){
      if(new_digest[[i]]!=old_digest[[i]]){
        changed<-c(changed,i)
        valid<-FALSE
      }
    }
  }
  if(!valid){
    warning("The following data sets have changed: ",changed)
  }
  return(valid)
}


.save_data <- function (new_data_digest, pkg_description, object_names, dataEnv) {
  .save_digest(new_data_digest)
  message("Saving to data")
  data_save_rda_path = file.path("data",paste0(pkg_description$Package,".rda"))
  save(list=object_names,file=data_save_rda_path,envir = dataEnv)
}