#' preprocessData
#' 
#' Package to install the preprocessData script that can be called via the R CMD mechanism.
#' R CMD preprocessData packagename looks for .R files in "data-raw" within the "packagename" package source tree.
#' It sources all the .R files in "data-raw". The .R files are expected to read "raw data" from some source, like
#' "inst/extdata", or from an online url, process them in some way, such that they are tidy and standardized, and 
#' save the resulting data frames or data.tables in "data".  The user should also document the data sets either 
#' using "roxygen2"  by including an .R file under the "R" directory, or in user-written .Rd files under the "man" 
#' directory. After the data have been generated, the user can call R CMD build packagename.
#' @docType package
#' @name preprocessData-package
NULL

#' Process data generation code in "data-raw" 
#' 
#' Assumes .R files in "data-raw" generate rda files to be stored in "data".
#' Sources each of these .R files. 
#' Meant to be called before R CMD build.
#' @name preprocessData
#' @import optparse roxygen2
preprocessData <- function(arg=NULL){
  if(is.null(arg)){
    parser<-OptionParser(usage = "R CMD preprocessData [options] package")
    arguments=parse_args(parser,positional_arguments=1)
    opt = arguments$options
    pkg_dir <- arguments$args
  }else{
    pkg_dir<-arg
  }
  raw_data_dir <- "data-raw"
  target <- file.path(".",pkg_dir,raw_data_dir)
  data_dir <- file.path(".",pkg_dir,"data")
  if(!file.exists(target)){
    stop("Directory ",target," doesn't exist.")
  }else{
    if(!file.exists(data_dir)){
      dir.create(data_dir)
    }
    old<-getwd()
    tryCatch({
      message("Processing data")
      # cd into the package directory
      setwd(pkg_dir)
      r_files <- dir(path = raw_data_dir,pattern="*.R",full=TRUE)
      old_data_digest<-.parse_data_digest()
      pkg_description<-roxygen2:::read.description("DESCRIPTION")
      #FIXME ensure that there are no name conflicts across multiple files.
      #FIXME ensure that the DATADIGEST holds info for all files. 
      #FIXME currently only valid for a single R file..
      #environment for the data
      dataEnv<-new.env(hash=TRUE)
      if(length(r_files)>1){
        stop("data-raw must contain a single R file for all data sets. This will be corrected in the future.")
      }
      for(i in seq_along(r_files)){
        cat(i," of ",length(r_files),": ",r_files[i],"\n")
        #Source an R file
        sys.source(file=r_files[i],envir = dataEnv,keep.source=TRUE,chdir = TRUE)
        #The created objects
        object_names <- ls(dataEnv)
        message("Found ",length(object_names), " data objects in file ",basename(r_files[i]))
        #Digest each object 
        new_data_digest<-.digest_data_env(object_names,dataEnv,pkg_description)
        if(!is.null(old_data_digest)){
          #Compare digest of each object against existing digest if available
          string_check <- .check_dataversion_string(old_data_digest,new_data_digest)
          can_write<-FALSE
          if(.compare_digests(old_data_digest,new_data_digest)&string_check$isequal){
            can_write<-TRUE
            message("Processed data sets match existing data sets at version ",new_data_digest$DataVersion)
          }else if(!.compare_digests(old_data_digest,new_data_digest)&string_check$isgreater){
            can_write<-TRUE
            message("Data has been updated and DataVersion string incremented to ",new_data_digest$DataVersion)
          }
          if(can_write){
            .save_data(new_data_digest,pkg_description,object_names,dataEnv)
          }else{
            message("Some data has changed, but the DataVersion string has not been incremented or\nis less than the version in DATADIGEST \nUpdate the DataVersion string to be greater than ",old_data_digest$DataVersion," in the DESCRIPTION file\nand re-run R CMD preprocessData.")
          }
        }else{
          .save_data(new_data_digest,pkg_description,object_names,dataEnv)
        }        
        eval(expr=expression(rm(list=ls())),envir = dataEnv)
      }
    },finally=setwd(old))
  }
  message("Done")
}

