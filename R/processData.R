#' preprocessData
#' 
#' Package to install the preprocessData script that can be called via the R CMD mechanism.
#' R CMD preprocessData packagename looks for .R files in "data-raw" within the "packagename" package source tree.
#' It sources a an .R file in "data-raw" named "datasets.R". The "datasets.R" file can source other files in "/data-raw".
#' This must be done via a call like: \code{sys.source("myRfile.R",env=topenv())}.
#' The "datasets.R" file and files it sources are expected to read raw data from 
#' "inst/extdata", or other sources, process them in some way, such that they are tidy and standardized.  The 
#' objects remaining in the environment after the code is run are presumed to be the data sets that will be written to
#' "/data". The user should also document these data sets  
#' using "roxygen2" in the .R files under the "data-raw" directory. The package will extract documentation for the data objects it finds, as well as for 
#' a data set with the name of the package, if present, and place it in the "/R" directory under the name "packagename.R". The "preprocessData" code will compare the digest of these data set objects against the contents of a "DATADIGEST" file 
#' in the package source tree (if present), and will also look for a "DataVersion: x.y.z" string in the DESCRIPTION file of the
#' package. If the data have changed, the user will be warned that the DataVersion needs to be incemented. If no DATADIGEST file exists, one will be created.
#' If the DataVersion string has been incremented and the digest matches DATADIGEST (if it exists), or if the data hasn't changed and the DataVersion string 
#' is unchanged, the code will write the data objects to "/data". The user can then build the package with
#' R CMD build packagename.
#' @docType package
#' @name preprocessData-package
NULL

#' Process data generation code in "data-raw" 
#' 
#' Assumes .R files in "data-raw" generate rda files to be stored in "data".
#' Sources datasets.R which can source other R files. 
#' R files sourced by datasets.R must invoke \code{sys.source("myRfile.R",env=topenv())}.
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
  target <- file.path(pkg_dir,raw_data_dir)
  data_dir <- file.path(pkg_dir,"data")
  if(!file.exists(target)){
    stop("Directory ",target," doesn't exist.")
  }else{
    if(!file.exists(data_dir)){
      dir.create(data_dir)
    }
    old<-getwd()
    tryCatch({
      # cd into the package directory
      setwd(pkg_dir)
      if(!file_test("R",op = "-d")){
        stop("You need a valid package data strucutre. Missing /R directory.")
      }
      message("Processing data")
      r_files <- dir(path = raw_data_dir,pattern="^datasets.R$",full=TRUE)
      old_data_digest<-.parse_data_digest()
      pkg_description<-try(roxygen2:::read.description("DESCRIPTION"),silent=TRUE)
      if(inherits(pkg_description,"try-error")){
        stop("You need a valid package DESCRIPTION file. Please see Writing R Extensions (http://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file).\n",pkg_description)
      }
      
      #FIXME ensure that there are no name conflicts across multiple files.
      #FIXME ensure that the DATADIGEST holds info for all files. 
      #FIXME currently only valid for a single R file..
      #environment for the data
      dataEnv<-new.env(hash=TRUE,parent = .GlobalEnv)
      if(length(r_files)!=1){
        stop("data-raw must contain a an .R named datasets.R. This file can source other .R files in the directory.")
      }
      do_documentation<-FALSE
      
      for(i in seq_along(r_files)){
        cat(i," of ",length(r_files),": ",r_files[i],"\n")
        #Source an R file
        sys.source(file=r_files[i],envir = dataEnv,keep.source=FALSE,chdir = TRUE)
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
          }else if((!.compare_digests(old_data_digest,new_data_digest))&string_check$isgreater){
            can_write<-TRUE
            message("Data has been updated and DataVersion string incremented to ",new_data_digest$DataVersion)
          }else if(.compare_digests(old_data_digest,new_data_digest)&string_check$isgreater){
            can_write<-FALSE
            message("Data hasn't changed but the DataVersion has been bumped. Stopping")
          }
          if(can_write){
            .save_data(new_data_digest,pkg_description,object_names,dataEnv)
            do_documentation<-TRUE
          }else{
            message("Some data has changed, but the DataVersion string has not been incremented or\nis less than the version in DATADIGEST \nUpdate the DataVersion string to be greater than ",old_data_digest$DataVersion," in the DESCRIPTION file\nand re-run R CMD preprocessData.")
          }
        }else{
          .save_data(new_data_digest,pkg_description,object_names,dataEnv)
          do_documenatation<-TRUE
        }
        if(do_documentation){
          #extract documentation and write to /R
          #FIXME this code is terrible and can be improved.. need to see how this is done in roxygen2
          all_r_files<-dir(raw_data_dir,pattern="\\.R$",full=TRUE)
          sources<-lapply(all_r_files,function(x)parse(x,keep.source=TRUE))
          docs<-lapply(sources,function(x)roxygen2:::comments(utils:::getSrcref(x)))
          docs<-lapply(docs,function(x)lapply(x,as.character))
          indx <- lapply(lapply(docs,function(x)lapply(x,function(y)sum(grepl("#'",y)))),function(x)unlist(x,use.names=FALSE)>0)
          docs<-lapply(1:length(docs),function(j)docs[[j]][indx[[j]]])
          #Extract @name
          doc_names<-lapply(docs,function(x)unlist(lapply(x,function(y)gsub(".+@name (\\D+)","\\1",{y[grepl("@name",y)]})),use.names=FALSE))
          save_docs<-NULL
          for(j in seq_along(doc_names)){
            #allow doc with @name of package to document the complete dataset
            save_docs<-c(save_docs,docs[[j]][doc_names[[j]]%in%c(object_names,pkg_description$Package)])
          }
          docfile<-file(file.path("R",paste0(pkg_description$Package,".R")),open = "w")
          sapply(save_docs,function(x){
            writeLines(text=x,con = docfile)
            writeLines("NULL",con=docfile)
            })
          close(docfile)
          message("Copied documentation to ",file.path("R",paste0(pkg_description$Package,".R")))
        }
        eval(expr=expression(rm(list=ls())),envir = dataEnv)
      }
    },finally=setwd(old))
  }
  message("Done")
}

