#' processData
#' 
#' Package to install the processData script that can be called via the R CMD mechanism.
#' R CMD processData packagename looks for .R files in "data-raw" within the "packagename" package source tree.
#' It sources all the .R files in "data-raw". The .R files are expected to read "raw data" from some source, like
#' "inst/extdata", or from an online url, process them in some way, such that they are tidy and standardized, and 
#' save the resulting data frames or data.tables in "data".  The user should also document the data sets either 
#' using "roxygen2"  by including an .R file under the "R" directory, or in user-written .Rd files under the "man" 
#' directory. After the data have been generated, the user can call R CMD build packagename.
#' @docType package
#' @name processData-package
NULL

#' Process data generation code in "data-raw" 
#' 
#' Assumes .R files in "data-raw" generate rda files to be stored in "data".
#' Sources each of these .R files. 
#' Meant to be called before R CMD build.
#' @name processData
#' @import optparse
processData <- function(){
  parser<-OptionParser(usage = "R CMD processData [options] package")
  arguments=parse_args(parser,positional_arguments=1)
  opt = arguments$options
  file <- arguments$args
  raw_data_dir <- "data-raw"
  target <- file.path(".",file,raw_data_dir)
  data_dir <- file.path(".",file,"data")
  if(!file.exists(target)){
    stop("Directory ",target," doesn't exist.")
  }else{
    if(!file.exists(data_dir)){
      dir.create(data_dir)
    }
    old<-getwd()
    tryCatch({
      message("Processing data")
      setwd(target)
      r_files <- dir(path = getwd(),pattern="*.R")
      for(i in seq_along(r_files)){
        cat(i," of ",length(r_files),": ",r_files[i],"\n")
        source(r_files[i])
      }
    },finally=setwd(old))
  }
  message("Done")
}

