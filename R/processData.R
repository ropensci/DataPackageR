#' DataPackageR
#'
#' Package to install the DataPackageR script that can be called via the R CMD mechanism.
#' R CMD DataPackageR packagename looks for .R files in "data-raw" within the "packagename" package source tree.
#' It sources a an .R file in "data-raw" named "datasets.R". The "datasets.R" file can source other files in "/data-raw".
#' This must be done via a call like: \code{sys.source("myRfile.R",env=topenv())}.
#' The "datasets.R" file and files it sources are expected to read raw data from
#' "inst/extdata", or other sources, process them in some way, such that they are tidy and standardized.  The
#' objects remaining in the environment after the code is run are presumed to be the data sets that will be written to
#' "/data". The user should also document these data sets
#' using "roxygen2" in the .R files under the "data-raw" directory. The package will extract documentation for the data objects it finds, as well as for
#' a data set with the name of the package, if present, and place it in the "/R" directory under the name "packagename.R". The "DataPackageR" code will compare the digest of these data set objects against the contents of a "DATADIGEST" file
#' in the package source tree (if present), and will also look for a "DataVersion: x.y.z" string in the DESCRIPTION file of the
#' package. If the data have changed, the user will be warned that the DataVersion needs to be incemented. If no DATADIGEST file exists, one will be created.
#' If the DataVersion string has been incremented and the digest matches DATADIGEST (if it exists), or if the data hasn't changed and the DataVersion string
#' is unchanged, the code will write the data objects to "/data". The user can then build the package with
#' R CMD build packagename.
#' @docType package
#' @name DataPackageR-package
NULL

#' Process data generation code in "data-raw"
#'
#' Assumes .R files in "data-raw" generate rda files to be stored in "data".
#' Sources datasets.R which can source other R files.
#' R files sourced by datasets.R must invoke \code{sys.source("myRfile.R",env=topenv())}.
#' Meant to be called before R CMD build.
#' @name DataPackageR
#' @param arg \code{character} name of the package to build.
#' @param masterfile \code{characer} path to file in data-raw that sources processing scripts. Will do 
#' a partial build of the package.
#' @return logical TRUE if succesful, FALSE, if not.
#' @import optparse roxygen2 rmarkdown desc
#' @importFrom utils getSrcref
#' @importFrom devtools as.package
#' @import devtools
DataPackageR <- function(arg = NULL,masterfile=NULL) {
  if (is.null(arg)) {
    parser <-
      OptionParser(usage = "R CMD DataPackageR [options] package")
    arguments = parse_args(parser,positional_arguments = 1)
    opt = arguments$options
    pkg_dir <- arguments$args
  }else{
    if (arg == "./") {
      stop(
        "You can't call DataPackageR with arg=\"./\" : Move out of the package directory and provide a proper package name."
      )
    }
    pkg_dir <- arg
  }
  raw_data_dir <- "data-raw"
  target <- file.path(pkg_dir,raw_data_dir)
  data_dir <- file.path(pkg_dir,"data")
  if (!file.exists(target)) {
    stop("Directory ",target," doesn't exist.")
  }else{
    if (!file.exists(data_dir)) {
      dir.create(data_dir)
    }
    old <- getwd()
    tryCatch({
      # cd into the package directory
      setwd(pkg_dir)
      if (!file_test("R",op = "-d")) {
        stop("You need a valid package data strucutre. Missing /R directory.")
      }
      message("Processing data")
      r_files <-
        dir(path = raw_data_dir,pattern = "^datasets.R$",full.names = TRUE)
      old_data_digest <- .parse_data_digest()
      pkg_description <-
        try(read.description(file="DESCRIPTION"),silent = TRUE)
      if (inherits(pkg_description,"try-error")) {
        stop(
          "You need a valid package DESCRIPTION file. Please see Writing R Extensions (http://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file).\n",pkg_description
        )
      }
      #environment for the data
      dataEnv <- new.env(hash = TRUE,parent = .GlobalEnv)
      if (length(r_files) != 1) {
        stop(
          "data-raw must contain a an .R named datasets.R. This file can source other .R files in the directory."
        )
      }
      if(!is.null(masterfile)){
        #process masterfile instead of datasets.R
        masterfile = list.files(path=".",pattern = masterfile,recursive = TRUE,full.names=TRUE)
        if(length(masterfile)==0){
          stop("Can't find ",masterfile)
        }
        r_files = masterfile
      }
      do_documentation <- FALSE
      can_write <- FALSE
      #log to the log file
      #Create a log directory in inst/extdata
      dir.create("inst/extdata/Logfiles/",recursive = TRUE,showWarnings = FALSE)
      #open a log files
      message("Logging to ",file.path("inst/extdata/Logfiles","processing.log"))
      LOGFILE <-
        file(file.path("inst/extdata/Logfiles","processing.log"))
      # sink(LOGFILE,append = TRUE,split = TRUE)
      for (i in seq_along(r_files)) {
        cat(i," of ",length(r_files),": ",r_files[i],"\n")
        #Source an R file
        sys.source(
          file = r_files[i],envir = dataEnv,keep.source = FALSE,chdir = TRUE
        )
        #The created objects
        object_names <- ls(dataEnv)
        message("Found ",length(object_names), " data objects in file ",basename(r_files[i]))
        #Digest each object
        new_data_digest <-
          .digest_data_env(object_names,dataEnv,pkg_description)
        if (!is.null(old_data_digest)) {
          #Compare digest of each object against existing digest if available
          #unless we are processing something from the masterfile.
          #Then we need to:
          # check what the new objects are and only compare the versions of those new objects
          
          string_check <-
            .check_dataversion_string(old_data_digest,new_data_digest)
          can_write <- FALSE
          if (.compare_digests(old_data_digest,new_data_digest,delta=masterfile) &
              string_check$isequal) {
                can_write <- TRUE
                message(
                  "Processed data sets match existing data sets at version ",new_data_digest$DataVersion
                )
              }else if ((!.compare_digests(old_data_digest,new_data_digest,delta=masterfile)) &
                        string_check$isequal) {
                        updated_version = .increment_data_version(pkg_description,new_data_digest);
                        pkg_description = updated_version$pkg_description
                        new_data_digest = updated_version$new_data_digest
                          can_write <- TRUE
                          message(
                            "Data has been updated and DataVersion string incremented automatically to ",new_data_digest$DataVersion
                          )
                        }else if (.compare_digests(old_data_digest,new_data_digest,delta=masterfile) &
                                  string_check$isgreater) {
                                    can_write <- TRUE
                                    message("Data hasn't changed but the DataVersion has been bumped.")
                        }else if ((!.compare_digests(old_data_digest,new_data_digest,delta=masterfile)) &
                                         string_check$isgreater) {
                          can_write <- TRUE
                          message("Data has changed and the DataVersion has been bumped.")
                        }
          
          if (can_write) {
            .save_data(new_data_digest,pkg_description,object_names,dataEnv,old_data_digest = old_data_digest, masterfile=masterfile)
            do_documentation <- TRUE
          }else{
            message(
              "Some data has changed, but the DataVersion string has not been incremented or\nis less than the version in DATADIGEST \nUpdate the DataVersion string to be greater than ",old_data_digest$DataVersion," in the DESCRIPTION file\nand re-run R CMD DataPackageR."
            )
          }
        }else{
          .save_data(new_data_digest,pkg_description,object_names,dataEnv)
          do_documenatation <- TRUE
        }
        if (do_documentation) {
          #extract documentation and write to /R
          #FIXME this code is terrible and can be improved.. need to see how this is done in roxygen2
          all_r_files <- dir(raw_data_dir,pattern = "\\.R$",full.names = TRUE)
          doc_parsed = .parseDocumentation(all_r_files)
          doc_parsed = Filter(f = function(x)!is.null(x),doc_parsed[c(pkg_description$Package,object_names)])
          #here need to append if we are doing a partial build.
          if(!is.null(masterfile)){
            old_doc_file = list.files(here("R"),full.names = TRUE,paste0(pkg_description$Package,".R"))
            if(file.exists(old_doc_file)){
             old_docs = .parseDocumentation(old_doc_file)
              merged_docs = .mergeDocumentation(old = old_docs, new = doc_parsed)
              save_docs = do.call(c,merged_docs)
            }
            else {
              save_docs = do.call(c,doc_parsed)
            }
          }else{
            save_docs = do.call(c,doc_parsed)
          }
          docfile <-
            file(file.path("R",pattern = paste0(pkg_description$Package,".R")),open = "w")
          sapply(save_docs,function(x) {
            writeLines(text = x,con = docfile)
          })
          close(docfile)
          message("Copied documentation to ",file.path("R",paste0(pkg_description$Package,".R")))
        }
        eval(expr = expression(rm(list = ls())),envir = dataEnv)
      }
      # copy html files to vignettes
      .vignettesFromPPFiles()
      
    },finally = {
      setwd(old);
    })
  }
  message("Done")
  if (can_write) {
    return(TRUE)
  }else{
    return(FALSE)
  }
}


.vignettesFromPPFiles <- function() {
  pkg <- as.package(".")
  check_suggested("rmarkdown")
  add_desc_package(pkg, "Suggests", "knitr")
  add_desc_package(pkg, "Suggests", "rmarkdown")
  add_desc_package(pkg, "VignetteBuilder", "knitr")
  use_directory("vignettes", pkg = pkg)
  use_git_ignore("inst/doc", pkg = pkg)
  message("Removing inst/doc from .gitignore")
  lines = readLines(".gitignore")
  lines = gsub("inst/doc","",lines)
  writeLines(lines,".gitignore")
  # browser()
  try(dir.create("inst/doc"),silent=TRUE)
  #TODO maybe copy only the files that have both html and Rmd.
  rmdfiles_for_vignettes = list.files(path="data-raw",pattern="Rmd$",full.names=TRUE,recursive = FALSE)
  htmlfiles_for_vignettes = list.files(path="inst/extdata/Logfiles",pattern="html$",full.names =TRUE,recursive = FALSE)
  purrr::map(htmlfiles_for_vignettes,function(x)file.copy(x,file.path("inst/doc",basename(x)),overwrite = TRUE))
  capture.output(purrr::map(rmdfiles_for_vignettes,function(x)file.copy(x,file.path("vignettes",basename(x)),overwrite = TRUE)))
  vignettes_to_process = list.files(path="vignettes",pattern="Rmd$",full.names =TRUE,recursive=FALSE)
  write_me_out = purrr::map(vignettes_to_process,function(x){
    title = "Default Vignette Title. Add yaml title: to your document"
    thisfile = read_file(x)
    stripped_yaml = gsub("---.*---","",thisfile)
    frontmatter = gsub("(---.*---).*","\\1",thisfile)
    con = textConnection(frontmatter)
    fm  = rmarkdown::yaml_front_matter(con)
    if(is.null(fm[["vignette"]])){
      #add boilerplate vignette yaml
      if(!is.null(fm$title))title = fm$title
      fm$vignette = paste0("%\\VignetteIndexEntry{",title,"}
                           %\\VignetteEngine{knitr::rmarkdown}
                           \\usepackage[utf8]{inputenc}")
    }else{
      #otherwise leave it as is.
    }
    write_me_out = paste0("---\n",yaml::as.yaml(fm),"---\n",stripped_yaml)
    write_me_out
  })
  names(write_me_out)=vignettes_to_process
  for(i in vignettes_to_process){
    writeLines(write_me_out[[i]],con = i)
    writeLines(write_me_out[[i]],con = file.path("inst/doc",basename(i)))
  }
}
