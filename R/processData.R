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
#' @importFrom here here
#' @importFrom here set_here
#' @import data.tree
#' @import devtools
DataPackageR <- function(arg = NULL,masterfile=NULL) {
  requireNamespace("futile.logger")
  requireNamespace("yaml")
  # This is cruft.
  # if (is.null(arg)) {
  #   parser <-
  #     OptionParser(usage = "R CMD DataPackageR [options] package")
  #   arguments = parse_args(parser,positional_arguments = 1)
  #   opt = arguments$options
  #   pkg_dir <- arguments$args
  # }else{
  #   if (arg == "./") {
  #     flog.fatal("Don't can't call buildDataSetPackage() inside a package directory.")
  #     stop("Quitting",call. = FALSE)
  #   }
  #   pkg_dir <- arg
  # }
  pkg_dir=arg
  pkg_dir = normalizePath(pkg_dir)
  raw_data_dir <- "data-raw"
  target <- normalizePath(file.path(pkg_dir,raw_data_dir))
  data_dir <- normalizePath(file.path(pkg_dir,"data"))
  raw_data_dir = target
  if (!file.exists(target)) {
    flog.fatal(paste0("Directory ",target," doesn't exist."))
    setwd(old)
    stop("exiting",call. = FALSE)
  }else{
    if (!file.exists(data_dir)) {
      dir.create(data_dir)
    }
    #get the current directory
    old <- getwd()
    #TODO see if we can get rid of the tryCatch
    # tryCatch({
      # cd into the package directory
      setwd(pkg_dir)
      
      #log to the log file
      #Create a log directory in inst/extdata
      logpath = file.path(normalizePath("inst/extdata"),"Logfiles")
      dir.create(logpath,recursive = TRUE,showWarnings = FALSE)
      #open a log file
      LOGFILE <-
        file.path(logpath,"processing.log")
      flog.appender(appender.tee(LOGFILE))
      flog.info(paste0("Logging to ", LOGFILE))
      
      #we know it's a proper package root, but we want to test if we have the necessary subdirectories
      if (!all(file_test(c("R","inst","data","data-raw"),op = "-d"))) {
        flog.fatal("You need a valid package data strucutre. Missing ./R ./inst ./data or ./data-raw subdirectories.")
        setwd(old)
        stop("exiting",call.=FALSE)
      }
      flog.info("Processing data")
      #read YAML
      ymlfile = dir(path = pkg_dir,pattern = "^datapackager.yml$",full.names = TRUE)
      if(length(ymlfile)==0){
        flog.fatal(paste0("Yaml configuration file not found at ",pkg_dir))
        setwd(old)
        stop("exiting",call.=FALSE)
      }
      ymlconf = read_yaml(ymlfile)
      #test that the structure of the yaml file is correct!
      if(!"configuration"%in%names(ymlconf)){
        flog.fatal("YAML is missing 'configuration:' entry")
        setwd(old)
        stop("exiting",call.=FALSE)
      }
      if(!all(c("files","objects")%in%map(ymlconf,names)[["configuration"]])){
        flog.fatal("YAML is missing files: and objects: entries")
        setwd(old)
        stop("exiting",call.=FALSE)
      }
      flog.info("Read yaml configuration")
      
      # r_files = unlist(map(ymlconf[["configuration"]][["files"]],"name"))
      # subset to files that have enable: TRUE
      r_files = unique(names(Filter(x = ymlconf[["configuration"]][["files"]],f=function(x)x$enabled)))
      if(length(r_files)==0){
        flog.fatal("No files enabled for processing!")
        stop("error",call.=FALSE)
      }
      objectsToKeep = map(ymlconf,"objects")[["configuration"]]
      
      r_files = file.path(raw_data_dir,r_files)
      
      if(all(!file.exists(r_files))){
        flog.fatal(paste0("Can't find any R or Rmd files."))
        flog.fatal(paste0("     Cant' find: ",r_files[!file.exists(r_files)]))
      }
      if(any(!file.exists(r_files))){
        flog.error(paste0("Can't find ",r_files[!file.exists(r_files)]))
      }
      flog.info(paste0("Found ",r_files))
      #TODO fix hidden warnings in test cases
      old_data_digest <- .parse_data_digest()
      pkg_description <-
        try(read.description(file="DESCRIPTION"),silent = TRUE)
      if (inherits(pkg_description,"try-error")) {
        setwd(old)
        flog.fatal("No valid DESCRIPTION file")
        stop(
          "You need a valid package DESCRIPTION file. Please see Writing R Extensions (http://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file).\n",pkg_description
        )
      }
      #check that we have at least one file
      if (length(r_files) == 0) {
        flog.fatal("You must specify at least one file to process.")
        setwd(old)
        stop("exiting",call.=FALSE)
      }
      if (length(objectsToKeep) == 0){
        flog.fatal("You must specify at least one data object.")
        setwd(old)
        stop("exiting",call.=FALSE)
      }
 
      #TODO Can we configure documentation in yaml?
      do_documentation <- FALSE
      #This flag indicates success
      can_write <- FALSE
      #environment for the data
      ENVS = new.env(hash = TRUE,parent = .GlobalEnv)
      for (i in seq_along(r_files)) {
        dataEnv <- new.env(hash = TRUE,parent = .GlobalEnv)
        flog.info(paste0("Processing ",i," of ",length(r_files),": ",r_files[i],"\n"))

        #config file goes in the root
        #render the r and rmd files
        render(input = r_files[i], envir= dataEnv, output_dir=logpath, clean=FALSE)
        #The created objects
        object_names <- ls(dataEnv)
        flog.info(paste0(sum(objectsToKeep%in%object_names), " required data objects created by ",basename(r_files[i])))
        
        if(sum(objectsToKeep%in%object_names)>0){
          for(o in objectsToKeep[objectsToKeep%in%object_names])
            assign(o,get(o, dataEnv),ENVS)
        }
      }
      dataEnv=ENVS
        #Digest each object
        new_data_digest <-
          .digest_data_env(ls(ENVS),dataEnv,pkg_description)
        if (!is.null(old_data_digest)) {
 
          string_check <-
            .check_dataversion_string(old_data_digest,new_data_digest)
          can_write <- FALSE
          if (.compare_digests(old_data_digest,new_data_digest,delta=masterfile) &
              string_check$isequal) {
                can_write <- TRUE
                flog.info(paste0(
                  "Processed data sets match existing data sets at version ",new_data_digest$DataVersion
                ))
              }else if ((!.compare_digests(old_data_digest,new_data_digest,delta=masterfile)) &
                        string_check$isequal) {
                        updated_version = .increment_data_version(pkg_description,new_data_digest);
                        pkg_description = updated_version$pkg_description
                        new_data_digest = updated_version$new_data_digest
                          can_write <- TRUE
                          flog.info(paste0(
                            "Data has been updated and DataVersion string incremented automatically to ",new_data_digest$DataVersion
                          ))
                        }else if (.compare_digests(old_data_digest,new_data_digest,delta=masterfile) &
                                  string_check$isgreater) {
                                    can_write <- TRUE
                                    flog.info("Data hasn't changed but the DataVersion has been bumped.")
                        }else if ((!.compare_digests(old_data_digest,new_data_digest,delta=masterfile)) &
                                         string_check$isgreater) {
                          can_write <- TRUE
                          flog.info("Data has changed and the DataVersion has been bumped.")
                        }
          
          if (can_write) {
            .save_data(new_data_digest,pkg_description,ls(dataEnv),dataEnv,old_data_digest = old_data_digest, masterfile=masterfile)
            do_documentation <- TRUE
          }else{
            flog.info(
              "Some data has changed, but the DataVersion string has not been incremented or\nis less than the version in DATADIGEST \nUpdate the DataVersion string to be greater than ",old_data_digest$DataVersion," in the DESCRIPTION file\nand re-run R CMD DataPackageR."
            )
          }
        }else{
          .save_data(new_data_digest,pkg_description,ls(dataEnv),dataEnv)
          do_documentation <- TRUE
        }
        if (do_documentation) {
          #Run autodoc #needs to be run when we have a partial build..
          if(!file.exists(file.path(target,"documentation.R")))
            .autoDoc(basename(pkg_dir),ds2kp = ls(dataEnv),env = dataEnv, path = target)
          
          #parse  documentation
          doc_parsed = .parseDocumentation(file.path(target,"documentation.R"))
          .identify_missing_docs = function(environment=NULL,description = NULL, docs = NULL){
            setdiff(ls(environment),setdiff(names(docs),description$Package))
          }
          
          # case where we add an object, ensures we combine the documentation properly
          missing_doc_for_autodoc = .identify_missing_docs(dataEnv,pkg_description,doc_parsed)
          if(length(missing_doc_for_autodoc)!=0){
            tmptarget = tempdir()
            .autoDoc(basename(pkg_dir),ds2kp = missing_doc_for_autodoc,env = dataEnv, path = tmptarget, name = "missing_doc.R")
            missing_doc = .parseDocumentation(file.path(tmptarget,"missing_doc.R"))
            
            doc_parsed = .mergeDocumentation(old = doc_parsed, new = missing_doc)
            docfile <-
              file(file.path(target,paste0("documentation",".R")),open = "w")
            sapply(doc_parsed,function(x) {
              writeLines(text = x,con = docfile)
            })
          }
          
          #Partial build if enabled=FALSE for any file
          #We've disabled an object but don't want to overwrite its documentation or remove it
          # The new approach just builds all the docs independent of what's enabled.
          # if(!all(unlist(map(ymlconf[["configuration"]][["files"]],"enabled")))){
          #   old_doc_file = list.files(file.path(pkg_dir,"R"),full.names = TRUE,paste0(pkg_description$Package,".R"))
          #   if (length(old_doc_file) != 0) {
          #     if (file.exists(old_doc_file)) {
          #       old_docs = .parseDocumentation(old_doc_file)
          #       not_built = setdiff(setdiff(names(old_docs), ls(dataEnv)), pkg_description$Package)
          #       #remove all but not_built from old_docs
          #       merged_docs = .mergeDocumentation(old = old_docs[not_built], new = doc_parsed)
          #       save_docs = do.call(c, merged_docs)
          #     }
            # }
            # else {
              # save_docs = do.call(c,doc_parsed)
            # }
          # }else{
            save_docs = do.call(c,doc_parsed)
          # }
          docfile <-
            file(file.path("R",pattern = paste0(pkg_description$Package,".R")),open = "w")
          sapply(save_docs,function(x) {
            writeLines(text = x,con = docfile)
          })
          close(docfile)
          flog.info(paste0("Copied documentation to ",file.path("R",paste0(pkg_description$Package,".R"))))
          #TODO test that we have documented everything successfully and that all files have been parsed successfully
          can_write = TRUE
        }
        eval(expr = expression(rm(list = ls())),envir = dataEnv)
      # } # we'll render one file after another then deal with the documentaton etc.
      # copy html files to vignettes
      .vignettesFromPPFiles(dir=pkg_dir)
      
    # },finally = {
      setwd(old);
    # })
  }
  flog.info("Done")
  if (can_write) {
    return(TRUE)
  }else{
    return(FALSE)
  }
}


.vignettesFromPPFiles <- function(dir=NULL) {
  pkg <- as.package(dir)
  check_suggested("rmarkdown")
  add_desc_package(pkg, "Suggests", "knitr")
  add_desc_package(pkg, "Suggests", "rmarkdown")
  add_desc_package(pkg, "VignetteBuilder", "knitr")
  use_directory("vignettes", pkg = pkg)
  use_git_ignore("inst/doc", pkg = pkg)
  lines = readLines(".gitignore")
  lines = gsub("inst/doc","",lines)
  writeLines(lines,".gitignore")
  # browser()
  try(dir.create("inst/doc"),silent=TRUE)
  #TODO maybe copy only the files that have both html and Rmd.
  rmdfiles_for_vignettes = list.files(path=file.path(pkg,"data-raw"),pattern="Rmd$",full.names=TRUE,recursive = FALSE)
  htmlfiles_for_vignettes = list.files(path=file.path(pkg,"inst/extdata/Logfiles"),pattern="html$",full.names =TRUE,recursive = FALSE)
  purrr::map(htmlfiles_for_vignettes,function(x)file.copy(x,file.path(pkg,"inst/doc",basename(x)),overwrite = TRUE))
  capture.output(purrr::map(rmdfiles_for_vignettes,function(x)file.copy(x,file.path(pkg,"vignettes",basename(x)),overwrite = TRUE)))
  vignettes_to_process = list.files(path=file.path(pkg,"vignettes"),pattern="Rmd$",full.names =TRUE,recursive=FALSE)
  write_me_out = purrr::map(vignettes_to_process,function(x){
    title = "Default Vignette Title. Add yaml title: to your document"
    thisfile = read_file(x)
    stripped_yaml = gsub("---\\s*\n.*\n---\\s*\n","", thisfile)
    frontmatter = gsub("(---\\s*\n.*\n---\\s*\n).*","\\1", thisfile)
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

    tmp = fm$vignette
    tmp = gsub("  $", "", paste0("vignette: >\n  ", gsub("\\}\\s*", "\\}\n  ", tmp)))
    fm$vignette = NULL
    write_me_out = paste0("---\n", paste0(yaml::as.yaml(fm), tmp), "---\n\n", stripped_yaml)
    write_me_out
  })
  names(write_me_out)=vignettes_to_process
  for(i in vignettes_to_process){
    writeLines(write_me_out[[i]],con = i)
    writeLines(write_me_out[[i]],con = file.path("inst/doc",basename(i)))
  }
}
