
#' Read an object created in a previously run processing script.
#'
#' @param name \code{character} the name of the object. Must be a
#' name available in the configuration objects. Other objects are not saved.
#' @details This function is only accessible within an R or Rmd file processed by DataPackageR.
#' It searches for an environment named \code{ENVS} within the current environment,
#' that holds the object with the given \code{name}. Such an environment is constructed and populated
#' with objects specified in the yaml \code{objects} property and passed along
#' to subsequent R and Rmd files as DataPackageR processes them in order.
#' @return An R object.
#' @export
#' @examples
#' if(rmarkdown::pandoc_available()){
#' ENVS <- new.env() # ENVS would be in the environment
#'                  # where the data processing is run. It is
#'                  # handled automatically by the package.
#' assign("find_me", 100, ENVS) #This is done automatically by DataPackageR
#'
#' find_me <- datapackager_object_read("find_me") # This would appear in an Rmd processed by
#'                                     # DataPackageR to access the object named "find_me" created
#'                                     # by a previous script. "find_me" would also need to
#'                                     # appear in the objects property of config.yml
#' }
datapackager_object_read <- function(name) {
  
  # get(name, get("ENVS", parent.frame()))
  
  #when the datapackage is being build, it will only read the object from the environment. when
  # in interactive mode, it should be try to load the objects reviously generated. 
  # It will preferentially read the object from the the temporary folder, but if that does not exist, it will read the
  # object from the the data folder
  buildingPackage<-getOption("DataPackageR_packagebuilding",TRUE)
  
  
  object<-try(get(name, get("ENVS", parent.frame(),inherits=FALSE),inherits=FALSE),silent=TRUE)
  
  if( !buildingPackage && inherits(object,"try-error")){
    #if the package is not being build and the object is not found in the "ENVS" environment
    temp_folder_path<-file.path(tempdir(),yml_find(usethis::proj_get())[["configuration"]][["render_root"]]$tmp)
    
    if(file.exists(objectPath<-file.path(temp_folder_path,paste0(name,".rds")))){
      message('loading ',name,' from temporary folder from previous build attempt.')
      object<-readRDS(objectPath)
      
    }else if(file.exists(objectPath<-file.path(project_data_path(),paste0(name,".rda")))){
      message('loading ',name,' from data directory.')
      print(objectPath)
      load_env<-new.env()
      load(objectPath,envir = load_env)
      object<-load_env[[ls(load_env)[1]]]
      
    }else{
      stop(paste(name,'not found!'))
      
    }
  }else if(inherits(object,"try-error")){
    #if the package is being build and the object is not found in the "ENVS" environment,
    # pass on the original error warning
    stop(object[1])
    
  }
  
  return(object)
}
