#' Edit DataPackageR yaml configuration
#'
#' @rdname yaml
#' @param path Path to the data package source.
#' @return A yaml configuration structured as an R nested list.
#' @description Edit a yaml configuration file via an API. 
#' @details Add, remove files and objects, enable or disable parsing of specific files,  list objects or files in a yaml config, or write a config back to a package.
#' @importFrom yaml yaml.load_file as.yaml write_yaml
#' @export
#'
#' @examples
#' yml_find("/tmp")
yml_find = function(path){
  path = normalizePath(path)
  config_yml = is_r_package$find_file("datapackager.yml",path=path)
  if(!file.exists(config_yml)){
    stop("Can't find a datapackager.yml config at ", dirname(config_yml), call.=FALSE)
  }
  config = yaml.load_file(config_yml)
  attr(config, "path") <- config_yml
  return(config)
}

#'@rdname yaml
#'@param config an R representation of the datapackager.yml config, returned by yml_find, or a path to the package root.
#'@export
yml_add_files = function(config, filenames){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  config[["configuration"]][["files"]] = unique(c(config[["configuration"]][["files"]],filenames))
  cat(as.yaml(config))
  return(config)
}

#'@rdname yaml
#'@param filenames A vector of filenames.
#'@export
yml_disable_compile = function(config, filenames){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  return(config)
  
}

#'@rdname yaml
#'@export
yml_enable_compile = function(config, filenames){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  return(config)
}


#'@rdname yaml
#'@param objects A vector of R object names.
#'@export
yml_add_objects = function(config, objects){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  config[["configuration"]][["objects"]] = unique(c(config[["configuration"]][["objects"]],objects))
  cat(as.yaml(config))
  return(config)
}


#'@rdname yaml
#'@export
yml_list_objects = function(config){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  cat(config[["configuration"]][["objects"]])
  invisible(config)
}

#'@rdname yaml
#'@export
yml_list_files = function(config){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  cat(config[["configuration"]][["files"]])
  invisible(config)
}

#'@rdname yaml
#'@export
yml_remove_objects = function(config, objects){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  config[["configuration"]][["objects"]] = setdiff(config[["configuration"]][["objects"]],objects)
  cat(as.yaml(config))
  return(config)
}

#'@rdname yaml
#'@export
yml_remove_files = function(config, filenames){
  if(is.character(config)){ #assume config is a package root path
    config = yml_find(config)
  }
  config[["configuration"]][["files"]] = setdiff(config[["configuration"]][["files"]],filenames)
  cat(as.yaml(config))
  return(config)
}

#'@rdname yaml
#'@export
yml_write = function(config){
  if(is.character(config)){
    stop("config must be a datapackager.yml configuration in r object representation, as ready by yml_find()", call.=FALSE)
  }
  path = attr(config,"path")
  write_yaml(config, file = path)
}
