#' Edit DataPackageR yaml configuration
#'
#' @rdname yaml
#' @param path Path to the data package source or path to write config file (for \code{yml_write})
#' @return A yaml configuration structured as an R nested list.
#' @description Edit a yaml configuration file via an API. 
#' @details Add, remove files and objects, enable or disable parsing of specific files,  list objects or files in a yaml config, or write a config back to a package.
#' @importFrom yaml yaml.load_file as.yaml write_yaml
#' @export
#'
#' @examples
#' \dontrun{
#' yml_find('/tmp')
#' }
yml_find <- function(path) {
  path <- normalizePath(path)
  config_yml <- is_r_package$find_file("datapackager.yml", path = path)
  if (!file.exists(config_yml)) {
    stop("Can't find a datapackager.yml config at ",
         dirname(config_yml),
         call. = FALSE)
  }
  config <- yaml.load_file(config_yml)
  attr(config, "path") <- config_yml
  return(config)
}

#'@rdname yaml
#'@param config an R representation of the datapackager.yml config, returned by yml_find, or a path to the package root.
#'@export
yml_add_files <- function(config, filenames) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  for (i in filenames) {
    if (is.null(config[["configuration"]][["files"]][[i]])) {
      config[["configuration"]][["files"]][[i]] <- list()
      config[["configuration"]][["files"]][[i]]$name <- i
      config[["configuration"]][["files"]][[i]]$enabled <- TRUE
    }
  }
  cat(as.yaml(config))
  return(config)
}

#'@rdname yaml
#'@param filenames A vector of filenames.
#'@export
yml_disable_compile <- function(config, filenames) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  for (i in filenames) {
    if (!is.null(config[["configuration"]][["files"]][[i]])) {
      config[["configuration"]][["files"]][[i]]$enabled <- FALSE
    }
  }
  return(config)
}

#'@rdname yaml
#'@export
yml_enable_compile <- function(config, filenames) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  for (i in filenames) {
    if (!is.null(config[["configuration"]][["files"]][[i]])) {
      config[["configuration"]][["files"]][[i]]$enabled <- TRUE
    }
  }
  return(config)
}


#'@rdname yaml
#'@param objects A vector of R object names.
#'@export
yml_add_objects <- function(config, objects) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  config[["configuration"]][["objects"]] <-
    unique(c(config[["configuration"]][["objects"]],
             objects))
  cat(as.yaml(config))
  return(config)
}


#'@rdname yaml
#'@export
yml_list_objects <- function(config) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  cat(config[["configuration"]][["objects"]])
  invisible(config[["configuration"]][["objects"]])
}

#'@rdname yaml
#'@export
yml_list_files <- function(config) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  cat(unlist(map(config[["configuration"]][["files"]], "name")))
  invisible(unlist(map(config[["configuration"]][["files"]], "name")))
}

#'@rdname yaml
#'@export
yml_remove_objects <- function(config, objects) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  config[["configuration"]][["objects"]] <-
    setdiff(config[["configuration"]][["objects"]],
            objects)
  cat(as.yaml(config))
  return(config)
}

#'@rdname yaml
#'@export
yml_remove_files <- function(config, filenames) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  for (i in filenames) {
    if (!is.null(config[["configuration"]][["files"]][[i]])) {
      config[["configuration"]][["files"]][[i]] <- NULL
    }
  }
  cat(as.yaml(config))
  return(config)
}

#'@rdname yaml
#'@export
yml_write <- function(config, path = NULL) {
  if (is.character(config)) {
    stop(
      paste0("config must be a datapackager.yml configuration",
      " in r object representation, as ready by yml_find()"),
      call. = FALSE
    )
  }
  if (is.null(path))
    path <-
      attr(config, "path")
  else
    path <- file.path(path, "datapackager.yml")
  write_yaml(config, file = path)
}

#' Construct a datapackager.yml configuration
#' 
#' @param code A vector of filenames
#' @param data A vector of quoted object names
#' @return a datapackager.yml configuration represented as an R object
#' @description Constructs a datapackager.yml configuration object from a vector of file names and a vector of object names (all quoted).
#' Can be written to disk via \code{yml_write}
#' @examples
#' conf = construct_yml_config(code=c('file1.rmd','file2.rmd'), data=c('object1','object2'))
#' tmp = normalizePath(tempdir())
#' yml_write(conf,path=tmp)
#' @export
construct_yml_config <- function(code = NULL, data = NULL) {
  code <- basename(code)
  files <- vector(length = length(code), mode = "list")
  names(files) <- code
  for (i in code) {
    files[[i]]$name <- i
    files[[i]]$enabled <- TRUE
  }
  files

  yml <- list(configuration = list(files = files, objects = data))
  return(yml)
}
