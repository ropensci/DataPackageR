#' Edit DataPackageR yaml configuration
#'
#' @rdname yaml
#' @param path Path to the data package source or path to write config file (for \code{yml_write})
#' @return A yaml configuration structured as an R nested list.
#' @description Edit a yaml configuration file via an API.
#' @details Add, remove files and objects, enable or disable parsing of specific files,  list objects or files in a yaml config, or write a config back to a package.
#' @importFrom yaml yaml.load_file as.yaml write_yaml
#' @importFrom stats runif
#' @importFrom withr with_options 
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' f <- tempdir()
#' f <- file.path(f,"foo.Rmd")
#' con <- file(f)
#' writeLines("```{r}\n tbl = table(sample(1:10,1000,replace=TRUE)) \n```\n",con=con)
#' close(con)
#' pname <- basename(tempfile())
#' datapackage_skeleton(name=pname,
#'    path = tempdir(),
#'    force = TRUE,
#'    r_object_names = "tbl",
#'    code_files = f)
#' yml <- yml_find(file.path(tempdir(),pname))
#' cat(yaml::as.yaml(yml))
#' yml <- yml_add_files(yml,"foo.Rmd")
#' yml_list_files(yml)
#' yml <- yml_disable_compile(yml,"foo.Rmd")
#' cat(yaml::as.yaml(yml))
#' yml <- yml_enable_compile(yml,"foo.Rmd")
#' cat(yaml::as.yaml(yml))
#' yml <- yml_add_objects(yml,"data1")
#' yml_list_objects(yml)
#' yml <- yml_remove_objects(yml,"data1")
#' yml <- yml_remove_files(yml,"foo.Rmd")
#' }
yml_find <- function(path) {
  path <- normalizePath(path, winslash = "/")
  config_yml <- is_r_package$find_file("datapackager.yml", path = path)
  if (!file.exists(config_yml)) {
    stop("Can't find a datapackager.yml config at ",
      dirname(config_yml),
      call. = FALSE
    )
  }
  config <- yaml::yaml.load_file(config_yml)
  attr(config, "path") <- config_yml
  return(config)
}

#' @rdname yaml
#' @param config an R representation of the datapackager.yml config, returned by yml_find, or a path to the package root.
#' @export
yml_add_files <- function(config, filenames) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  for (i in filenames) {
    if (is.null(config[["configuration"]][["files"]][[i]])) {
      config[["configuration"]][["files"]][[i]] <- list()
      # config[["configuration"]][["files"]][[i]]$name <- i
      config[["configuration"]][["files"]][[i]]$enabled <- TRUE
    }
  }
  cat(yaml::as.yaml(config))
  return(config)
}

#' @rdname yaml
#' @param filenames A vector of filenames.
#' @export
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

#' @rdname yaml
#' @export
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


#' @rdname yaml
#' @param objects A vector of R object names.
#' @export
yml_add_objects <- function(config, objects) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  config[["configuration"]][["objects"]] <-
    unique(c(
      config[["configuration"]][["objects"]],
      objects
    ))
  cat(yaml::as.yaml(config))
  return(config)
}


#' @rdname yaml
#' @export
yml_list_objects <- function(config) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  cat("\n")
  cat(config[["configuration"]][["objects"]])
  invisible(config[["configuration"]][["objects"]])
}

#' @rdname yaml
#' @export
yml_list_files <- function(config) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  cat("\n")
  cat(names(config[["configuration"]][["files"]]))
  invisible(names(config[["configuration"]][["files"]]))
}

#' @rdname yaml
#' @export
yml_remove_objects <- function(config, objects) {
  if (is.character(config)) {
    # assume config is a package root path
    config <- yml_find(config)
  }
  config[["configuration"]][["objects"]] <-
    setdiff(
      config[["configuration"]][["objects"]],
      objects
    )
  cat(yaml::as.yaml(config))
  return(config)
}

#' @rdname yaml
#' @export
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
  cat(yaml::as.yaml(config))
  return(config)
}

#' @rdname yaml
#' @export
yml_write <- function(config, path = NULL) {
  if (is.character(config)) {
    stop(
      paste0(
        "config must be a datapackager.yml configuration",
        " in r object representation, as ready by yml_find()"
      ),
      call. = FALSE
    )
  }
  if (is.null(path)) {
    path <-
      attr(config, "path")
  } else {
    path <- file.path(path, "datapackager.yml")
  }
  yaml::write_yaml(config, file = path)
}


.create_tmpdir_render_root <- function(sub = NULL) {
  if (is.null(sub)) {
    sub <- as.character(as.integer(stats::runif(1) * 1000000))
  }
  render_root <- file.path(tempdir(), sub)
  tempdir_exists <-
    try(normalizePath(dirname(render_root),
      winslash = "/",
      mustWork = TRUE
    ),
    silent = TRUE
    )
  if (!dir.exists(render_root)) {
    dir.create(render_root, recursive = TRUE, showWarnings = FALSE)
  }
  render_root <- normalizePath(render_root, winslash = "/", mustWork = TRUE)
  return(render_root)
}

#' Construct a datapackager.yml configuration
#'
#' @param code A vector of filenames
#' @param data A vector of quoted object names
#' @param render_root The root directory where the package data processing code will be rendered.
#' Defaults to is set to a randomly generated named subdirectory of \code{tempdir()}.
#' @return a datapackager.yml configuration represented as an R object
#' @description Constructs a datapackager.yml configuration object from a vector of file names and a vector of object names (all quoted).
#' Can be written to disk via \code{yml_write}.
#' \code{render_root} is set to a randomly generated named subdirectory of \code{tempdir()}.
#' @examples
#' conf <- construct_yml_config(code = c('file1.rmd','file2.rmd'), data=c('object1','object2'))
#' tmp <- normalizePath(tempdir(), winslash = "/")
#' yml_write(conf,path=tmp)
#' @export
construct_yml_config <- function(code = NULL, data = NULL, render_root = NULL) {
  if (!is.null(code)) {
    code <- basename(code)
  }
  files <- vector(length = length(code), mode = "list")
  names(files) <- code
  for (i in code) {
    files[[i]]$enabled <- TRUE
  }
  
  # create render root at a temporary directory.
  # this will be stored in the yaml. What if we restart?
  # see processData - it gets validated and created if not existing.
  # would prefer to have something like "NULL" or "tmp" specify a default to a
  # temporary directory.  But also have a consistent subdirectory beneath it.
  # currently not consistent, since we are randomly
  # generating a subdirectory name.
  # we could use "tmp: subdir" and construct the path.

  yml <- list(configuration = list(files = files, objects = data))
  if (is.null(render_root)) {
    render_root <- .create_tmpdir_render_root()
    yml[["configuration"]]$render_root$tmp <- basename(render_root)
  } else {
    render_root <-
      try(normalizePath(render_root,
        winslash = "/",
        mustWork = TRUE
      ),
      silent = TRUE
      )
    if (inherits(render_root, "try-error")) {
      .multilog_fatal(paste0(
        dirname(render_root),
        " doesn't exist!"
      ))
      stop("error", call. = FALSE)
    }
    yml[["configuration"]]$render_root <- render_root
  }
  return(yml)
}

.get_render_root <- function(x) {
  if ("tmp" %in% names(x$configuration$render_root)) {
    sub <- x$configuration$render_root$tmp
    render_root <- .create_tmpdir_render_root(sub)
    return(render_root)
  } else if (length(x$configuration$render_root) != 0) {
    return(x$configuration$render_root)
  } else {
    .multilog_fatal("render_root is not set in yaml")
    stop("error", call. = FALSE)
  }
}
