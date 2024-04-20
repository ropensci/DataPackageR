#' Add a raw data set to inst/extdata
#'
#' The file or directory specified by \code{path} will be moved into
#' the inst/extdata directory.
#'
#' @param path \code{character} path to file or directory.
#' @param ignore \code{logical} whether to ignore the path or file in git and R build.
#'
#' @return invisibly returns TRUE for success. Stops on failure.
#' @importFrom usethis proj_get proj_set create_package use_data_raw
#' @importFrom utils file_test
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' myfile <- tempfile()
#' file <- system.file("extdata", "tests", "extra.Rmd",
#'                      package = "DataPackageR")
#' raw_data <- system.file("extdata", "tests", "raw_data",
#'                         package = "DataPackageR")
#' datapackage_skeleton(
#'   name = "datatest",
#'   path = tempdir(),
#'   code_files = file,
#'   force = TRUE,
#'   r_object_names = "data")
#' use_raw_dataset(raw_data)
#' }
use_raw_dataset <- function(path = NULL, ignore = FALSE) {
  if (is.null(path)) {
    stop("You must provide a full path to a file or directory.")
  }
  proj_path <- usethis::proj_get()
  if (!utils::file_test("-d", file.path(proj_path, "inst", "extdata"))) {
    stop(paste0("inst/extdata doesn't exist in ", proj_path), call. = FALSE)
  }
  raw_file <- normalizePath(path)
  if (utils::file_test("-f", raw_file)) {
    file.copy(
      from = raw_file,
      to = file.path(proj_path, "inst", "extdata"),
      overwrite = TRUE
    )
    if (ignore) {
      # inst/extdata is a path relative to the project root
      # as needed by git_ignore
      use_ignore(basename(raw_file), path = file.path("inst", "extdata"))
    }
    return(invisible(TRUE))
  } else if (utils::file_test("-d", raw_file)) {
    file.copy(
      from = raw_file,
      to = file.path(proj_path, "inst", "extdata"),
      recursive = TRUE, overwrite = TRUE
    )
    if (ignore) {
      #should work and the directory should be ignored
      use_ignore(basename(raw_file), path = file.path("inst", "extdata"))
    }
    return(invisible(TRUE))
  } else {
    stop("path must be a path to an existing file or directory.")
  }
}


#' Add a processing script to a data package.
#'
#' The Rmd or R file or directory specified by \code{file} will be moved into
#' the data-raw directory. It will also be added to the yml configuration file.
#' Any existing file by that name will be overwritten when overwrite is set to TRUE
#'
#' @param file \code{character} path to an existing file or name of a new R or Rmd file to create.
#' @param title \code{character} title of the processing script for the yaml header. Used only if file is being created.
#' @param author \code{character} author name for the yaml header. Used only if the file is being created.
#' @param overwrite \code{logical} default FALSE. Overwrite existing file of the same name.
#'
#' @return invisibly returns TRUE for success. Stops on failure.
#' @importFrom usethis proj_get proj_set create_package use_data_raw
#' @importFrom utils file_test packageVersion
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' myfile <- tempfile()
#' file <- system.file("extdata", "tests", "extra.Rmd",
#'                      package = "DataPackageR")
#' datapackage_skeleton(
#'   name = "datatest",
#'   path = tempdir(),
#'   code_files = file,
#'   force = TRUE,
#'   r_object_names = "data")
#' use_processing_script(file = "newScript.Rmd",
#'     title = "Processing a new dataset",
#'     author = "Y.N. Here.")
#' }
use_processing_script <- function(file = NULL, title = NULL, author = NULL, overwrite = FALSE) {
  if (is.null(file)) {
    stop("You must provide a full path to a file.")
  }
  proj_path <- usethis::proj_get()
  if (!utils::file_test("-d", file.path(proj_path, "data-raw"))) {
    stop(paste0("data-raw doesn't exist in ", proj_path), call. = FALSE)
  }
  #check if the given file or directory already exists
  if (utils::file_test("-f",file.path(proj_path,"data-raw",file))|utils::file_test("-d",file.path(proj_path,"data-raw",file))) { #nolint
    if (overwrite) {
      .bullet(paste0("Courtesy warning: ", basename(file), " exists in ",crayon::blue("'data-raw'"),", and ",crayon::red("WILL")," be overwritten."),bullet = crayon::red("\u2622")) #nolint
    } else {
      .bullet(paste0("Courtesy warning: ", basename(file), " exists in ",crayon::blue("'data-raw'"),", and ",crayon::red("WILL NOT")," be overwritten."),bullet = crayon::red("\u2622")) #nolint
    }
  }
  raw_file <- suppressWarnings(normalizePath(file))

  if (utils::file_test("-f", raw_file)) {
    # test if it's an R or Rmd file.
    if (!(grepl("\\.rmd$", tolower(raw_file)) |
      grepl("\\.r$", tolower(raw_file)))) {
      stop("file must be an .R or .Rmd.")
    }
    file.copy(
      from = raw_file,
      to = file.path(proj_path, "data-raw"),
      overwrite = overwrite
    )
    # add it to the yaml
    yml <- yml_find(path = proj_path)
    yml <- yml_add_files(yml, basename(raw_file))
    yml_write(yml)

    invisible(TRUE)
  } else if (utils::file_test("-d", raw_file)) {
    stop("path argument must be a path to a file, not a directory.")
  } else if ((!grepl("/", raw_file) &
    !grepl("^\\.", raw_file)) &
    (grepl("\\.r$", tolower(raw_file)) |
      grepl("\\.rmd$", tolower(raw_file)))) {
    # we have a valid file name and should create it.
    if (file.exists(file.path(proj_path, "data-raw", basename(raw_file))) &&
        !overwrite) {
      .bullet(paste0("Skipping file creation: pass overwrite = TRUE to use_processing_script()"), bullet = crayon::red("\u2622")) #nolint
    } else {
      if (getOption('DataPackageR_verbose', TRUE)){
        cat("Attempting to create ", raw_file)
      }
      file.create(file.path(proj_path, "data-raw", basename(raw_file)))
      .update_header(file.path(proj_path,
                               "data-raw",
                               basename(raw_file)),
                     title = title,
                     author = author)
    }
    # add it to the yaml.
    yml <- yml_find(path = proj_path)
    yml <- yml_add_files(yml, basename(raw_file))
    yml_write(yml)

    invisible(TRUE)
  } else {
    stop("path argument must be a path to an existing file or a new file name, cannot begin with a dot '.' and must end in R or Rmd (case insensitive).") # nolint
  }
}



#' Add a data object to a data package.
#'
#' The data object will be added to the yml configuration file.
#' @param object_name Name of the data object. Should be created by a processing script in data-raw. \code{character} vector of length 1.
#'
#' @return invisibly returns TRUE for success.
#' @export
#'
#' @examples
#' if(rmarkdown::pandoc_available()){
#' myfile <- tempfile()
#' file <- system.file("extdata", "tests", "extra.Rmd",
#'                      package = "DataPackageR")
#' datapackage_skeleton(
#'   name = "datatest",
#'   path = tempdir(),
#'   code_files = file,
#'   force = TRUE,
#'   r_object_names = "data")
#' use_data_object(object_name = "newobject")
#' }
#'
use_data_object <- function(object_name = NULL) {
  if (is.null(object_name)) {
    stop(paste0(object_name, " cannot be NULL."))
  } else if(!is.character(object_name) | !length(object_name)==1){
    stop("object_name must be a character vector of length 1.")
  } else {
    proj_path <- usethis::proj_get()
    yml <- yml_find(path = proj_path)
    yml <- yml_add_objects(yml, objects = object_name)
    yml_write(yml)
    invisible(TRUE)
  }
}


.update_header <- function(file = NULL,
                           title = NULL,
                           author = NULL) {
  file_contents <- readLines(file)
  if (grepl("\\.r$", tolower(file))) {
    # get the front matter as comments.
    partitioned_file <- .partition_r_front_matter(file_contents)
  } else {
    partitioned_file <- .partition_rmd_front_matter(file_contents)
  }
  if (!is.null(partitioned_file$front_matter)) {
    front_matter <- .parse_yaml_front_matter(
      gsub("#'\\s*", "", partitioned_file$front_matter))
  } else {
    front_matter <- list()
  }
  {
    if (!is.null(title)) {
      front_matter$title <- title
    }
    if (!is.null(author)) {
      front_matter$author <- author
    }

    front_matter <- yaml::as.yaml(front_matter)
    front_matter <-
      ifelse(
        grepl("\\.r$", tolower(file)),
        gsub(
          "#' $", "",
          gsub(
            "\n", "\n#' ",
            paste0("#' ", front_matter)
          )
        ),
        front_matter
      )

    # open the file for writing.
    connection <- file(file, open = "w+")
    # write the header
    writeLines(ifelse(grepl("\\.r$", tolower(file)),
                      "#' ---", "---"), con = connection)
    writeLines(front_matter, con = connection, sep = "")
    writeLines(ifelse(grepl("\\.r$", tolower(file)),
                      "#' ---", "---"), con = connection)
    # write the body
    if (!is.null(partitioned_file$body)) {
      writeLines(partitioned_file$body, con = connection)
    }
    # close the file
    close(connection)
  }
}

.partition_r_front_matter <- function(input_lines) {
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] >
      1) && grepl("^#'\\s*---\\s*$", input_lines[delimiters[1]])) {
      if (delimiters[1] == 1) {
        TRUE
      } else {
        .is_blank(input_lines[1:delimiters[1] - 1])
      }
    }
    else {
      FALSE
    }
  }
  delimiters <- grep("^(#'\\s*---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {
    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]
    input_body <- c()
    if (delimiters[1] > 1) {
      input_body <- c(input_body, input_lines[1:delimiters[1] -
        1])
    }
    if (delimiters[2] < length(input_lines)) {
      input_body <- c(input_body, input_lines[-(1:delimiters[2])])
    }
    list(front_matter = front_matter, body = input_body)
  }
  else {
    list(front_matter = NULL, body = input_lines)
  }
}


.partition_rmd_front_matter <- function(input_lines) {
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] >
      1) && grepl("^---\\s*$", input_lines[delimiters[1]])) {
      if (delimiters[1] == 1) {
        TRUE
      } else {
        .is_blank(input_lines[1:delimiters[1] - 1])
      }
    }
    else {
      FALSE
    }
  }
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {
    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]
    input_body <- c()
    if (delimiters[1] > 1) {
      input_body <- c(input_body, input_lines[1:delimiters[1] -
        1])
    }
    if (delimiters[2] < length(input_lines)) {
      input_body <- c(input_body, input_lines[-(1:delimiters[2])])
    }
    list(front_matter = front_matter, body = input_body)
  }
  else {
    list(front_matter = NULL, body = input_lines)
  }
}


.parse_yaml_front_matter <- function(front_matter) {
  if (length(front_matter) > 2) {
    front_matter <- front_matter[2:(length(front_matter) -
      1)]
    front_matter <- paste(front_matter, collapse = "\n")
    .validate_front_matter(front_matter)
    parsed_yaml <- .yaml_load_utf8(front_matter)
    if (is.list(parsed_yaml)) {
      parsed_yaml
    } else {
      list()
    }
  }
  else {
    list()
  }
}

.validate_front_matter <- function(front_matter) {
  front_matter <- .trim_trailing_ws(front_matter)
  if (grepl(":$", front_matter)) {
    stop("Invalid YAML front matter (ends with ':')", call. = FALSE)
  }
}

.trim_trailing_ws <- function(x) {
  sub("\\s+$", "", x)
}

.yaml_load_utf8 <- function(string, ...) {
  string <- paste(string, collapse = "\n")
  if (packageVersion("yaml") >= "2.1.14") {
    yaml::yaml.load(string, ...)
  }
  else {
    .mark_utf8(yaml::yaml.load(enc2utf8(string), ...)) #nocov
  }
}

.mark_utf8 <- function(x) {
  if (is.character(x)) {
    Encoding(x) <- "UTF-8"
    return(x)
  }
  if (!is.list(x)) {
    return(x)
  }
  attrs <- attributes(x)
  res <- lapply(x, .mark_utf8)
  attributes(res) <- attrs
  names(res) <- .mark_utf8(names(res))
  res
}

.is_blank <- function(x) {
  if (length(x)) {
    all(grepl("^\\s*$", x))
  } else {
    TRUE
  }
}
