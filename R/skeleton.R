#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom usethis create_package
.codefile_validate <- function(code_files) {
  # do they exist?
  assertthat::assert_that(all(unlist(purrr::map(
    code_files, file.exists
  ))), msg = "code_files do not all exist!")
  # are the .Rmd files?
  assertthat::assert_that(all(grepl(".*\\.r$", tolower(code_files)) |
    grepl(".*\\.rmd$", tolower(code_files))),
  msg = "code files are not Rmd or R files!"
  )
}

#' Create a Data Package skeleton for use with DataPackageR.
#'
#' Creates a package skeleton directory structure for use with DataPackageR.
#' Adds the DataVersion string to DESCRIPTION, creates the DATADIGEST file, and the data-raw directory.
#' Updates the Read-and-delete-me file to reflect the additional necessary steps.
#' @name datapackage_skeleton
#' @param name  \code{character} name of the package to create.
#' @rdname datapackage_skeleton
#' @param path A \code{character} path where the pacakge is located. See \code{\link[utils]{package.skeleton}}
#' @param force \code{logical} Force the package skeleton to be recreated even if it exists. see \code{\link[utils]{package.skeleton}}
#' @param code_files Optional \code{character} vector of paths to Rmd files that process raw data
#' into R objects. 
#' @param r_object_names \code{vector} of quoted r object names , tables, etc. created when the files in \code{code_files} are run.
#' @param raw_data_dir \code{character} pointing to a raw data directory. Will be moved with all its subdirectories to "inst/extdata"
#' @param dependencies \code{vector} of \code{character}, paths to R files that will be moved to "data-raw" but not included in the yaml config file. e.g., dependency scripts.
#' @note renamed \code{datapackage.skeleton()} to \code{datapackage_skeleton()}.
#' @importFrom crayon bold green
#' @export
datapackage_skeleton <-
  function(name = NULL,
             path = ".",
             force = FALSE,
             code_files = character(),
             r_object_names = character(),
           raw_data_dir = character(),
           dependencies = character()) {
    if (is.null(name)) {
      stop("Must supply a package name", call. = FALSE)
    }
    if (force) {
      unlink(file.path(path,name),recursive = TRUE, force = TRUE)
    }
    usethis::create_package(path = file.path(path,name), 
                            rstudio = FALSE, open = FALSE)
 
    # create the rest of the necessary elements in the package
    package_path <- file.path(path, name)
    description <-
      desc::desc(file = file.path(package_path, "DESCRIPTION"))
    description$set("DataVersion" = "0.1.0")
    description$set("Version" = "1.0")
    description$set("Package" = name)
    description$set("Roxygen" = "list(markdown = TRUE)")
    message("Adding DataVersion string to DESCRIPTION")

    description$write()
    message("Creating data and data-raw directories")
    usethis::use_directory("data-raw")
    usethis::use_directory("data")
    usethis::use_directory("inst/extdata")
    con <-
      file(file.path(package_path, "Read-and-delete-me"), open = "w")
    writeLines(
      c(
        "Edit the DESCRIPTION file to reflect",
        "the contents of your package.",
        "Optionally put your raw data under",
        "'inst/extdata/'. If the datasets are large,",
        "they may reside elsewhere outside the package",
        "source tree. If you passed R and Rmd files to",
        "datapackage.skeleton, they should now appear in 'data-raw'.",
        "When you call package_build(), your datasets will",
        "be automatically documented. Edit datapackager.yml to",
        "add additional files / data objects to the package.",
        "After building, you should edit dat-raw/documentation.R",
        "to fill in dataset documentation details and rebuild.",
        "",
        "NOTES",
        "If your code relies on other packages,",
        "add those to the @import tag of the roxygen markup.",
        "The R object names you wish to make available",
        "(and document) in the package must match",
        "the roxygen @name tags and must be listed",
        "in the yml file."
      ),
      con
    )
    close(con)

    if (length(r_object_names) != 0) {
      message("configuring yaml file")
      # Rather than copy, read in, modify (as needed), and write.
      # process the string
      .copy_files_to_data_raw <- function(x, obj = c("code","dependencies")){
        if (length(x) != 0) {
          .codefile_validate(x)
          # copy them over
          obj <- match.arg(obj, c("code","dependencies"))
          message(paste0(crayon::bold("Moving ",obj," into "),
                         crayon::green("data-raw")))
          # purrr::map(x, function(y)
          for (y in x)
            file.copy(y, file.path(package_path, "data-raw"), overwrite = TRUE)
        }
      }
      
      .copy_data_to_inst_extdata <- function(x){
        if (length(x) != 0) {
          # copy them over
          cat(crayon::bold("Moving raw data into"), 
              crayon::green("inst/extdata"))
          file.copy(x, file.path(package_path, "inst/extdata"),
                    recursive = TRUE, overwrite = TRUE)
        }
      }
      .copy_files_to_data_raw(code_files, obj = "code")
      .copy_files_to_data_raw(dependencies, obj = "dependencies")
      .copy_data_to_inst_extdata(raw_data_dir)
      
      yml <- construct_yml_config(code = code_files, data = r_object_names)
      yaml::write_yaml(yml, file = file.path(package_path, "datapackager.yml"))
    } else {
      stop("No r_object_names specified to move into the datapackage.")
    }

    oldrdfiles <-
      list.files(
        path = file.path(package_path, "man"),
        pattern = "Rd",
        full.names = TRUE
      )
    file.remove(file.path(package_path, "NAMESPACE"))
    oldrdafiles <-
      list.files(
        path = file.path(package_path, "data"),
        pattern = "rda",
        full.names = TRUE
      )
    oldrfiles <-
      list.files(
        path = file.path(package_path, "R"),
        pattern = "R",
        full.names = TRUE
      )
    file.remove(oldrdafiles)
    file.remove(oldrfiles)
    file.remove(oldrdfiles)
    invisible(NULL)
  }


#' @rdname datapackage_skeleton
#' @name datapackage.skeleton
#' @param list Not used.
#' @param environment Not used.
#' @aliases datapackage_skeleton
#' @export
#' @examples
#' f <- tempdir()
#' f <- file.path(f,"foo.Rmd")
#' con <- file(f)
#' writeLines("```{r}\n tbl = table(sample(1:10,1000,replace=TRUE)) \n```\n",con=con)
#' close(con)
#' pname <- basename(tempfile())
#' datapackage_skeleton(name = pname,
#'    path = tempdir(),
#'    force = TRUE,
#'    r_object_names = "tbl",
#'    code_files = f)
datapackage.skeleton <- function(name = NULL,
                                 list = character(),
                                 environment = .GlobalEnv,
                                 path = ".",
                                 force = FALSE,
                                 code_files = character(),
                                 r_object_names = character()) {
  warning("Please use datapackage_skeleton() instead of datapackage.skeleton()")
  datapackage_skeleton(
    name = name,
    path = path,
    force = force,
    code_files = code_files,
    r_object_names = r_object_names
  )
}
