#' @importFrom assertthat assert_that
#' @importFrom purrr map
.codefile_validate <- function(code_files) {
  # do they exist?
  assert_that(all(unlist(map(
    code_files, file.exists
  ))), msg = "code_files do not all exist!")
  # are the .Rmd files?
  assert_that(all(grepl(".*\\.r$", tolower(code_files)) |
                    grepl(".*\\.rmd$", tolower(code_files))),
              msg = "code files are not Rmd or R files!")
}

#' Create a Data Package skeleton for use with DataPackageR.
#'
#' Creates a package skeleton for use with preprpocessData. Creates the additional information needed for versioning
#' datasets, namely the DataVersion string in DESCRIPTION, DATADIGEST, and the data-raw directory. Updates Read-and-delete-me
#' to reflect the additional necessary steps.
#' @name datapackage.skeleton
#' @param name  see \code{\link[utils]{package.skeleton}}
#' @param list see \code{\link[utils]{package.skeleton}}
#' @param environment see \code{\link[utils]{package.skeleton}}
#' @param path see \code{\link[utils]{package.skeleton}}
#' @param force see \code{\link[utils]{package.skeleton}}
#' @param code_files Optional character vector of paths to Rmd files that process raw data
#' into R objects. Treated differently that \code{code_files} in \code{\link[utils]{package.skeleton}}. 
#' Will always pass an empty \code{character()} vector to that function.
#' @param r_object_names \code{vector} of quoted r object names , tables, etc. created by \code{code_files}.
#' @export
#' @examples
#' \dontrun{
#' datapackage.skeleton(name="MyDataPackage",path="/tmp")
#' }
datapackage.skeleton <-
  function(name = NULL,
           list = character(),
           environment = .GlobalEnv,
           path = ".",
           force = FALSE,
           code_files = character(),
           r_object_names = character()) {
    if (is.null(name)) {
      stop("Must supply a package name", call. = FALSE)
    }
    if (length(list) == 0) {
      # don't pass on the code_files here, but use that argument to
      package.skeleton(
        name = name,
        environment = environment,
        path = path,
        force = force,
        code_files =
          character()
      )
    } else{
      package.skeleton(
        name = name,
        list = list,
        environment = environment,
        path = path,
        force =
          force,
        code_files = code_files
      )
    }
    #create the rest of the necessary elements in the package
    package_path <- file.path(path, name)
    description <-
      desc(file = file.path(package_path, "DESCRIPTION"))
    description$set("DataVersion" = "0.1.0")
    description$set("Package" = name)
    message("Adding DataVersion string to DESCRIPTION")

    description$write()
    message("Creating data and data-raw directories")
    dir.create(
      file.path(package_path, "data-raw"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(file.path(package_path, "data"),
               showWarnings = FALSE,
               recursive = TRUE)
    dir.create(file.path(package_path, "R"),
               showWarnings = FALSE,
               recursive = TRUE)
    dir.create(
      file.path(package_path, "inst/extdata"),
      recursive = TRUE,
      showWarnings = FALSE
    )
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
      if (length(code_files) != 0) {
        .codefile_validate(code_files)
        # copy them over
        purrr::map(code_files, function(x)
          file.copy(x, file.path(package_path, "data-raw")))
      }


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
