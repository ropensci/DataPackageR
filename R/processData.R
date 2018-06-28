#' DataPackageR
#'
#' Package to install the DataPackageR script that can be called via the R CMD mechanism.
#' R CMD DataPackageR packagename looks for .R files in 'data-raw' within the 'packagename' package source tree.
#' It sources a an .R file in 'data-raw' named 'datasets.R'. The 'datasets.R' file can source other files in '/data-raw'.
#' This must be done via a call like: \code{sys.source('myRfile.R',env=topenv())}.
#' The 'datasets.R' file and files it sources are expected to read raw data from
#' 'inst/extdata', or other sources, process them in some way, such that they are tidy and standardized.  The
#' objects remaining in the environment after the code is run are presumed to be the data sets that will be written to
#' '/data'. The user should also document these data sets
#' using 'roxygen2' in the .R files under the 'data-raw' directory. The package will extract documentation for the data objects it finds, as well as for
#' a data set with the name of the package, if present, and place it in the '/R' directory under the name 'packagename.R'. The 'DataPackageR' code will compare the digest of these data set objects against the contents of a 'DATADIGEST' file
#' in the package source tree (if present), and will also look for a 'DataVersion: x.y.z' string in the DESCRIPTION file of the
#' package. If the data have changed, the user will be warned that the DataVersion needs to be incemented. If no DATADIGEST file exists, one will be created.
#' If the DataVersion string has been incremented and the digest matches DATADIGEST (if it exists), or if the data hasn't changed and the DataVersion string
#' is unchanged, the code will write the data objects to '/data'. The user can then build the package with
#' R CMD build packagename.
#' @docType package
#' @name DataPackageR-package
NULL


.validate_render_root <- function(x) {
  # catch an error if it doesn't exist
  render_root <-
    try(normalizePath(x,
                      mustWork = TRUE,
                      winslash = "/"
    ), silent = TRUE)
  if (inherits(render_root, "try-error")) {
    flog.error(paste0("render_root  = ", x, " doesn't exist."))
    # try creating, even if it's an old temp dir.
    # This isn't ideal. Would like to rather say it's a temporary
    # directory and use the current one..
    return(FALSE)
  }
  return(TRUE)
}


#' Process data generation code in 'data-raw'
#'
#' Assumes .R files in 'data-raw' generate rda files to be stored in 'data'.
#' Sources datasets.R which can source other R files.
#' R files sourced by datasets.R must invoke \code{sys.source('myRfile.R',env=topenv())}.
#' Meant to be called before R CMD build.
#' @name DataPackageR
#' @param arg \code{character} name of the package to build.
#' @return logical TRUE if succesful, FALSE, if not.
#' @importFrom desc desc
#' @importFrom rmarkdown render
#' @importFrom utils getSrcref modifyList
#' @importFrom devtools document
#' @importFrom here here
#' @importFrom here set_here
#' @importFrom data.tree as.Node
DataPackageR <- function(arg = NULL) {
  requireNamespace("futile.logger")
  requireNamespace("yaml")
  old <- getwd()
  on.exit(setwd(old))
  pkg_dir <- arg
  pkg_dir <- normalizePath(pkg_dir, winslash = "/")
  raw_data_dir <- "data-raw"
  target <- normalizePath(file.path(pkg_dir, raw_data_dir), winslash = "/")
  data_dir <- normalizePath(file.path(pkg_dir, "data"), winslash = "/")
  raw_data_dir <- target

  # validate that render_root exists.
  # if it's an old temp dir, what then?
  
  if (!file.exists(target)) {
    flog.fatal(paste0("Directory ", target, " doesn't exist."))
    {
      stop("exiting", call. = FALSE)
    }
  } else {
    if (!file.exists(data_dir)) {
      dir.create(data_dir)
    }
    # get the current directory
    old <- setwd(pkg_dir) # TODO: ideally replace this soon
    on.exit(setwd(old))
    # log to the log file Create a log directory in inst/extdata
    logpath <-
      file.path(
        normalizePath("inst/extdata",
          winslash = "/"
        ),
        "Logfiles"
      )
    dir.create(logpath, recursive = TRUE, showWarnings = FALSE)
    # open a log file
    LOGFILE <- file.path(logpath, "processing.log")
    flog.appender(appender.tee(LOGFILE))
    flog.info(paste0("Logging to ", LOGFILE))
    # we know it's a proper package root, but we want to test if we have the
    # necessary subdirectories
    if (!all(file_test(c("R", "inst", "data", "data-raw"), op = "-d"))) {
      flog.fatal(paste0(
        "You need a valid package data strucutre.",
        " Missing ./R ./inst ./data or",
        "./data-raw subdirectories."
      ))
      {
        stop("exiting", call. = FALSE)
      }
    }
    flog.info("Processing data")
    # read YAML
    ymlfile <- dir(
      path = pkg_dir, pattern = "^datapackager.yml$",
      full.names = TRUE
    )
    if (length(ymlfile) == 0) {
      flog.fatal(paste0("Yaml configuration file not found at ", pkg_dir))
      {
        stop("exiting", call. = FALSE)
      }
    }
    ymlconf <- read_yaml(ymlfile)
    # test that the structure of the yaml file is correct!
    if (!"configuration" %in% names(ymlconf)) {
      flog.fatal("YAML is missing 'configuration:' entry")
      {
        stop("exiting", call. = FALSE)
      }
    }
    if (!all(c("files", "objects") %in%
      map(ymlconf, names)[["configuration"]])) {
      flog.fatal("YAML is missing files: and objects: entries")
      {
        stop("exiting", call. = FALSE)
      }
    }
    flog.info("Reading yaml configuration")
    # files that have enable: TRUE
    assert_that("configuration" %in% names(ymlconf))
    assert_that("files" %in% names(ymlconf[["configuration"]]))
    assert_that(!is.null(names(ymlconf[["configuration"]][["files"]])))

    r_files <- unique(names(
      Filter(
        x = ymlconf[["configuration"]][["files"]],
        f = function(x) x$enabled
      )
    ))
    if (length(r_files) == 0) {
      flog.fatal("No files enabled for processing!")
      {
        stop("error", call. = FALSE)
      }
    }
    objects_to_keep <- map(ymlconf, "objects")[["configuration"]]
    render_root <- .get_render_root(ymlconf)
    if (!.validate_render_root(render_root)) {
      flog.fatal(paste0(
        "Can't create, or render_root = ",
        render_root, " doesn't exist"
      ))
      stop("error", call. = FALSE)
    } else {
      render_root <- normalizePath(render_root, winslash = "/")
    }

    r_files <- file.path(raw_data_dir, r_files)
    if (all(!file.exists(r_files))) {
      flog.fatal(paste0("Can't find any R or Rmd files."))
      flog.fatal(paste0(
        "     Cant' find file: ",
        r_files[!file.exists(r_files)]
      ))
      stop("error", call. = FALSE)
    }
    if (any(!file.exists(r_files))) {
      flog.error(paste0(
        "Can't find ",
        r_files[!file.exists(r_files)]
      ))
      stop("error", call. = FALSE)
    }
    flog.info(paste0("Found ", r_files))
    # TODO fix hidden warnings in test cases
    old_data_digest <- .parse_data_digest()
    pkg_description <- try(read.description(file = "DESCRIPTION"),
      silent = TRUE
    )
    if (inherits(pkg_description, "try-error")) {
      flog.fatal("No valid DESCRIPTION file")
      {
        stop(
          paste0(
            "You need a valid package DESCRIPTION file.",
            "Please see Writing R Extensions",
            "(http://cran.r-project.org/doc/manuals/",
            "r-release/R-exts.html#The-DESCRIPTION-file).\n"
          ),
          pkg_description
        )
      }
    }
    # check that we have at least one file
    if (length(r_files) == 0) {
      flog.fatal("You must specify at least one file to process.")
      {
        stop("exiting", call. = FALSE)
      }
    }
    if (length(objects_to_keep) == 0) {
      flog.fatal("You must specify at least one data object.")
      {
        stop("exiting", call. = FALSE)
      }
    }
    # TODO Can we configure documentation in yaml?
    do_documentation <- FALSE
    # This flag indicates success
    can_write <- FALSE
    # environment for the data
    ENVS <- new.env(hash = TRUE, parent = .GlobalEnv)
    for (i in seq_along(r_files)) {
      dataenv <- new.env(hash = TRUE, parent = .GlobalEnv)
      # assign ENVS into dataenv.
      # provide functions in the package to read from it.
      assign(x = "ENVS", value = ENVS, dataenv)
      flog.info(paste0(
        "Processing ", i, " of ",
        length(r_files), ": ", r_files[i],
        "\n"
      ))
      # config file goes in the root render the r and rmd files
      render(
        input = r_files[i], envir = dataenv,
        output_dir = logpath, clean = FALSE, knit_root_dir = render_root
      )
      # The created objects
      object_names <- ls(dataenv)
      flog.info(paste0(
        sum(objects_to_keep %in% object_names),
        " required data objects created by ",
        basename(r_files[i])
      ))
      if (sum(objects_to_keep %in% object_names) > 0) {
        for (o in objects_to_keep[objects_to_keep %in% object_names]) {
          assign(o, get(o, dataenv), ENVS)
        }
      }
    }
    # currently environments for each file are independent.
    dataenv <- ENVS
    # Digest each object
    new_data_digest <- .digest_data_env(ls(ENVS), dataenv, pkg_description)
    if (!is.null(old_data_digest)) {
      string_check <- .check_dataversion_string(
        old_data_digest,
        new_data_digest
      )
      can_write <- FALSE
      stopifnot(!((!.compare_digests(
        old_data_digest,
        new_data_digest
      )) & string_check$isgreater))
      if (.compare_digests(
        old_data_digest,
        new_data_digest
      ) &
        string_check$isequal) {
        can_write <- TRUE
        flog.info(paste0(
          "Processed data sets match ",
          "existing data sets at version ",
          new_data_digest[["DataVersion"]]
        ))
      } else if ((!.compare_digests(
        old_data_digest,
        new_data_digest
      )) &
        string_check$isequal) {
        updated_version <- .increment_data_version(
          pkg_description,
          new_data_digest
        )
        pkg_description <- updated_version$pkg_description
        new_data_digest <- updated_version$new_data_digest
        can_write <- TRUE
        flog.info(paste0(
          "Data has been updated and DataVersion ",
          "string incremented automatically to ",
          new_data_digest[["DataVersion"]]
        ))
      } else if (.compare_digests(
        old_data_digest,
        new_data_digest
      ) &
        string_check$isgreater) {
        # edge case that shouldn't happen
        # but we test for it in the test suite
        can_write <- TRUE
        flog.info(paste0(
          "Data hasn't changed but the ",
          "DataVersion has been bumped."
        ))
      } else if (string_check$isless & .compare_digests(
        old_data_digest,
        new_data_digest
      )) {
        # edge case that shouldn't happen but
        # we test for it in the test suite.
        flog.info(paste0(
          "New DataVersion is less than ",
          "old but data are unchanged"
        ))
        new_data_digest <- old_data_digest
        pkg_description[["DataVersion"]] <- new_data_digest[["DataVersion"]]
        can_write <- TRUE
      } else if (string_check$isless & !.compare_digests(
        old_data_digest,
        new_data_digest
      )) {
        updated_version <- .increment_data_version(
          pkg_description,
          new_data_digest
        )
        pkg_description <- updated_version$pkg_description
        new_data_digest <- updated_version$new_data_digest
        can_write <- TRUE
      }
      if (can_write) {
        .save_data(new_data_digest,
          pkg_description,
          ls(dataenv),
          dataenv,
          old_data_digest = old_data_digest
        )
        do_documentation <- TRUE
      }
    } else {
      .save_data(new_data_digest,
        pkg_description,
        ls(dataenv),
        dataenv,
        old_data_digest = NULL
      )
      do_documentation <- TRUE
    }
    if (do_documentation) {
      # Run .doc_autogen #needs to be run when we have a partial build..
      if (!file.exists(file.path(target, "documentation.R"))) {
        .doc_autogen(basename(pkg_dir),
          ds2kp = ls(dataenv),
          env = dataenv,
          path = target
        )
      }
      # parse documentation
      doc_parsed <- .doc_parse(file.path(target, "documentation.R"))
      .identify_missing_docs <- function(environment = NULL,
                                               description = NULL,
                                               docs = NULL) {
        setdiff(
          ls(environment),
          setdiff(
            names(docs),
            description[["Package"]]
          )
        )
      }
      # case where we add an object,
      # ensures we combine the documentation properly
      missing_doc_for_autodoc <- .identify_missing_docs(
        dataenv,
        pkg_description,
        doc_parsed
      )
      if (length(missing_doc_for_autodoc) != 0) {
        tmptarget <- tempdir()
        file.info("Writing missing docs.")
        .doc_autogen(basename(pkg_dir),
          ds2kp = missing_doc_for_autodoc,
          env = dataenv,
          path = tmptarget,
          name = "missing_doc.R"
        )
        missing_doc <- .doc_parse(file.path(tmptarget, "missing_doc.R"))
        doc_parsed <- .doc_merge(
          old = doc_parsed,
          new = missing_doc
        )
        file.info("Writing merged docs.")
        docfile <- file(
          file.path(
            target,
            paste0("documentation", ".R")
          ),
          open = "w"
        )
        for (i in seq_along(doc_parsed)) {
          writeLines(text = doc_parsed[[i]], con = docfile)
        }
      }
      # Partial build if enabled=FALSE for
      # any file We've disabled an object but don't
      # want to overwrite its documentation
      # or remove it The new approach just builds
      # all the docs independent of what's enabled.
      save_docs <- do.call(c, doc_parsed)
      docfile <- file(file.path("R",
        pattern = paste0(pkg_description$Package, ".R")
      ),
      open = "w"
      )
      for (i in seq_along(save_docs)) {
        writeLines(text = save_docs[[i]], con = docfile)
      }
      close(docfile)
      flog.info(
        paste0(
          "Copied documentation to ",
          file.path("R", paste0(pkg_description$Package, ".R"))
        )
      )
      # TODO test that we have documented
      # everything successfully and that all files
      # have been parsed successfully
      can_write <- TRUE
    }
    eval(expr = expression(rm(list = ls())), envir = dataenv)
    # copy html files to vignettes
    .ppfiles_mkvignettes(dir = pkg_dir)
  }
  flog.info("Done")
  return(can_write)
}


.ppfiles_mkvignettes <- function(dir = NULL) {
  pkg <- as.package(dir)
  # check_suggested("rmarkdown")
  add_desc_package(pkg, "Suggests", "knitr")
  add_desc_package(pkg, "Suggests", "rmarkdown")
  add_desc_package(pkg, "VignetteBuilder", "knitr")
  use_directory("vignettes", pkg = pkg)
  use_git_ignore("inst/doc", pkg = pkg)
  lines <- readLines(".gitignore")
  lines <- gsub("inst/doc", "", lines)
  writeLines(lines, ".gitignore")
  try(dir.create(file.path(pkg$path, "inst/doc"),
    showWarnings = FALSE
  ),
  silent = TRUE
  )
  # TODO maybe copy only the files that have both html and Rmd.
  rmdfiles_for_vignettes <-
    list.files(
      path = file.path(pkg$path, "data-raw"),
      pattern = "Rmd$",
      full.names = TRUE,
      recursive = FALSE
    )
  htmlfiles_for_vignettes <-
    list.files(
      path = file.path(pkg$path, "inst/extdata/Logfiles"),
      pattern = "html$",
      full.names = TRUE,
      recursive = FALSE
    )
  purrr::map(
    htmlfiles_for_vignettes,
    function(x) {
      file.copy(x,
        file.path(
          pkg$path,
          "inst/doc",
          basename(x)
        ),
        overwrite = TRUE
      )
    }
  )
  capture.output(purrr::map(
    rmdfiles_for_vignettes,
    function(x) {
      file.copy(x,
        file.path(
          pkg$path,
          "vignettes",
          basename(x)
        ),
        overwrite = TRUE
      )
    }
  ))
  vignettes_to_process <- list.files(
    path = file.path(pkg$path, "vignettes"),
    pattern = "Rmd$",
    full.names = TRUE,
    recursive = FALSE
  )
  write_me_out <- purrr::map(vignettes_to_process, function(x) {
    title <- "Default Vignette Title. Add yaml title: to your document"
    thisfile <- read_file(x)
    stripped_yaml <- gsub("---\\s*\n.*\n---\\s*\n", "", thisfile)
    frontmatter <- gsub("(---\\s*\n.*\n---\\s*\n).*", "\\1", thisfile)
    con <- textConnection(frontmatter)
    fm <- rmarkdown::yaml_front_matter(con)
    if (is.null(fm[["vignette"]])) {
      # add boilerplate vignette yaml
      if (!is.null(fm$title)) {
        title <- fm$title
      }
      fm$vignette <- paste0("%\\VignetteIndexEntry{", title, "}
                           %\\VignetteEngine{knitr::rmarkdown}
                           \\usepackage[utf8]{inputenc}")
    } else {
      # otherwise leave it as is.
    }
    tmp <- fm$vignette
    tmp <- gsub(
      "  $",
      "",
      paste0(
        "vignette: >\n  ",
        gsub(
          "\\}\\s*",
          "\\}\n  ",
          tmp
        )
      )
    )
    fm$vignette <- NULL
    write_me_out <- paste0(
      "---\n",
      paste0(yaml::as.yaml(fm), tmp),
      "---\n\n",
      stripped_yaml
    )
    write_me_out
  })
  names(write_me_out) <- vignettes_to_process
  for (i in vignettes_to_process) {
    writeLines(write_me_out[[i]], con = i)
    writeLines(write_me_out[[i]],
      con = file.path(
        pkg$path,
        "inst/doc",
        basename(i)
      )
    )
  }
}
