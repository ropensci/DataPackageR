

# function .doc_autogen() automates the creation of a basic roxygen template for the package and each object in objects_to_keep
# arguments are pname and ds2kp, normally defined in datasets.R
# pname is name of package, ds2kp is list of objects to save in data package
.doc_autogen <- function(pname, ds2kp, env, path, name = "documentation.R") {
  links <- c(pname, ds2kp)
  linksrox <- paste0("\\link{", links, "}")

  # create default file to be edited and
  # renamed manually by user, who then rebuilds package
  tempfilename <- file.path(path, name)
  if (file.exists(tempfilename)) {
    file.remove(tempfilename)
  }

  # create Roxygen documentation for data package
  con <- file(tempfilename, open = "w")
  writeLines(
    c(
      .rc(
        c(
          pname,
          paste0("A data package for ", pname, "."),
          "@docType package",
          paste0("@aliases ", pname, "-package"),
          "@title Package Title",
          paste0("@name ", pname),
          "@description A description of the data package",
          paste0(
            "@details Use \\code{",
            "data(package='", pname, "')$",
            "results[, 3]} to",
            "see a list of available",
            "data sets in this data package"
          ),
          "    and/or DataPackageR::load_all",
          "_datasets() to load them.",
          "@seealso",
          linksrox[2:length(links)]
        )
      ),
      "NULL\n\n\n"
    ), con
  )

  # Cycle through the rest of the files listed
  # in 'links' and create Roxygen
  # documentation for each one
  for (ds in links[2:length(links)]) {
    type <- class(get(ds, envir = env))[1]
    writeLines(
      .rc(c(
        "Detailed description of the data",
        paste("@name", ds),
        "@docType data",
        "@title Descriptive data title",
        paste0(
          "@format a \\code{", type,
          "} containing the following fields:"
        ),
        "\\describe{"
      )), con
    )
    # set up documentation template
    # for each field, using \item{varname}{}
    # with a blank description to fill in
    for (var in names(get(ds, envir = env))) {
      writeLines(
        .rc(paste0("\\item{", var, "}{}")),
        con
      )
    }
    writeLines(
      c(
        .rc(
          c(
            "}",
            paste0(
              "@source The data comes from",
              "________________________."
            ),
            "@seealso",
            # dataset being documented
            # should not list itself in its
            # seealsos
            linksrox[which(links != ds)]
          )
        ),
        "NULL\n\n\n"
      ), con
    )
  }
  close(con)
}
