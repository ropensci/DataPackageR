testthat::test_that(
  "no phantom package loading from roxygenise() or associated warnings",
  {
    # test README.Rmd sequence that led to warnings
    processing_code <- system.file(
      "extdata", "tests", "subsetCars.Rmd", package = "DataPackageR"
    )
    pkg_name <- "mtcars20"
    # remove this directory on exit
    temp_dir <- withr::local_tempdir()
    pkg_path <- file.path(temp_dir, pkg_name)

    datapackage_skeleton(
      pkg_name, force = TRUE,
      code_files = processing_code,
      r_object_names = "cars_over_20",
      path = temp_dir)

    expect_no_warning(package_build(pkg_path, install = FALSE))
    # test phantom pkg loading side effect from roxygen2::roxygenise()
    expect_false(
      res1 <- pkg_name %in% devtools::package_info('attached')$package
    )

    # reset for next test
    if (res1) devtools::unload(pkg_name)

    # test phantom pkg loading side effect from roxygen2::roxygenise()
    # which is called by devtools::document()
    expect_no_warning(document(pkg_path, install = FALSE))
    expect_false(
      res2 <- pkg_name %in% devtools::package_info('attached')$package
    )

    # reset and verify attachment state
    if (res2) devtools::unload(pkg_name)
    expect_false(pkg_name %in% devtools::package_info('attached')$package)
  }
)
