context("skeleton")
test_that("data, code, and dependencies are moved into place by skeleton", {
  file <- system.file("extdata", "tests", "extra.Rmd",
    package = "DataPackageR"
  )
  ancillary <- system.file("extdata", "tests", "rfileTest.R",
    package = "DataPackageR"
  )
  raw_data <- system.file("extdata", "tests", "raw_data",
    package = "DataPackageR"
  )
  expect_null(
    datapackage_skeleton(
      name = "datatest",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = "data",
      raw_data_dir = raw_data,
      dependencies = ancillary
    )
  )
  expect_true(
    file.exists(
      normalizePath(
        file.path(
          tempdir(),
          "datatest",
          "inst",
          "extdata",
          "raw_data",
          "testdata.csv"
        ),
        winslash = "/"
      )
    )
  )
  expect_true(
    file.exists(
      normalizePath(
        file.path(
          tempdir(),
          "datatest",
          "data-raw",
          "extra.Rmd"
        ),
        winslash = "/"
      )
    )
  )
  expect_true(
    file.exists(
      normalizePath(
        file.path(
          tempdir(),
          "datatest",
          "data-raw",
          "rfileTest.R"
        ),
        winslash = "/"
      )
    )
  )
  unlink(file.path(tempdir(), "datatest"),
    recursive = TRUE,
    force = TRUE
  )
  try(usethis::proj_set(NULL),silent = TRUE) #wrap in try for usethis 1.4 vs 1.5
})
