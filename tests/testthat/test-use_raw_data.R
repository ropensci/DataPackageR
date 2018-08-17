context("test-use_raw_data")

test_that("use_raw_data works as expected", {
  myfile <- tempfile()
  file <- system.file("extdata", "tests", "extra.rmd",
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
  file.create(myfile)
  unlink(file.path(tempdir(), "datatest", "inst"),
         force = TRUE,
         recursive = TRUE)
  expect_error(use_raw_dataset(myfile))
  dir.create(file.path(tempdir(),
                       "datatest", "inst", "extdata"), 
             recursive = TRUE)
  expect_true(use_raw_dataset(myfile))
  expect_true(file_test(
    "-f",
    file.path(tempdir(), "datatest", "inst", "extdata", 
              basename(myfile))
  ))
  expect_error(use_raw_dataset())
  expect_error(suppressWarnings(use_raw_dataset("foobar")))
  expect_true(use_raw_dataset(file.path(tempdir(), "datatest", "R")))
})
