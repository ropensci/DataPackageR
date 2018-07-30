context("datapackage skeleton")

test_that("datapackage skeleton builds correct structure", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR")
  # normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  
  expect_null(
    datapackage_skeleton(
      name = "subsetCars",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = "cars_over_20"
    )
  )
  unlink(file.path(tempdir(), "subsetCars"),
         recursive = TRUE,
         force = TRUE)
})


