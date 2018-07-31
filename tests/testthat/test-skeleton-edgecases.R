context("datapackage_skeleton")
test_that("datapackage_skeleton errors with no name arg", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR")
  file2 <- system.file("extdata", "tests", "extra.rmd",
                       package = "DataPackageR")
  expect_error(
    datapackage_skeleton(
      name = NULL,
      path = tempdir(),
      code_files = c(file1, file2),
      force = TRUE,
      r_object_names = c("cars_over_20", "pressure")
    )
  )
})
