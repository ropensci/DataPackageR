context("build documentation")
test_that("documentation is built via document()", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR")
  datapackage_skeleton(
    name = "subsetCars",
    path = tempdir(),
    code_files = c(file),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
      package_build(
        file.path(tempdir(), "subsetCars"))
  expect_true(document(file.path(tempdir(),"subsetCars")))
  v <- vignette(package = "subsetCars")
  expect_equal(v$results[,"Item"],"subsetCars")
  unlink(file.path(tempdir(), "subsetCars"),
         recursive = TRUE,
         force = TRUE)
})
