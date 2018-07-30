
context("version string bump")
test_that("manual bump version when data unchanged", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR")
  file2 <- system.file("extdata", "tests", "extra.rmd",
                       package = "DataPackageR")
  expect_null(
    datapackage_skeleton(
      name = "subsetCars",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tempdir(), "subsetCars"))
  pkg <- desc::desc(file.path(tempdir(), "subsetCars"))
  pkg$set("DataVersion", "0.2.0")
  pkg$write()
  package_build(file.path(tempdir(), "subsetCars"))
  unlink(file.path(tempdir(), "subsetCars"),
         recursive = TRUE,
         force = TRUE)
})
