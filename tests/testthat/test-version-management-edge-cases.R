
context("Data Version management")
test_that("data changes but version out of sync", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
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
  config <- yml_find(file.path(tempdir(), "subsetCars"))
  config <- yml_add_files(config, "extra.rmd")
  config <- yml_add_objects(config, "pressure")
  file.copy(file2, file.path(tempdir(), "subsetCars", "data-raw"))
  yml_write(config)
  pkg <- desc::desc(file.path(tempdir(), "subsetCars"))
  pkg$set("DataVersion", "0.0.0")
  pkg$write()
  package_build(file.path(tempdir(), "subsetCars"))
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})
