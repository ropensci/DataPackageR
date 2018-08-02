
context("documentation")
test_that("can_read_pkg_description,  data_version", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  datapackage_skeleton(
    name = "subsetCars",
    path = tempdir(),
    code_files = c(file, file2),
    force = TRUE,
    r_object_names = c("cars_over_20", "pressure")
  )
  DataPackageR:::read_pkg_description(file.path(tempdir(), "subsetCars"))
  devtools::load_all(file.path(tempdir(), "subsetCars"))
  expected_version <-
    structure(list(c(0L, 1L, 0L)),
      class = c("package_version", "numeric_version")
    )
  expect_equal(data_version("subsetCars"), expected_version)
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})
