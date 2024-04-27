
context("documentation")
test_that("can_read_pkg_description,  data_version", {
  td <- withr::local_tempdir()
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.Rmd",
    package = "DataPackageR"
  )
  datapackage_skeleton(
    name = "subsetCars",
    path = td,
    code_files = c(file, file2),
    force = TRUE,
    r_object_names = c("cars_over_20", "pressure")
  )
  td_sc <- file.path(td, "subsetCars")
  # validate package description
  d <- desc::desc(td_sc)
  on.exit(pkgload::unload("subsetCars"))
  pkgload::load_all(td_sc)
  expected_version <-
    structure(list(c(0L, 1L, 0L)),
      class = c("package_version", "numeric_version")
    )
  expect_equal(data_version("subsetCars"), expected_version)
})
