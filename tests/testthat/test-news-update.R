context("news file")
test_that("news file is created", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  datapackage_skeleton(
    name = "subsetCars",
    path = tempdir(),
    code_files = c(file),
    force = TRUE,
    r_object_names = c("cars_over_20")
  )
  options(DataPackageR_interact = FALSE)
  package_build(file.path(tempdir(), "subsetCars"))
  expect_equal(readLines(file.path(tempdir(), "subsetCars", "NEWS.md"))[3], "Package built in non-interactive mode") # nolint
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
  expect_equal(DataPackageR:::.prompt_user_for_change_description(interact = FALSE), "Package built in non-interactive mode") # nolint
})
