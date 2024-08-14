context("project paths")
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
package_build(file.path(tempdir(), "subsetCars"))
usethis::proj_set(file.path(tempdir(), "subsetCars"))
test_that("path functions throw no warning when file does not exist", {
  expect_no_warning(project_path('zzzZZZ333.txt'))
  expect_no_warning(project_extdata_path('zzzZZZ333.txt'))
  expect_no_warning(project_data_path('zzzZZZ333.txt'))
})
test_that("project_path works with file arguments", {
  expect_equal(project_path("DESCRIPTION"), expected = file.path(usethis::proj_get(), "DESCRIPTION")) # nolint
})
test_that("project_data_path works with file arguments", {
  expect_equal(project_data_path("cars_over_20.rda"), expected = file.path(usethis::proj_get(), "data", "cars_over_20.rda")) # nolint
})
test_that("project_extdata_path works with file arguments", {
  expect_equal(project_extdata_path("Logfiles/processing.log"), expected = file.path(usethis::proj_get(), "inst", "extdata", "Logfiles", "processing.log")) # nolint
})
unlink(file.path(tempdir(), "subsetCars"),
  recursive = TRUE,
  force = TRUE
)
try(usethis::proj_set(path = NULL), silent = TRUE) #compatibility between usethis 1.4 and 1.5

