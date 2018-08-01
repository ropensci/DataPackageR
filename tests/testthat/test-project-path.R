context("project paths")
file <- system.file("extdata", "tests", "subsetCars.Rmd",
                    package = "DataPackageR")
datapackage_skeleton(
  name = "subsetCars",
  path = tempdir(),
  code_files = c(file),
  force = TRUE,
  r_object_names = c("cars_over_20")
)
package_build(file.path(tempdir(), "subsetCars"))
usethis::proj_set(file.path(tempdir(), "subsetCars"))
test_that("project_path works with file arguments", {
  expect_equal(project_path("foo.Rmd"),expected = file.path(usethis::proj_get(),"foo.Rmd")) #nolint
})
test_that("project_data_path works with file arguments", {
  expect_equal(project_data_path("foo.Rmd"),expected = file.path(usethis::proj_get(),"data","foo.Rmd")) #nolint
})
test_that("project_extdata_path works with file arguments", {
  expect_equal(project_extdata_path("foo.Rmd"),expected = file.path(usethis::proj_get(),"inst","extdata","foo.Rmd")) #nolint
})
unlink(file.path(tempdir(), "subsetCars"),
       recursive = TRUE,
       force = TRUE)
