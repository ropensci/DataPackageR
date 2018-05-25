context("datapackage skeleton")

test_that("datapackage skeleton builds correct structure", {
  file = system.file("extdata","tests","subsetCars.Rmd",package="DataPackageR")
  expect_null(datapackage.skeleton(name="subsetCars",code_files = file, r_object_names = "cars_over_20"))
})
test_that("package can be built from different locations", {
    expect_equal(basename(buildDataSetPackage("subsetCars")),"subsetCars_1.0.tar.gz")
    setwd("subsetCars")
    expect_equal(basename(buildDataSetPackage(".")),"subsetCars_1.0.tar.gz")
    expect_error(buildDataSetPackage("subsetCars"))
    setwd("..")
})
unlink("subsetCars",recursive=TRUE)
unlink("subsetCars_1.0.tar.gz")