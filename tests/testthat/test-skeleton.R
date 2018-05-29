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

test_that("yaml reading, adding, removing, listing, and writing", {
  test_config <-
    structure(list(configuration = list(files = "subsetCars.Rmd", objects = "cars_over_20")),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  config = yml_find("subsetCars")
  expect_identical(config,test_config)
  
  config = yml_add_files(config,"foo_file")
  test_config <-
    structure(list(configuration = list(files = c("subsetCars.Rmd", "foo_file"
    ), objects = "cars_over_20")),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  expect_identical(config,test_config)
  
  config = yml_remove_files(config,"foo_file")
  test_config <-
    structure(list(configuration = list(files = "subsetCars.Rmd", objects = "cars_over_20")),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  expect_identical(config,test_config)
  
  
  config = yml_add_objects(config,"foo_obj")
  test_config <-
    structure(list(configuration = list(files = "subsetCars.Rmd", objects = c("cars_over_20", 
                                                                    "foo_obj"))),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  expect_identical(config,test_config)
  
  
  config = yml_remove_objects(config,"foo_obj")
  test_config <-
    structure(list(configuration = list(files = "subsetCars.Rmd", objects = "cars_over_20")),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  expect_identical(config,test_config)
  
  
  config = yml_list_files(config)
  test_config <-
    structure(list(configuration = list(files = "subsetCars.Rmd", objects = "cars_over_20")),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  expect_identical(config,test_config)
  
  
  
  config = yml_list_objects(config)
  test_config <-
    structure(list(configuration = list(files = "subsetCars.Rmd", objects = "cars_over_20")),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  expect_identical(config,test_config)
  
  #still the same after writing?
  yml_write(config)
  test_config <-
    structure(list(configuration = list(files = "subsetCars.Rmd", objects = "cars_over_20")),path = "/Users/gfinak/Documents/Projects/DataPackageR/tests/testthat/subsetCars/datapackager.yml")
  config = yml_find("subsetCars")
  expect_identical(config,test_config)
  
})

unlink("subsetCars",recursive=TRUE)
unlink("subsetCars_1.0.tar.gz")
