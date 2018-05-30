context("datapackage skeleton")

test_that("datapackage skeleton builds correct structure", {
  file = system.file("extdata","tests","subsetCars.Rmd",package="DataPackageR")

  tmp <<- tempdir()
  tmp <<- normalizePath(tmp)
  expect_null(datapackage.skeleton(name="subsetCars",path = tmp,code_files = c(file), force=TRUE, r_object_names = "cars_over_20"))
})
context("building")
test_that("package can be built from different locations", {
    expect_equal(basename(buildDataSetPackage(file.path(tmp,"subsetCars"))),"subsetCars_1.0.tar.gz")
    old = getwd()
    setwd(file.path(tmp,"subsetCars"))
    expect_equal(basename(buildDataSetPackage(".")),"subsetCars_1.0.tar.gz")
    expect_error(buildDataSetPackage("subsetCars"))
    setwd(old)
})

context("yaml config")
test_that("yaml reading, adding, removing, listing, and writing", {
  file = system.file("extdata","tests","subsetCars.Rmd",package="DataPackageR")
  tmp <<- tempdir()
  tmp <<- normalizePath(tmp)
  expect_null(datapackage.skeleton(name="subsetCars",path = tmp,code_files = c(file), force=TRUE, r_object_names = "cars_over_20"))
  
  test_config <-
    structure(list(configuration = list(files = list(subsetCars.Rmd = list(
      name = "subsetCars.Rmd", enabled = TRUE)), objects = "cars_over_20")))
  config = yml_find(file.path(tmp,"subsetCars"))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
  config = yml_add_files(config,"foo_file")
  test_config <-
    structure(list(configuration = list(files = list(subsetCars.Rmd = list(
      name = "subsetCars.Rmd", enabled = TRUE), foo_file = list(
        name = "foo_file", enabled = TRUE)), objects = "cars_over_20")))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
  config = yml_remove_files(config,"foo_file")
  test_config <-
    structure(list(configuration = list(files = list(subsetCars.Rmd = list(
      name = "subsetCars.Rmd", enabled = TRUE)), objects = "cars_over_20")))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
  
  config = yml_add_objects(config,"foo_obj")
  test_config <-
    structure(list(configuration = list(files = list(subsetCars.Rmd = list(
      name = "subsetCars.Rmd", enabled = TRUE)), objects = c("cars_over_20", 
                                                             "foo_obj"))))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
  
  config = yml_remove_objects(config,"foo_obj")
  test_config <-
    structure(list(configuration = list(files = list(subsetCars.Rmd = list(
      name = "subsetCars.Rmd", enabled = TRUE)), objects = "cars_over_20")))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
  
  config = yml_list_files(config)
  test_config <-structure(list(configuration = list(files = list(subsetCars.Rmd = list(
    name = "subsetCars.Rmd", enabled = TRUE)), objects = "cars_over_20")))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
  
  
  config = yml_list_objects(config)
  test_config <-structure(list(configuration = list(files = list(subsetCars.Rmd = list(
    name = "subsetCars.Rmd", enabled = TRUE)), objects = "cars_over_20")))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
  #still the same after writing?
  yml_write(config)
  test_config <-
    structure(list(configuration = list(files = list(subsetCars.Rmd = list(
      name = "subsetCars.Rmd", enabled = TRUE)), objects = "cars_over_20")))
  config = yml_find(file.path(tmp,"subsetCars"))
  attr(test_config,"path") <- attr(config,"path")
  expect_identical(config,test_config)
  
})

context("conditional build")
test_that("conditional build works as expected", {
  file = system.file("extdata","tests","subsetCars.Rmd",package="DataPackageR")
  file2 = system.file("extdata","tests","extra.rmd",package="DataPackageR")
  
  tmp <<- tempdir()
  tmp <<- normalizePath(tmp)
  expect_null(datapackage.skeleton(name="subsetCars",path = tmp,code_files = c(file,file2), force=TRUE, r_object_names = c("cars_over_20")))
  buildDataSetPackage(file.path(tmp,"subsetCars"))
  expect_equal(list.files(file.path(tmp,"subsetCars","data")),"cars_over_20.rda")
  expect_true(all(c("subsetCars","cars_over_20")%in%names(DataPackageR:::.parseDocumentation(list.files(file.path(tmp,"subsetCars","R"),full=TRUE)))))
  config = yml_find(file.path(tmp,"subsetCars"))
  config = yml_add_objects(config,"pressure")
  yml_write(config)
  buildDataSetPackage(file.path(tmp,"subsetCars"))
  # have we saved the new object?
  expect_equal(list.files(file.path(tmp,"subsetCars","data")),c("cars_over_20.rda","pressure.rda"))
  #has the documentation been built for the new object?
  expect_true(all(c("subsetCars","cars_over_20", "pressure")%in%names(DataPackageR:::.parseDocumentation(list.files(file.path(tmp,"subsetCars","R"),full=TRUE)))))
  
  config = yml_disable_compile(config,basename(file2))
  yml_write(config)
  buildDataSetPackage(file.path(tmp,"subsetCars"))
  expect_equal(list.files(file.path(tmp,"subsetCars","data")),c("cars_over_20.rda","pressure.rda"))
  expect_true(all(c("subsetCars","cars_over_20", "pressure")%in%names(DataPackageR:::.parseDocumentation(list.files(file.path(tmp,"subsetCars","R"),full=TRUE)))))
})
  