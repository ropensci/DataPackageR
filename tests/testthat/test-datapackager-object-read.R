context("datapackager_object_read")
test_that("data objects can be read across scripts", {
  # file <- system.file("extdata", "tests", "subsetCars.Rmd",
  #                     package = "DataPackageR"
  # )
  # datapackage_skeleton(
  #   name = "subsetCars",
  #   path = tempdir(),
  #   code_files = c(file),
  #   force = TRUE,
  #   r_object_names = "cars_over_20"
  # )
  #
  ENVS <- new.env()
  dataenv <- new.env()
  assign("foo", 100, ENVS)
  assign("ENVS", ENVS, dataenv)
  assign(
    "datapackager_object_read",
    datapackager_object_read,
    dataenv
  )
  expect_equal(
    eval(
      datapackager_object_read("foo"),
      dataenv
    ),
    100
  )
  # expect_true(is.character(DataPackageR::project_path()))
  # expect_true(is.character(DataPackageR::project_data_path()))
  # expect_true(is.character(DataPackageR::project_extdata_path()))
})


test_that("data objects are saved incrementally in render_root", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR"
  )
  datapackage_skeleton(
    name = "subsetCars",
    path = tempdir(),
    code_files = c(file),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  package_build(
    file.path(tempdir(), "subsetCars")
  )
  expect_true(utils::file_test("-f",file.path(tempdir(),DataPackageR::yml_find(file.path(tempdir(),"subsetCars"))[["configuration"]][["render_root"]][["tmp"]],"cars_over_20.rds")))
})


test_that("data objects can be read from render_root or the data dir", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR"
  )
  datapackage_skeleton(
    name = "subsetCars",
    path = tempdir(),
    code_files = c(file),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  package_build(
    file.path(tempdir(), "subsetCars")
  )

  #create object that doesn't exist in temporary file, so datapackager_object_read is forced to look in the data dir
  file.copy(file.path(tempdir(),"subsetCars","data","cars_over_20.rda"),
            file.path(tempdir(), "subsetCars","data","cars_over_20_2.rda"))

  original<-readRDS(file.path(tempdir(),DataPackageR::yml_find(file.path(tempdir(),"subsetCars"))[["configuration"]][["render_root"]][["tmp"]],"cars_over_20.rds"))

  expect_identical(suppressMessages(datapackager_object_read("cars_over_20")),original)
  expect_identical(datapackager_object_read("cars_over_20_2"),original)

  #check if the reading will try to read from the ENV
  options("DataPackageR_packagebuilding" = TRUE)
  on.exit({options("DataPackageR_packagebuilding" = FALSE)})

  expect_error(datapackager_object_read("cars_over_20"))

})
