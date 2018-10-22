context("datapackager_object_read")
test_that("data objects can be read across scripts", {
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
  expect_true(is.character(DataPackageR::project_path()))
  expect_true(is.character(DataPackageR::project_data_path()))
  expect_true(is.character(DataPackageR::project_extdata_path()))
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
  expect_true(file_test("-f",file.path(tempdir(),DataPackageR::yml_find(file.path(tempdir(),"subsetCars"))[["configuration"]][["render_root"]][["tmp"]],"cars_over_20.rds")))
})
