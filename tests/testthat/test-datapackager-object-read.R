context("datapackager_object_read")
test_that("data objects can be read across scripts", {
  ENVS <- new.env()
  dataenv <- new.env()
  assign("foo", 100, ENVS)
  assign("ENVS", ENVS, dataenv)
  assign("datapackager_object_read",
         datapackager_object_read,
         dataenv)
  expect_equal(eval(datapackager_object_read("foo"),
                    dataenv),
               100)
  expect_true(is.character(DataPackageR::project_path()))
  expect_true(is.character(DataPackageR::project_data_path()))
  expect_true(is.character(DataPackageR::project_extdata_path()))
})