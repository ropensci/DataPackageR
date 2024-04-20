test_that("Error on data object same name as data package", {
  file <- system.file("extdata", "tests", "extra.Rmd", package = "DataPackageR")
  td <- withr::local_tempdir()
  pp <- 'pressure'
  datapackage_skeleton(name = pp, path = td,
                       code_files = file, r_object_names = pp)
  err_msg <- "Data object not allowed to have same name as data package"
  expect_error(package_build(file.path(td, pp)), err_msg)
})
