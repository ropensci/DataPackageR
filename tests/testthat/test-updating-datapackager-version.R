context("updating datapackager API version")
test_that("can update", {

  #setup, build example package
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
                       package = "DataPackageR"
  )
  expect_null(
    datapackage_skeleton(
      name = "subsetCars",
      path = tempdir(),
      code_files = c(file, file2),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tempdir(), "subsetCars"))

  #remove news.md and modify with the digest so it thinks there has been an update when rebuilt
  file.remove(file.path(tempdir(),"subsetCars","news.md"))
  oldDigest<-DataPackageR:::.parse_data_digest(file.path(tempdir(),"subsetCars"))
  oldDigest$cars_over_20<-"123456789"
  DataPackageR:::.save_digest(oldDigest,file.path(tempdir(),"subsetCars"))

  expect_identical(
    try(package_build(file.path(tempdir(), "subsetCars"))),
    normalizePath(file.path(tempdir(),"subsetCars_1.0.tar.gz"),winslash = "/")
  )#if it passes, it returns the path to the tar file?

})
