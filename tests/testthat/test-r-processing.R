context("R file script processing to vignette")
test_that("R file processing works", {
  file <- system.file("extdata", "tests", "rfileTest.R",
                      package = "DataPackageR")
  datapackage_skeleton(
    name = "rfiletest",
    path = tempdir(),
    code_files = c(file),
    force = TRUE,
    r_object_names = "data"
  )
  expect_equal(
    basename(
      package_build(
        file.path(tempdir(), "rfiletest"))),
    "rfiletest_1.0.tar.gz")
  v <- vignette(package="rfiletest")
  expect_equal(v$results[,"Item"],"rfileTest")
  
  remove.packages("rfiletest")  
  unlink(file.path(tempdir(), "rfiletest"),
         recursive = TRUE,
         force = TRUE)
})