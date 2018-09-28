context("R file script processing to vignette")
test_that("R file processing works and creates vignettes", {
  file <- system.file("extdata", "tests", "rfileTest.R",
    package = "DataPackageR"
  )
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
        file.path(tempdir(), "rfiletest"),
        install = TRUE
      )
    ),
    "rfiletest_1.0.tar.gz"
  )
  v <- vignette(package = "rfiletest")
  expect_equal(v$results[, "Item"], "rfileTest")
  expect_true(file_test("-f", file.path(tempdir(),"rfiletest","inst","doc","rfileTest.pdf")))
  unlink(file.path(tempdir(), "rfiletest"),
    recursive = TRUE,
    force = TRUE
  )

  file <- system.file("extdata", "tests", "rfileTest_noheader.R",
    package = "DataPackageR"
  )
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
        file.path(tempdir(), "rfiletest"),
        install = TRUE
      )
    ),
    "rfiletest_1.0.tar.gz"
  )
  v <- vignette(package = "rfiletest")
  expect_equal(v$results[, "Item"], "rfileTest_noheader")
  unlink(file.path(tempdir(), "rfiletest"),
    recursive = TRUE,
    force = TRUE
  )
  remove.packages("rfiletest")
})
