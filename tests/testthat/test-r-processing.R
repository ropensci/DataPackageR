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
  
  temp_libpath <- file.path(tempdir(),"lib")
  dir.create(temp_libpath)
  
  expect_equal(
    basename(package_build(
      file.path(tempdir(), "rfiletest"),
      install = TRUE,
      lib = temp_libpath
    )),
    "rfiletest_1.0.tar.gz"
  )
  
  v <- vignette(package = "rfiletest", lib.loc = temp_libpath)
  
  expect_equal(v$results[, "Item"], "rfileTest")
  expect_true(file_test("-f", file.path(tempdir(),"rfiletest","inst","doc","rfileTest.pdf")))
  unlink(file.path(tempdir(), "rfiletest"),
    recursive = TRUE,
    force = TRUE
  )
  remove.packages("rfiletest", lib = temp_libpath)
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
  dir.create(temp_libpath)
  expect_equal(
    basename(
      package_build(
        file.path(tempdir(), "rfiletest"),
        install = TRUE,
        lib = temp_libpath
      )
    ),
    "rfiletest_1.0.tar.gz"
  )
  v <- vignette(package = "rfiletest", lib.loc = temp_libpath)
  expect_equal(v$results[, "Item"], "rfileTest_noheader")
  unlink(file.path(tempdir(), "rfiletest"),
    recursive = TRUE,
    force = TRUE
  )
  try(usethis::proj_set(NULL),silent = TRUE) #wrap in try for usethis 1.4 vs 1.5
  remove.packages("rfiletest",lib = temp_libpath)
})
