
context("conditional build")
test_that("can add a data item", {

  
  library(testthat); library(DataPackageR)
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR"
  )
  
  expect_null(
    datapackage_skeleton(
      name = "testRSourcing",
      path = tempdir(),
      code_files = file,
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  
  package_build(file.path(tempdir(), "testRSourcing"))
  
  path_rmd <- paste0( tempdir(), "\\testRSourcing\\data-raw" )
  path_rmd <- normalizePath( path_rmd )
  wd_old <- getwd()
  setwd(path_rmd)
  fileConn<-file("depRmd.Rmd")
  writeLines(c("---\ntitle: Rmd to test loading of /R functions\n---\n`r test_func(3)`"), fileConn)
  close(fileConn)
  setwd(wd_old)
  
  path_rmd <- normalizePath( file.path( tempdir(), "testRSourcing", "data-raw", "depRmd.Rmd" ) )
  
  path_pkg <- normalizePath( file.path( tempdir(), "testRSourcing" ) )
  yml <- yml_find( path_pkg )
  yml <- yml_add_files( yml, "depRmd.Rmd" )
  yml_write( yml  )
  expect_error( package_build(file.path(tempdir(), "testRSourcing")))


  path_r <- paste0( tempdir(), "\\testRSourcing\\R" )
  path_r <- normalizePath( path_r )
  wd_old <- getwd()
  setwd(path_r)
  fileConn<-file("test_func.R")
  writeLines(c("test_func <- function(x) x^2"), fileConn)
  close(fileConn)
  setwd(wd_old)
  
  package_build(file.path(tempdir(), "testRSourcing"))
  path_rmd <- file.path( tempdir(), "testRSourcing", "inst", "doc" )
  path_rmd <- normalizePath( path_rmd )
  expect_true( "depRmd.html" %in%
                 list.files( path_rmd ) )
  expect_true( "depRmd.Rmd" %in%
                 list.files( path_rmd ) )

  unlink(file.path(tempdir(), "testRSourcing"),
         recursive = TRUE,
         force = TRUE
  )
  setwd(tempdir())
})
