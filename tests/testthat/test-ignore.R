context("ignore")

test_that("use_ignore works", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR"
  )
  expect_null(
    datapackage_skeleton(
      name = "subsetCars",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  expect_output(use_ignore(file = "mydata.csv",path = "inst/extdata"),"Adding 'mydata.csv' to 'inst/extdata/\\.gitignore'|Adding '\\^inst/extdata/mydata\\\\.csv\\$' to '\\.Rbuildignore'")
})
