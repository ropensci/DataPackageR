context("ignore")

test_that("use_ignore works", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR"
  )
  local({
    tempdir <- withr::local_tempdir()
    expect_null(
      datapackage_skeleton(
        name = "subsetCars",
        path = tempdir,
        code_files = c(file),
        force = TRUE,
        r_object_names = c("cars_over_20")
      )
    )
    use_ignore(file = "mydata.csv", path = file.path("inst", "extdata"))
    expect_true(
      'mydata.csv' %in% readLines(
        file.path(tempdir, 'subsetCars', 'inst', 'extdata', '.gitignore')
      )
    )
    expect_true(
      '^inst/extdata/mydata\\.csv$' %in% readLines(
        file.path(tempdir, 'subsetCars', '.Rbuildignore')
      )
    )
    expect_message(use_ignore(),"No file name provided to ignore.")
  })
})
