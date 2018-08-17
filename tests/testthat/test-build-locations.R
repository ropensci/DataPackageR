context("building packages")
test_that(".onLoad sets options", {
  DataPackageR:::.onLoad()
  expect_true(getOption("DataPackageR_interact") == interactive())
  options("DataPackageR_interact" = FALSE)
})

test_that("package can be built from different locations", {
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
  expect_equal(
    basename(
      package_build(
        file.path(tempdir(), "subsetCars")
      )
    ),
    "subsetCars_1.0.tar.gz"
  )

  old <-
    setwd(file.path(tempdir(), "subsetCars")) #nolint
  on.exit(setwd(old)) #nolint
  expect_equal(basename(package_build(".")), "subsetCars_1.0.tar.gz")
  suppressWarnings(expect_error(package_build("subsetCars")))

  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})
