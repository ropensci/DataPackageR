context("yaml config manipulation")
test_that("can remove a data item", {
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
      r_object_names = c("cars_over_20", "pressure")
    )
  )
  package_build(file.path(tempdir(), "subsetCars"))
  # have we saved the new object?
  config <- yml_find(file.path(tempdir(), "subsetCars"))
  config <- yml_disable_compile(config, basename(file2))
  yml_write(config)
  package_build(file.path(tempdir(), "subsetCars"))
  expect_equal(
    list.files(file.path(tempdir(), "subsetCars", "data")),
    c("cars_over_20.rda", "pressure.rda")
  )
  expect_true(all(
    c("subsetCars", "cars_over_20", "pressure") %in%
      names(DataPackageR:::.doc_parse(
        list.files(file.path(tempdir(), "subsetCars", "R"),
          full.names = TRUE
        )
      ))
  ))
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})
