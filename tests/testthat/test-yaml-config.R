context("conditional build")
test_that("can add a file", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR")
  file2 <- system.file("extdata", "tests", "extra.rmd",
                       package = "DataPackageR")
  expect_null(
    datapackage_skeleton(
      name = "subsetCars",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tempdir(), "subsetCars"))
  expect_equal(list.files(file.path(tempdir(), "subsetCars", "data")),
               "cars_over_20.rda")
  expect_true(all(
    c("subsetCars", "cars_over_20") %in%
      names(DataPackageR:::.doc_parse(
        list.files(file.path(tempdir(), "subsetCars", "R"), full.names = TRUE)
      ))
  ))
  config <- yml_find(file.path(tempdir(), "subsetCars"))
  config <- yml_add_files(config, "extra.rmd")
  yml_write(config)
  file.copy(from = file2, file.path(tempdir(), "subsetCars", "data-raw"))
  expect_equal(basename(package_build(file.path(tempdir(),
                                                "subsetCars"))),
               "subsetCars_1.0.tar.gz")
  expect_equal(names(DataPackageR:::.doc_parse(
    list.files(file.path(tempdir(),
                         "subsetCars",
                         "R"),
               full.names = TRUE)
  )),
  c("subsetCars", "cars_over_20"))
  config <- yml_add_objects(config, "pressure")
  yml_write(config)
  expect_equal(basename(package_build(file.path(tempdir(),
                                                "subsetCars"))),
               "subsetCars_1.0.tar.gz")
  expect_equal(names(DataPackageR:::.doc_parse(
    list.files(file.path(tempdir(),
                         "subsetCars",
                         "R"),
               full.names = TRUE)
  )),
  c("subsetCars", "cars_over_20", "pressure"))
  expect_equal(basename(list.files(
    file.path(tempdir(), "subsetCars", "data"),
    full.names = TRUE
  )),
  c("cars_over_20.rda", "pressure.rda"))
  unlink(file.path(tempdir(), "subsetCars"),
         recursive = TRUE,
         force = TRUE)
})
