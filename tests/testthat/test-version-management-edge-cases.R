
context("Data Version management")
test_that("data changes but version out of sync", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.Rmd",
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
  package_build(file.path(tempdir(), "subsetCars"))
  news_lines <- readLines(file.path(tempdir(), "subsetCars","NEWS.md"))
  expect_true(sum(grepl("Added: cars_over_20", news_lines)) == 1)
  config <- yml_find(file.path(tempdir(), "subsetCars"))
  config <- yml_add_files(config, "extra.Rmd")
  config <- yml_add_objects(config, "pressure")
  file.copy(file2, file.path(tempdir(), "subsetCars", "data-raw"))
  yml_write(config)
  package_build(file.path(tempdir(), "subsetCars"))
  news_lines <- readLines(file.path(tempdir(), "subsetCars","NEWS.md"))
  expect_false(any(grepl("Changed: cars_over_20", news_lines)))
  expect_false(any(grepl("Deleted: cars_over_20", news_lines)))
  expect_true(sum(grepl("Added: pressure", news_lines)) == 1)
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})
