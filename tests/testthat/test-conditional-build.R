
context("conditional build")
test_that("can add a data item", {
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
      code_files = c(file, file2),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tempdir(), "subsetCars"))
  expect_equal(
    list.files(file.path(tempdir(), "subsetCars", "data")),
    "cars_over_20.rda"
  )
  expect_true(all(
    c("subsetCars", "cars_over_20") %in%
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
  setwd(tempdir())
  try(usethis::proj_set(NULL),silent = TRUE) #wrap in try for usethis 1.4 vs 1.5
})
