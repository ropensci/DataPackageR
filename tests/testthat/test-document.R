context("build documentation")
test_that("documentation is built via document()", {
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
  package_build(
    file.path(tempdir(), "subsetCars")
  )
  dir.create(file.path(tempdir(),"lib"))
  expect_true(document(file.path(tempdir(), "subsetCars"), lib = file.path(tempdir(),"lib")))
  docfile <- readLines(file.path(
    tempdir(),
    "subsetCars", "data-raw", "documentation.R"
  ))
  connection <- file(file.path(
    tempdir(),
    "subsetCars", "data-raw", "documentation.R"
  ),
  open = "w+"
  )
  writeLines(
    text =
      c(docfile, "
  #' Use roxygen to document a package.
  #'
  #' This is dummy documentation used to test markdown documentation
  #' for [roxygen2::roxygenize()] in the `subsetCars`` test package.
  #' 
  #' @name testmarkdownroxygen
  #' @param none there are no parameters
  #'  this is a link to a  function: [document()]
  #' @seealso [DataPackageR::document()], `browseVignettes(\"subsetCars\")`
  #' @md
  NULL
  "),
    con = connection
  )
  flush(connection)
  close(connection)
  expect_output(
    document(file.path(tempdir(), "subsetCars"), lib = file.path(tempdir(),"lib")),
    "Writing testmarkdownroxygen.Rd"
  )
  v <- vignette(package = "subsetCars", lib.loc = file.path(tempdir(),"lib"))
  expect_equal(v$results[, "Item"], "subsetCars")
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
  try(usethis::proj_set(NULL),silent = TRUE) #wrap in try for usethis 1.4 vs 1.5
})
