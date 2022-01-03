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
  
  temp_libpath <- file.path(tempdir(), "lib")
  dir.create(temp_libpath)
  
  package_build(file.path(tempdir(), "subsetCars"))
  
  expect_true(document(file.path(tempdir(), "subsetCars"), lib = temp_libpath))
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
    document(file.path(tempdir(), "subsetCars"), lib = temp_libpath),
    "Writing testmarkdownroxygen.Rd"
  )
  v <- vignette(package = "subsetCars", lib.loc = temp_libpath)
  expect_equal(v$results[, "Item"], "subsetCars")
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
  try(usethis::proj_set(NULL),silent = TRUE) #wrap in try for usethis 1.4 vs 1.5
})
