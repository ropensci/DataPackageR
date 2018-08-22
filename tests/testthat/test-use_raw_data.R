context("test-use_raw_data")

test_that("use_raw_data works as expected", {
  myfile <- tempfile()
  file <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  ancillary <- system.file("extdata", "tests", "rfileTest.R",
    package = "DataPackageR"
  )
  raw_data <- system.file("extdata", "tests", "raw_data",
    package = "DataPackageR"
  )
  expect_null(
    datapackage_skeleton(
      name = "subsetCars20",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = "data",
      raw_data_dir = raw_data,
      dependencies = ancillary
    )
  )
  file.create(myfile)
  unlink(
    file.path(tempdir(), "subsetCars20", "inst"),
    force = TRUE,
    recursive = TRUE
  )
  expect_error(use_raw_dataset(myfile))
  dir.create(file.path(
    tempdir(),
    "subsetCars20", "inst", "extdata"
  ),
  recursive = TRUE
  )
  expect_true(use_raw_dataset(myfile))
  expect_true(file_test(
    "-f",
    file.path(
      tempdir(),
      "subsetCars20",
      "inst",
      "extdata",
      basename(myfile)
    )
  ))
  expect_error(use_raw_dataset())
  expect_error(suppressWarnings(use_raw_dataset("foobar")))
  expect_true(use_raw_dataset(file.path(tempdir(), "subsetCars20", "R")))
})


test_that("use_processing_script works as expected", {
  myfile <- tempfile()
  file <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage_skeleton(
      name = "subsetCars20",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = "data"
    )
  )
  expect_true(use_processing_script("newScript.Rmd"))
  expect_true(use_processing_script("newScript.Rmd", title = "foo"))
  expect_true(use_processing_script("newScript.Rmd", title = "foo", author = "bar"))
  expect_true(use_processing_script("newScript.Rmd", author = "bar"))

  expect_true(use_processing_script("newScript.R"))
  expect_true(use_processing_script("newScript.R", title = "foo"))
  expect_true(use_processing_script("newScript.R", title = "foo", author = "bar"))
  expect_true(use_processing_script("newScript.R", author = "bar"))
  expect_equal(readLines(
    file.path(tempdir(), "subsetCars20", "data-raw", "newScript.Rmd")
  )[2], "author: bar")
  expect_equal(readLines(
    file.path(tempdir(), "subsetCars20", "data-raw", "newScript.R")
  )[2], "#' author: bar")
  expect_error(use_processing_script(file = NULL))
  unlink(
    file.path(tempdir(), "subsetCars20", "data-raw"),
    force = TRUE,
    recursive = TRUE
  )
  expect_error(use_processing_script(file = "newScript.R"))
  dir.create(file.path(tempdir(), "subsetCars20", "data-raw"))
  expect_error(use_processing_script(file = "newScript.foo"))
  expect_error(use_processing_script("."))
  file.create(file.path(tempdir(), "foo.csv"))
  expect_error(use_processing_script(file.path(tempdir(), "foo.csv")))
  file.create(file.path(tempdir(), "foo.R"))
  expect_true(use_processing_script(file.path(tempdir(), "foo.R")))
})

test_that("use_data_object works as expected", {
  myfile <- tempfile()
  file <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage_skeleton(
      name = "subsetCars20",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = "data"
    )
  )
  expect_true(use_data_object("newobject"))
  expect_error(use_data_object(object_name = NULL))
})

test_that(".update_header", {
  con <- file(file.path(tempdir(), "foo.R"), open = "wt")
  writeLines(
    text = c("#' ---", "#' title: My Title", "#' author: My Name", "#' ---"),
    con = con
  )
  close(con)
  DataPackageR:::.update_header(
    file = file.path(tempdir(), "foo.R"),
    title = "new title",
    author = "new author"
  )
  expect_equal(
    readLines(file.path(tempdir(), "foo.R")),
    c("#' ---", "#' title: new title", "#' author: new author", "#' ---")
  )

  con <- file(file.path(tempdir(), "foo.Rmd"), open = "wt")
  writeLines(text = c("---", "title: My Title", "author: My Name", "---"), con = con)
  close(con)
  DataPackageR:::.update_header(
    file = file.path(tempdir(), "foo.Rmd"),
    title = "new title",
    author = "new author"
  )
  expect_equal(
    readLines(file.path(tempdir(), "foo.Rmd")),
    c("---", "title: new title", "author: new author", "---")
  )
})

test_that(".partition_r_front_matter", {
  test_string1 <-
    c("#' ---\n", "#' input: in", "#' output out", "#' ---")
  test_string2 <- c("#' ---\n", "#' input: in", "#' output out")
  test_string3 <- c("#' input: in", "#' output out")
  test_string4 <- c("#' input: in", "#' output out", "#' ---")
  test_string5 <-
    c(" ", "#' ---\n", "#' input: in", "#' output out", "#' ---")

  expect_equal(
    DataPackageR:::.partition_r_front_matter(test_string1)$body,
    NULL
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_r_front_matter(test_string1)$front_matter
    ),
    FALSE
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_r_front_matter(test_string2)$body
    ),
    FALSE
  )
  expect_equal(
    DataPackageR:::.partition_r_front_matter(test_string2)$front_matter,
    NULL
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_r_front_matter(test_string3)$body
    ),
    FALSE
  )
  expect_equal(
    DataPackageR:::.partition_r_front_matter(test_string3)$front_matter,
    NULL
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_r_front_matter(test_string4)$body
    ),
    FALSE
  )
  expect_equal(
    DataPackageR:::.partition_r_front_matter(test_string4)$front_matter,
    NULL
  )
  expect_equal(
    DataPackageR:::.partition_r_front_matter(test_string5)$body,
    " "
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_r_front_matter(test_string5)$front_matter
    ),
    FALSE
  )
})

test_that(".partition_rmd_front_matter", {
  test_string1 <- c("---\n", " input: in", " output out", "---")
  test_string2 <- c("---\n", " input: in", " output out")
  test_string3 <- c(" input: in", " output out")
  test_string4 <- c("input: in", " output out", " ---")
  test_string5 <- c(" ", "---\n", " input: in", " output out", "---")

  expect_equal(
    DataPackageR:::.partition_rmd_front_matter(test_string1)$body,
    NULL
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_rmd_front_matter(test_string1)$front_matter
    ),
    FALSE
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_rmd_front_matter(test_string2)$body
    ),
    FALSE
  )
  expect_equal(
    DataPackageR:::.partition_rmd_front_matter(test_string2)$front_matter,
    NULL
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_rmd_front_matter(test_string3)$body
    ),
    FALSE
  )
  expect_equal(
    DataPackageR:::.partition_rmd_front_matter(test_string3)$front_matter,
    NULL
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_rmd_front_matter(test_string4)$body
    ),
    FALSE
  )
  expect_equal(
    DataPackageR:::.partition_rmd_front_matter(test_string4)$front_matter,
    NULL
  )
  expect_equal(
    is.null(
      DataPackageR:::.partition_rmd_front_matter(test_string5)$front_matter
    ),
    FALSE
  )
  expect_equal(
    DataPackageR:::.partition_rmd_front_matter(test_string5)$body,
    " "
  )
})


test_that(".parse_yaml_front_matter", {
  test_string1 <- c("---", " input: in", " output: out", "---\n")
  test_string2 <- c("---", " input: in", " output: out\n")
  test_string3 <- c(" input: in", " output: out\n")
  test_string4 <- c("input: in", " output: out", "---\n")
  test_string5 <-
    c(" ", "---", " input: in", " output: out", "---\n")
  test_string6 <- c("---", "input: in ", "output: out", "test:")
  expect_null(DataPackageR:::.validate_front_matter(paste0(test_string1, collapse = "\n")))
  expect_error(DataPackageR:::.validate_front_matter(paste0(test_string6, collapse = "\n")))

  expect_equal(
    (DataPackageR:::.parse_yaml_front_matter(test_string1)),
    list(input = "in", output = "out")
  )
  expect_equal(
    (DataPackageR:::.parse_yaml_front_matter(test_string2)),
    list(input = "in")
  )
  expect_equal((DataPackageR:::.parse_yaml_front_matter(test_string3)), list())
  expect_equal(
    (DataPackageR:::.parse_yaml_front_matter(test_string4)),
    list(output = "out")
  )
  expect_equal(
    (DataPackageR:::.parse_yaml_front_matter(test_string5)),
    list(input = "in", output = "out")
  )
  expect_equal(
    (DataPackageR:::.parse_yaml_front_matter(test_string6)),
    list(input = "in", output = "out")
  )
})

test_that(".mark_utf8", {
  expect_equal(DataPackageR:::.mark_utf8("\320\274"), "м")
  expect_equal(DataPackageR:::.mark_utf8(list("\320\274")), list("м"))
})

test_that(".is_blank", {
  expect_true(DataPackageR:::.is_blank(x = ""))
})
