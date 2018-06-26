context("datapackage skeleton")

test_that("datapackage skeleton builds correct structure", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  tmp <<- tempdir()
  tmp <<- normalizePath(tmp, winslash = "/", mustWork = TRUE)

  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file),
      force = TRUE,
      r_object_names = "cars_over_20"
    )
  )
  unlink(file.path(tmp, "subsetCars"), recursive = TRUE, force = TRUE)
})

context("building")
test_that("package can be built from different locations", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  datapackage.skeleton(
    name = "subsetCars",
    path = tmp,
    code_files = c(file),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  expect_equal(basename(package_build(
    file.path(tmp, "subsetCars")
  )), "subsetCars_1.0.tar.gz")
  old <- getwd()
  on.exit(setwd(old))
  setwd(file.path(tmp, "subsetCars"))
  expect_equal(basename(package_build(".")), "subsetCars_1.0.tar.gz")
  expect_error(package_build("subsetCars"))
  unlink(file.path(tmp, "subsetCars"), recursive = TRUE, force = TRUE)
})

context("yaml config")
test_that("yaml reading, adding, removing, listing, and writing", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file),
      force = TRUE,
      r_object_names = "cars_over_20"
    )
  )

  test_config <-
    structure(list(configuration = list(
      files = list(
        subsetCars.Rmd =
          list(name = "subsetCars.Rmd", enabled = TRUE)
      ),
      objects = "cars_over_20",
      render_root = "dummy"
    )))
  config <- yml_find(file.path(tmp, "subsetCars"))
  config$configuration$render_root <- "dummy"
  attr(test_config, "path") <- attr(config, "path")
  expect_identical(config, test_config)

  config <- yml_add_files(config, "extra.rmd")
  test_config <-
    structure(list(configuration = list(
      files = list(
        subsetCars.Rmd = list(name = "subsetCars.Rmd", enabled = TRUE),
        extra.rmd = list(name = "extra.rmd", enabled = TRUE)
      ),
      objects = "cars_over_20",
      render_root = "dummy"
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml")
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"

  expect_identical(config, test_config)

  config <- yml_remove_files(config, "foo_file")
  test_config <-
    structure(list(configuration = list(
      files = list(
        subsetCars.Rmd = list(name = "subsetCars.Rmd", enabled = TRUE),
        extra.rmd = list(name = "extra.rmd", enabled = TRUE)
      ),
      objects = "cars_over_20",
      render_root = "dummy"
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml")
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)


  config <- yml_add_objects(config, "foo_obj")
  test_config <-
    structure(list(configuration = list(
      files = list(
        subsetCars.Rmd = list(name = "subsetCars.Rmd", enabled = TRUE),
        extra.rmd = list(name = "extra.rmd", enabled = TRUE)
      ),
      objects = c(
        "cars_over_20",
        "foo_obj"
      ),
      render_root = "dummy"
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml")

  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)


  config <- yml_remove_objects(config, "foo_obj")
  test_config <-
    structure(list(configuration = list(
      files = list(
        subsetCars.Rmd = list(name = "subsetCars.Rmd", enabled = TRUE),
        extra.rmd = list(name = "extra.rmd", enabled = TRUE)
      ),
      objects = "cars_over_20",
      render_root = "dummy"
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml")
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)


  list <- yml_list_files(config)
  expect_identical(list, c(subsetCars.Rmd = "subsetCars.Rmd", extra.rmd = "extra.rmd"))



  list <- yml_list_objects(config)
  expect_identical(list, "cars_over_20")

  # still the same after writing?
  yml_write(config)
  test_config <-
    structure(list(configuration = list(
      files = list(
        subsetCars.Rmd = list(name = "subsetCars.Rmd", enabled = TRUE),
        extra.rmd = list(name = "extra.rmd", enabled = TRUE)
      ),
      objects = "cars_over_20",
      render_root = "dummy"
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml")

  config <- yml_find(file.path(tmp, "subsetCars"))
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)
  unlink(file.path(tmp, "subsetCars"), recursive = TRUE, force = TRUE)
})

context("conditional build")
test_that("can add a data item", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file, file2),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tmp, "subsetCars"))
  expect_equal(
    list.files(file.path(tmp, "subsetCars", "data")),
    "cars_over_20.rda"
  )
  expect_true(all(
    c("subsetCars", "cars_over_20") %in%
      names(DataPackageR:::.doc_parse(list.files(
        file.path(tmp, "subsetCars", "R"),
        full.names = TRUE
      )))
  ))
  unlink(file.path(tmp, "subsetCars"), recursive = TRUE, force = TRUE)
})

test_that("can remove a data item", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file, file2),
      force = TRUE,
      r_object_names = c("cars_over_20", "pressure")
    )
  )
  package_build(file.path(tmp, "subsetCars"))
  # have we saved the new object?
  config <- yml_find(file.path(tmp, "subsetCars"))
  config <- yml_disable_compile(config, basename(file2))
  yml_write(config)
  package_build(file.path(tmp, "subsetCars"))
  expect_equal(
    list.files(file.path(tmp, "subsetCars", "data")),
    c("cars_over_20.rda", "pressure.rda")
  )
  expect_true(all(
    c("subsetCars", "cars_over_20", "pressure") %in%
      names(DataPackageR:::.doc_parse(list.files(
        file.path(tmp, "subsetCars", "R"),
        full.names = TRUE
      )))
  ))
  unlink(file.path(tmp, "subsetCars"), recursive = TRUE, force = TRUE)
})

context("documentation")
test_that("can_read_pkg_description,  dataVersion", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  datapackage.skeleton(
    name = "subsetCars",
    path = tmp,
    code_files = c(file, file2),
    force = TRUE,
    r_object_names = c("cars_over_20", "pressure")
  )
  DataPackageR:::read_pkg_description(file.path(tmp, "subsetCars"))
  devtools::load_all(file.path(tmp, "subsetCars"))
  expected_version <- structure(list(c(0L, 1L, 0L)), class = c("package_version", "numeric_version"))
  expect_equal(dataVersion("subsetCars"), expected_version)
  unlink(file.path(tmp, "subsetCars"), recursive = TRUE, force = TRUE)
})


context("skeleton")
test_that("edge cases work as expected", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_error(
    datapackage.skeleton(
      name = NULL,
      path = tmp,
      code_files = c(file1, file2),
      force = TRUE,
      r_object_names = c("cars_over_20", "pressure")
    )
  )
  datapackage.skeleton(
    name = "subsetCars",
    list = list(),
    path = tmp,
    code_files = c(file2),
    force = TRUE,
    r_object_names = c("cars_over_20")
  )
  unlink(file.path(tmp, "subsetCars"), recursive = TRUE, force = TRUE)
})

context("conditional build")
test_that("can add a file", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tmp, "subsetCars"))
  expect_equal(
    list.files(file.path(tmp, "subsetCars", "data")),
    "cars_over_20.rda"
  )
  expect_true(all(
    c("subsetCars", "cars_over_20") %in%
      names(DataPackageR:::.doc_parse(
        list.files(file.path(tmp, "subsetCars", "R"), full.names = TRUE)
      ))
  ))
  config <- yml_find(file.path(tmp, "subsetCars"))
  config <- yml_add_files(config, "extra.rmd")
  yml_write(config)
  file.copy(from = file2, file.path(tmp, "subsetCars", "data-raw"))
  expect_equal(
    basename(package_build(file.path(
      tmp,
      "subsetCars"
    ))),
    "subsetCars_1.0.tar.gz"
  )
  expect_equal(
    names(DataPackageR:::.doc_parse(
      list.files(file.path(
        tmp,
        "subsetCars",
        "R"
      ),
      full.names = TRUE
      )
    )),
    c("subsetCars", "cars_over_20")
  )
  config <- yml_add_objects(config, "pressure")
  yml_write(config)
  expect_equal(
    basename(package_build(file.path(
      tmp,
      "subsetCars"
    ))),
    "subsetCars_1.0.tar.gz"
  )
  expect_equal(
    names(DataPackageR:::.doc_parse(
      list.files(file.path(
        tmp,
        "subsetCars",
        "R"
      ),
      full.names = TRUE
      )
    )),
    c("subsetCars", "cars_over_20", "pressure")
  )
  expect_equal(
    basename(list.files(
      file.path(tmp, "subsetCars", "data"),
      full.names = TRUE
    )),
    c("cars_over_20.rda", "pressure.rda")
  )
  unlink(file.path(tmp, "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})

test_that("auto bump version when data unchanged", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tmp, "subsetCars"))
  pkg <- desc::desc(file.path(tmp, "subsetCars"))
  pkg$set("DataVersion", "0.0.0")
  pkg$write()
  package_build(file.path(tmp, "subsetCars"))
  unlink(file.path(tmp, "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})


test_that("manual bump version when data unchanged", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tmp, "subsetCars"))
  pkg <- desc::desc(file.path(tmp, "subsetCars"))
  pkg$set("DataVersion", "0.2.0")
  pkg$write()
  package_build(file.path(tmp, "subsetCars"))
  unlink(file.path(tmp, "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})


test_that("data changes but version out of sync", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage.skeleton(
      name = "subsetCars",
      path = tmp,
      code_files = c(file),
      force = TRUE,
      r_object_names = c("cars_over_20")
    )
  )
  package_build(file.path(tmp, "subsetCars"))
  config <- yml_find(file.path(tmp, "subsetCars"))
  config <- yml_add_files(config, "extra.rmd")
  config <- yml_add_objects(config, "pressure")
  file.copy(file2, file.path(tmp, "subsetCars", "data-raw"))
  yml_write(config)
  pkg <- desc::desc(file.path(tmp, "subsetCars"))
  pkg$set("DataVersion", "0.0.0")
  pkg$write()
  package_build(file.path(tmp, "subsetCars"))
  unlink(file.path(tmp, "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})


context("devtool_functions.R")
test_that("create_namespace", {
  expect_null(DataPackageR:::create_namespace(tmp))
})

test_that("extract_package_name", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  datapackage.skeleton(
    name = "subsetCars",
    path = tmp,
    code_files = c(file),
    force = TRUE,
    r_object_names = c("cars_over_20")
  )
  expect_equal(DataPackageR:::extract_package_name(file.path(tmp, "subsetCars")), "subsetCars")
  unlink(file.path(tmp, "subsetCars"))
})


test_that("check_package_name", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  datapackage.skeleton(
    name = "subsetCars",
    path = tmp,
    code_files = c(file),
    force = TRUE,
    r_object_names = c("cars_over_20")
  )
  expect_null(DataPackageR:::check_package_name(file.path(tmp, "subsetCars")))
  unlink(file.path(tmp, "subsetCars"))
})


test_that(".setup", {
  tmp <- tempdir()
  tmp <- file.path(tmp, "test")
  dir.create(tmp)
  expect_true(DataPackageR:::.setup(path = tmp))
  unlink(tmp, force = TRUE, recursive = TRUE)
})
