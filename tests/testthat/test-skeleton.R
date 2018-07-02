context("datapackage skeleton")

test_that("datapackage skeleton builds correct structure", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  tmp <<- tempdir()
  tmp <<- normalizePath(tmp, winslash = "/", mustWork = TRUE)

  expect_null(
    datapackage_skeleton(
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
  datapackage_skeleton(
    name = "subsetCars",
    path = tmp,
    code_files = c(file),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  expect_equal(basename(package_build(
    file.path(tmp, "subsetCars")
  )), "subsetCars_1.0.tar.gz")
  
  old <- setwd(file.path(tmp, "subsetCars"))
  on.exit(setwd(old))
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
    datapackage_skeleton(
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
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint
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
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint
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
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint

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
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)


  list <- yml_list_files(config)
  expect_identical(
    list,
    c(
      subsetCars.Rmd = "subsetCars.Rmd",
      extra.rmd = "extra.rmd"
    )
  )



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
    )), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint

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
    datapackage_skeleton(
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
    datapackage_skeleton(
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
test_that("can_read_pkg_description,  data_version", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  file2 <- system.file("extdata", "tests", "extra.rmd",
    package = "DataPackageR"
  )
  datapackage_skeleton(
    name = "subsetCars",
    path = tmp,
    code_files = c(file, file2),
    force = TRUE,
    r_object_names = c("cars_over_20", "pressure")
  )
  DataPackageR:::read_pkg_description(file.path(tmp, "subsetCars"))
  devtools::load_all(file.path(tmp, "subsetCars"))
  expected_version <-
    structure(list(c(0L, 1L, 0L)),
      class = c("package_version", "numeric_version")
    )
  expect_equal(data_version("subsetCars"), expected_version)
  unlink(file.path(tmp, "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
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
    datapackage_skeleton(
      name = NULL,
      path = tmp,
      code_files = c(file1, file2),
      force = TRUE,
      r_object_names = c("cars_over_20", "pressure")
    )
  )
  datapackage_skeleton(
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
    datapackage_skeleton(
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
    datapackage_skeleton(
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
    datapackage_skeleton(
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
    datapackage_skeleton(
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

context("varying arguments")
test_that("package built in different edge cases", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  tmp <- tempdir()
  datapackage_skeleton(
    name = "subsetCars",
    path = tmp,
    code_files = c(file),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  expect_error(package_build(packageName = NULL))
  old <- setwd(file.path(tmp, "subsetCars"))
  on.exit(setwd(old))
  expect_equal(
    basename(
      package_build(packageName = NULL)
    ),
    "subsetCars_1.0.tar.gz"
  )
  expect_equal(yml_list_objects(file.path(tmp, "subsetCars")), "cars_over_20")

  config <- yml_find(file.path(tmp, "subsetCars"))
  config[["configuration"]]$render_root <- ""
  expect_equal(DataPackageR:::.get_render_root(config), "")
  config[["configuration"]]$render_root <- NULL
  expect_error(DataPackageR:::.get_render_root(config))
  yml <- construct_yml_config("foo",
    render_root = normalizePath(tempdir(),
      winslash = "/"
    )
  )
  expect_equal(
    basename(DataPackageR:::.get_render_root(yml)),
    basename(normalizePath(tempdir(), winslash = "/"))
  )
  expect_error(package_build(tempdir()))
  expect_error(data_version("foo"))
  expect_error(
    DataPackageR:::.increment_data_version("foo",
      new_data_digest = "bar",
      which = "path"
    )
  )
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(tempdir())))
  unlink(file.path(tmp, "foo"),
    force = TRUE,
    recursive = TRUE
  )
  package.skeleton("foo", path = tmp)
  dir.create(file.path(tmp, "foo", "data-raw"))
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(file.path(tmp, "foo"))))
  unlink(file.path(tmp, "foo", "R"), recursive = TRUE, force = TRUE)
  unlink(file.path(tmp, "foo", "inst"), recursive = TRUE, force = TRUE)
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(file.path(tmp, "foo"))))
  unlink(file.path(tmp, "foo"), force = TRUE, recursive = TRUE)


  package.skeleton("foo", path = tmp)
  expect_error(yml_find(file.path(tmp, "foo")))
  dir.create(file.path(tmp, "foo", "data-raw"))
  unlink(file.path(tmp, "foo", "DESCRIPTION"))
  yml <- DataPackageR:::construct_yml_config("foo.Rmd")
  yml_write(yml, path = file.path(tmp, "foo"))
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(file.path(tmp, "foo"))))
  yml$configuration$files <- " "
  yml_write(yml, path = file.path(tmp, "foo"))
  expect_error(DataPackageR:::DataPackageR(file.path(tmp, "foo")))


  unlink(file.path(tmp, "foo"),
    force = TRUE,
    recursive = TRUE
  )
  suppressWarnings(expect_error(construct_yml_config("foo",
    render_root = "bar"
  )))
  unlink(file.path(tmp, "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )

  yml <- DataPackageR:::construct_yml_config("foo.Rmd")
  expect_true(
    yml_enable_compile(
      yml,
      "foo.Rmd"
    )[["configuration"]][["files"]][["foo.Rmd"]][["enabled"]]
  )
  expect_false(
    yml_disable_compile(
      yml,
      "foo.Rmd"
    )[["configuration"]][["files"]][["foo.Rmd"]][["enabled"]]
  )
  expect_error(yml_write("/"))
  expect_equal(
    DataPackageR:::.combine_digests(
      list(
        DataVersion = "1.1.1",
        foo = "bar"
      ),
      list(
        DataVersion = "1.1.1",
        bar = "foo"
      )
    ),
    list(
      DataVersion = "1.1.1",
      bar = "foo",
      foo = "bar"
    )
  )
  e <- new.env()
  d <- list(DataVersion = "0.1.0")
  assign("a", 10, e)
  expect_equal(
    DataPackageR:::.digest_data_env("a", e, d),
    list(
      DataVersion = "0.1.0",
      a = "2522027d230e3dfe02d8b6eba1fd73e1"
    )
  )
  e <- new.env()
  d <- list()
  assign("a", 10, e)
  suppressWarnings(expect_error(DataPackageR:::.digest_data_env("a", e, d)))

  suppressWarnings(expect_error(
    DataPackageR:::.check_dataversion_string(
      list(DataVersion = "1.1.1"),
      list(DataVersion = "1.a.1")
    )
  ))

  unlink(file.path(tmp, "foo"),
    force = TRUE,
    recursive = TRUE
  )
  package.skeleton("foo", path = tempdir())

  library(futile.logger)
  flog.appender(appender.console())
  suppressWarnings(expect_false({
    DataPackageR:::.compare_digests(
      list(
        DataVersion = "1.1.1",
        a = paste0(letters[1:10], collapse = "")
      ),
      list(
        DataVersion = "1.1.2",
        a = paste0(LETTERS[1:10], collapse = "")
      )
    )
  }))

  unlink(file.path(tmp, "foo"),
    force = TRUE,
    recursive = TRUE
  )
  suppressWarnings(expect_error(yml_list_objects("foo")))
  expect_false(DataPackageR:::.validate_render_root("/foobar"))
  suppressWarnings(expect_error(DataPackageR:::yml_add_files("subsetCars", "foo.rmd")))
  suppressWarnings(expect_error(DataPackageR:::yml_disable_compile("subsetCars", "foo.rmd")))
  suppressWarnings(expect_error(DataPackageR:::yml_enable_compile("subsetCars", "foo.rmd")))
  suppressWarnings(expect_error(DataPackageR:::yml_add_objects("subsetCars", "foo.rmd")))
  suppressWarnings(expect_error(DataPackageR:::yml_list_files("subsetCars")))
  suppressWarnings(expect_error(DataPackageR:::yml_remove_objects("subsetCars", "bar")))
  suppressWarnings(expect_error(DataPackageR:::yml_remove_files("subsetCars", "foo.rmd")))
  yml <- DataPackageR:::construct_yml_config("foo.Rmd", "foobar")
  expect_equal(length(names(yml[["configuration"]][["files"]])), 1)
  expect_equal(length(names(
    yml_remove_files(
      yml,
      "foo.Rmd"
    )[["configuration"]][["files"]]
  )), 0)
  expect_error(DataPackageR::construct_yml_config(
    "foo.Rmd",
    render_root = "foobar"
  ))
  expect_error(DataPackageR:::datapackage_skeleton(
    name = "foo", path = tempdir(), list = list()
  ))
  unlink(normalizePath(file.path(
    tempdir(),
    "foo"
  ), winslash = "/"),
  recursive = TRUE, force = TRUE
  )
  expect_error(DataPackageR:::datapackage_skeleton(
    name = "foo", path = tempdir(), r_object_names = "bar",
    list = list(a = "b")
  ))
  unlink("foo", recursive = TRUE, force = TRUE)
  expect_error(DataPackageR:::read_pkg_description("foo"))
  unlink(file.path(tmp, "foo"),
    force = TRUE,
    recursive = TRUE
  )
  package.skeleton(path = tempdir(), "foo")
  dir.create(file.path(tempdir(), "foo", "data-raw"))
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "foo"))))
  yml <- DataPackageR:::construct_yml_config("foo")
  yml_write(yml, path = file.path(tempdir(), "foo"))
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "foo")))
  yml <- DataPackageR:::construct_yml_config("", "bar")
  yml_write(yml, path = file.path(tempdir(), "foo"))
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "foo")))
  unlink(file.path(tempdir(), "foo", "DESCRIPTION"),
    force = TRUE,
    recursive = TRUE
  )
  unlink(file.path(tempdir(), "foo", "config.yml"),
    force = TRUE,
    recursive = TRUE
  )
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "foo")))
  datapackage_skeleton(
    name = "subsetCars",
    path = tempdir(),
    code_files = system.file("extdata", "tests", "subsetCars.Rmd",
      package = "DataPackageR"
    ),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  unlink(file.path(tempdir(), "subsetCars", "DESCRIPTION"),
    force = TRUE,
    recursive = TRUE
  )
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "subsetCars")
    )
  ))
  unlink(file.path(tempdir(), "subsetCars"),
    force = TRUE,
    recursive = TRUE
  )
  datapackage_skeleton(
    name = "subsetCars",
    path = tempdir(),
    code_files = system.file("extdata", "tests", "subsetCars.Rmd",
      package = "DataPackageR"
    ),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  yml <- yml_find(file.path(tempdir(), "subsetCars"))
  ymlbak <- yml
  yml$configuration <- NULL
  yml_write(yml)
  expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "subsetCars")
    )
  )
  yml <- ymlbak
  yml$configuration$files <- NULL
  yml$configuration$objects <- NULL
  yml_write(yml)
  expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "subsetCars")
    )
  )
  yml <- ymlbak
  yml_write(yml_disable_compile(yml, "subsetCars.Rmd"))
  expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "subsetCars")
    )
  )
  yml <- ymlbak
  yml$configuration$render_root <- "foobar"
  yml_write(yml)
  expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "subsetCars")
    )
  )
  yml <- ymlbak
  yml$configuration$objects <- list()
  yml_write(yml)
  expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "subsetCars")
    )
  )
  yml <- ymlbak
  yml$configuration$files <-
    list(foo.rmd = list(
      name =
        "foo.rmd", enabled = TRUE
    ))
  yml_write(yml)
  expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "subsetCars")
    )
  )
  unlink(file.path(tempdir(), "subsetCars", "data"),
    force = TRUE,
    recursive = TRUE
  )
})

test_that("datapackager_object_read", {
  ENVS <- new.env()
  dataenv <- new.env()
  assign("foo", 100, ENVS)
  assign("ENVS", ENVS, dataenv)
  assign(
    "datapackager_object_read",
    datapackager_object_read,
    dataenv
  )
  expect_equal(
    eval(
      datapackager_object_read("foo"),
      dataenv
    ),
    100
  )
})
