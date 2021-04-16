context("edge cases")
test_that("package built in different edge cases", {
  require(futile.logger)
  DataPackageR:::.multilog_setup(normalizePath(file.path(tempdir(),"test.log"), winslash = "/"))
  DataPackageR:::.multilog_thresold(INFO, TRACE)
  
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
  expect_error(package_build(packageName = NULL))
  old <- getwd()
  setwd(file.path(tempdir(), "subsetCars")) # nolint
  on.exit(setwd(old)) # nolint
  expect_equal(
    basename(package_build(packageName = NULL)),
    "subsetCars_1.0.tar.gz"
  )
  expect_equal(
    yml_list_objects(file.path(
      tempdir(),
      "subsetCars"
    )),
    "cars_over_20"
  )

  config <- yml_find(file.path(tempdir(), "subsetCars"))
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
  unlink(file.path(tempdir(), "foo"),
    force = TRUE,
    recursive = TRUE
  )
  package.skeleton("foo", path = tempdir())
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "foo")
    )
  ))
  dir.create(file.path(tempdir(), "foo", "data-raw"))
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "foo")
    )
  ))
  unlink(file.path(tempdir(), "foo", "R"),
    recursive = TRUE,
    force = TRUE
  )
  unlink(file.path(tempdir(), "foo", "inst"),
    recursive = TRUE,
    force = TRUE
  )
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "foo")
    )
  ))
  unlink(file.path(tempdir(), "foo"),
    force = TRUE,
    recursive = TRUE
  )


  package.skeleton("foo", path = tempdir(), force = TRUE)
  expect_error(yml_find(file.path(tempdir(), "foo")))
  dir.create(file.path(tempdir(), "foo", "data-raw"))
  unlink(file.path(tempdir(), "foo", "DESCRIPTION"))
  yml <- DataPackageR:::construct_yml_config("foo.Rmd")
  yml_write(yml, path = file.path(tempdir(), "foo"))
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(
      file.path(tempdir(), "foo")
    )
  ))
  yml$configuration$files <- " "
  yml_write(yml, path = file.path(tempdir(), "foo"))
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "foo")))


  unlink(file.path(tempdir(), "foo"),
    force = TRUE,
    recursive = TRUE
  )
  suppressWarnings(expect_error(construct_yml_config("foo",
    render_root = "bar"
  )))
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
  setwd(old)

  yml <- DataPackageR:::construct_yml_config("foo.Rmd")
  expect_true(yml_enable_compile(
    yml,
    "foo.Rmd"
  )[["configuration"]][["files"]][["foo.Rmd"]][["enabled"]]) # nolint
  expect_false(yml_disable_compile(
    yml,
    "foo.Rmd"
  )[["configuration"]][["files"]][["foo.Rmd"]][["enabled"]]) # nolint
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

  suppressWarnings(expect_error(DataPackageR:::.check_dataversion_string(
    list(DataVersion = "1.1.1"),
    list(DataVersion = "1.a.1")
  )))

  unlink(file.path(tempdir(), "foo"),
    force = TRUE,
    recursive = TRUE
  )
  package.skeleton("foo", path = tempdir())
  DataPackageR:::.multilog_setup(normalizePath(file.path(tempdir(),"test.log"), winslash = "/"))
  DataPackageR:::.multilog_thresold(INFO, TRACE)
  
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

  unlink(file.path(tempdir(), "foo"),
    force = TRUE,
    recursive = TRUE
  )
  suppressWarnings(expect_error(yml_list_objects("foo")))
  expect_false(DataPackageR:::.validate_render_root("/foobar"))
  suppressWarnings(expect_error(
    DataPackageR:::yml_add_files("subsetCars", "foo.rmd")
  ))
  suppressWarnings(expect_error(
    DataPackageR:::yml_disable_compile("subsetCars", "foo.rmd")
  ))
  suppressWarnings(expect_error(
    DataPackageR:::yml_enable_compile("subsetCars", "foo.rmd")
  ))
  suppressWarnings(expect_error(
    DataPackageR:::yml_add_objects("subsetCars", "foo.rmd")
  ))
  suppressWarnings(expect_error(
    DataPackageR:::yml_list_files("subsetCars")
  ))
  suppressWarnings(expect_error(
    DataPackageR:::yml_remove_objects("subsetCars", "bar")
  ))
  suppressWarnings(expect_error(
    DataPackageR:::yml_remove_files("subsetCars", "foo.rmd")
  ))
  yml <- DataPackageR:::construct_yml_config("foo.Rmd", "foobar")
  expect_equal(length(names(yml[["configuration"]][["files"]])), 1)
  expect_equal(length(names(yml_remove_files(
    yml,
    "foo.Rmd"
  )[["configuration"]][["files"]])), 0)
  expect_error(DataPackageR::construct_yml_config("foo.Rmd",
    render_root = "foobar"
  ))
  expect_null(DataPackageR:::datapackage_skeleton(
    name = "foo",
    path = tempdir()
  ))
  suppressWarnings(unlink(normalizePath(file.path(
    tempdir(),
    "foo"
  ), winslash = "/"),
  recursive = TRUE,
  force = TRUE
  ))
  expect_error(DataPackageR:::read_pkg_description("foo"))
  unlink(file.path(tempdir(), "foo"),
    force = TRUE,
    recursive = TRUE
  )
  package.skeleton(path = tempdir(), "foo")
  dir.create(file.path(tempdir(), "foo", "data-raw"))
  suppressWarnings(
    expect_error(
      DataPackageR:::DataPackageR(file.path(
        tempdir(),
        "foo"
      ))
    )
  )
  yml <- DataPackageR:::construct_yml_config("foo")
  yml_write(yml, path = file.path(tempdir(), "foo"))
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "foo")))
  yml <- DataPackageR:::construct_yml_config("", "bar")
  yml_write(yml, path = file.path(tempdir(), "foo"))
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "foo")))
  unlink(
    file.path(tempdir(), "foo", "DESCRIPTION"),
    force = TRUE,
    recursive = TRUE
  )
  unlink(file.path(tempdir(), "foo", "config.yml"),
    force = TRUE,
    recursive = TRUE
  )
  try(usethis::proj_set(NULL),silent = TRUE) #wrap in try for usethis 1.4 vs 1.5
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
  unlink(
    file.path(tempdir(), "subsetCars", "DESCRIPTION"),
    force = TRUE,
    recursive = TRUE
  )
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(file.path(
    tempdir(), "subsetCars"
  ))))
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
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "subsetCars")))
  yml <- ymlbak
  yml$configuration$files <- NULL
  yml$configuration$objects <- NULL
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "subsetCars")))
  yml <- ymlbak
  yml_write(yml_disable_compile(yml, "subsetCars.Rmd"))
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "subsetCars")))
  yml <- ymlbak
  yml$configuration$render_root <- "foobar"
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "subsetCars")))
  yml <- ymlbak
  yml$configuration$objects <- list()
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "subsetCars")))
  yml <- ymlbak
  yml$configuration$files <-
    list(foo.rmd = list(
      name =
        "foo.rmd", enabled = TRUE
    ))
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(file.path(tempdir(), "subsetCars")))
  unlink(
    file.path(tempdir(), "subsetCars", "data"),
    force = TRUE,
    recursive = TRUE
  )
})

