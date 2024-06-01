context("edge cases")
test_that("local edge case block 1", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
                      package = "DataPackageR"
  )
  td <- withr::local_tempdir()
  datapackage_skeleton(
    name = "subsetCars",
    path = td,
    code_files = file,
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  withr::with_dir(td, {
    # error out when wd is the parent directory
    expect_error(package_build(packageName = NULL))
  })
  td_sc <- file.path(td, "subsetCars")
  withr::with_dir(td_sc, {
    # successful build when wd is the package directory
    expect_equal(
      basename(package_build(packageName = NULL)),
      "subsetCars_1.0.tar.gz"
    )
  })
  # some yml tests
  expect_equal(
    yml_list_objects(td_sc),
    "cars_over_20"
  )
  config <- yml_find(td_sc)
  config[["configuration"]]$render_root <- ""
  expect_equal(DataPackageR:::.get_render_root(config), "")
  config[["configuration"]]$render_root <- NULL
  expect_error(DataPackageR:::.get_render_root(config))
})


test_that("local edge case block 2", {
  td <- withr::local_tempdir()
  # This seems like it needs a new tempdir
  yml <- construct_yml_config("foo", render_root = td)
  expect_equal(
    basename(DataPackageR:::.get_render_root(yml)),
    basename(td)
  )
  expect_error(package_build(td))
  expect_error(data_version("foo"))
  # this just errors because 'path' != 'patch'
  expect_error(
    DataPackageR:::.increment_data_version("foo",
                                           new_data_digest = "bar",
                                           which = "path"
    )
  )
  # errors out on non-package path
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(td)))
})

test_that("local edge case block 3", {
  td <- withr::local_tempdir()
  test_env <- new.env()
  assign('test_obj', pi, envir = test_env)
  utils::package.skeleton("foo", path = td, environment = test_env)
  td_foo <- file.path(td, 'foo')
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(td_foo)
  ))
  dir.create(file.path(td_foo, "data-raw"))
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(td_foo)
  ))
  unlink(file.path(td_foo, "R"),
         recursive = TRUE,
         force = TRUE
  )
  unlink(file.path(td_foo, "inst"),
         recursive = TRUE,
         force = TRUE
  )
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(td_foo)
  ))
})


test_that("local edge case block 4", {
  test_env <- new.env()
  assign('test_obj', pi, envir = test_env)
  td <- withr::local_tempdir()
  utils::package.skeleton("foo", path = td, environment = test_env, force = TRUE)
  td_foo <- file.path(td, 'foo')
  expect_error(yml_find(td_foo))
  dir.create(file.path(td_foo, "data-raw"))
  unlink(file.path(td_foo, "DESCRIPTION"))
  yml <- DataPackageR:::construct_yml_config("foo.Rmd")
  yml_write(yml, path = td_foo)
  suppressWarnings(expect_error(
    DataPackageR:::DataPackageR(td_foo)
  ))
  yml$configuration$files <- " "
  yml_write(yml, path = td_foo)
  expect_error(DataPackageR:::DataPackageR(td_foo))
  unlink(td_foo, force = TRUE, recursive = TRUE)
  suppressWarnings({
    expect_error(construct_yml_config("foo", render_root = "bar"))
  })
})


test_that("local edge case block 5", {
  yml <- DataPackageR:::construct_yml_config("foo.Rmd")
  expect_true(yml_enable_compile(
    yml,
    "foo.Rmd"
  )[["configuration"]][["files"]][["foo.Rmd"]][["enabled"]]) # nolint
  expect_false(yml_disable_compile(
    yml,
    "foo.Rmd"
  )[["configuration"]][["files"]][["foo.Rmd"]][["enabled"]]) # nolint
  expect_error(yml_write("x"))
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
})


test_that(".digest_data_env output as expected", {
  e <- new.env()
  assign("a", 10, e)
  expect_equal(
    DataPackageR:::.digest_data_env("a", e, "0.1.0"),
    list(
      DataVersion = "0.1.0",
      a = "2522027d230e3dfe02d8b6eba1fd73e1"
    )
  )
})

test_that("validate_DataVersion works as expected", {
  expect_error(validate_DataVersion(NULL))
  expect_error(validate_DataVersion(NA_character_))
  expect_error(validate_DataVersion(c('1.0.0', '2.0.2')))
  expect_error(validate_DataVersion(1.0))
  expect_error(validate_DataVersion(""))
  expect_equal(validate_DataVersion('0.1.0'), '0.1.0')
  # needs to be a 3-part version
  expect_error(validate_DataVersion('1.0'))
  expect_error(validate_DataVersion('1.0.0.1'))
  # works as expected on valid input
  expect_equal(
    validate_DataVersion(package_version('0.1.0')),
    '0.1.0'
  )
  # converts back to character if input package_version class
  expect_true(
    inherits(
      validate_DataVersion(package_version('0.1.0')),
      'character'
    )
  )
})

test_that(".check_dataversion_string works as expected", {
  expect_error(
    DataPackageR:::.check_dataversion_string(
      list(DataVersion = "1.1.1"),
      list(DataVersion = NULL)
    )
  )
  expect_error(
    DataPackageR:::.check_dataversion_string(
      list(DataVersion = "1.1.1"),
      list(DataVersion = NA_character_)
    )
  )
  expect_error(
    DataPackageR:::.check_dataversion_string(
      list(DataVersion = "1.1.1"),
      list(DataVersion = "1.a.1")
    )
  )
  expect_equal(
    DataPackageR:::.check_dataversion_string(
      list(DataVersion = "1.0.1"),
      list(DataVersion = "1.0.1")
    ),
    "equal"
  )
  expect_equal(
    DataPackageR:::.check_dataversion_string(
      list(DataVersion = "1.0.1"),
      list(DataVersion = "1.1.1")
    ),
    "lower"
  )
  expect_equal(
    DataPackageR:::.check_dataversion_string(
      list(DataVersion = "1.0.2"),
      list(DataVersion = "1.0.1")
    ),
    "higher"
  )
})


test_that("local edge case block 8", {
  test_env <- new.env()
  assign('test_obj', pi, envir = test_env)
  td <- withr::local_tempdir()
  utils::package.skeleton("foo", path = td,
                          environment = test_env, force = TRUE)
  td_foo <- file.path(td, 'foo')
  DataPackageR:::.multilog_setup(file.path(td,"test.log"))
  DataPackageR:::.multilog_thresold(INFO, TRACE)
  # data in digest changes while names do not
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
  # names in digest changes while data do not
  suppressWarnings(expect_false({
    DataPackageR:::.compare_digests(
      list(
        DataVersion = "1.1.1",
        a = paste0(letters[1:10], collapse = "")
      ),
      list(
        DataVersion = "1.1.2",
        b = paste0(letters[1:10], collapse = "")
      )
    )
  }))
  # names in digest nor data changes
  suppressWarnings(expect_true({
    DataPackageR:::.compare_digests(
      list(
        DataVersion = "1.1.1",
        a = paste0(letters[1:10], collapse = "")
      ),
      list(
        DataVersion = "1.1.1",
        a = paste0(letters[1:10], collapse = "")
      )
    )
  }))
  # names in old digest have one more than new
  suppressWarnings(expect_false({
    DataPackageR:::.compare_digests(
      list(
        DataVersion = "1.1.1",
        a = paste0(letters[1:10], collapse = ""),
        b = paste0(LETTERS[1:10], collapse = "")
      ),
      list(
        DataVersion = "1.1.2",
        a = paste0(letters[1:10], collapse = "")
      )
    )
  }))
  # names in new digest have one more than old
  suppressWarnings(expect_false({
    DataPackageR:::.compare_digests(
      list(
        DataVersion = "1.1.1",
        a = paste0(letters[1:10], collapse = "")
      ),
      list(
        DataVersion = "1.1.2",
        a = paste0(letters[1:10], collapse = ""),
        b = paste0(LETTERS[1:10], collapse = "")
      )
    )
  }))
  unlink(td_foo,
         force = TRUE,
         recursive = TRUE
  )
  suppressWarnings(expect_error(yml_list_objects(td_foo)))
  expect_error(DataPackageR:::.validate_render_root(file.path(td, 'foobar')))
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
    path = td
  ))
  expect_no_error(validate_package_skeleton(file.path(td, 'foo')))
  suppressWarnings(
    unlink(td_foo,
           recursive = TRUE,
           force = TRUE
    )
  )
  expect_error(validate_package_skeleton(file.path(td, 'foo')))
})


test_that("local edge case block 9", {
  test_env <- new.env()
  assign('test_obj', pi, envir = test_env)
  td <- withr::local_tempdir()
  utils::package.skeleton("foo", path = td, environment = test_env, force = TRUE)
  td_foo <- file.path(td, 'foo')
  dir.create(file.path(td_foo, "data-raw"))
  suppressWarnings({
    expect_error(DataPackageR:::DataPackageR(td_foo))
  })
  yml <- DataPackageR:::construct_yml_config("foo")
  yml_write(yml, path = td_foo)
  expect_error(DataPackageR:::DataPackageR(td_foo))
  yml <- DataPackageR:::construct_yml_config("", "bar")
  yml_write(yml, path = td_foo)
  expect_error(DataPackageR:::DataPackageR(td_foo))
  unlink(
    file.path(td_foo, "DESCRIPTION"),
    force = TRUE,
    recursive = TRUE
  )
  unlink(file.path(td_foo, "datapackager.yml"),
         force = TRUE,
         recursive = TRUE
  )
  try(usethis::proj_set(NULL),silent = TRUE) #wrap in try for usethis 1.4 vs 1.5
  expect_error(DataPackageR:::DataPackageR(td_foo))
})


test_that("local edge case block 10", {
  td <- withr::local_tempdir()
  datapackage_skeleton(
    name = "subsetCars",
    path = td,
    code_files = system.file("extdata", "tests", "subsetCars.Rmd",
                             package = "DataPackageR"
    ),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  td_cars <- file.path(td, 'subsetCars')
  unlink(
    file.path(td_cars, "DESCRIPTION"),
    force = TRUE,
    recursive = TRUE
  )
  suppressWarnings(expect_error(DataPackageR:::DataPackageR(td_cars)))
})


test_that("local edge case block 11", {
  td <- withr::local_tempdir()
  datapackage_skeleton(
    name = "subsetCars",
    path = td,
    code_files = system.file("extdata", "tests", "subsetCars.Rmd",
                             package = "DataPackageR"
    ),
    force = TRUE,
    r_object_names = "cars_over_20"
  )
  td_cars <- file.path(td, 'subsetCars')
  yml <- yml_find(td_cars)
  ymlbak <- yml
  yml$configuration <- NULL
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(td_cars))
  yml <- ymlbak
  yml$configuration$files <- NULL
  yml$configuration$objects <- NULL
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(td_cars))
  yml <- ymlbak
  yml_write(yml_disable_compile(yml, "subsetCars.Rmd"))
  expect_error(DataPackageR:::DataPackageR(td_cars))
  yml <- ymlbak
  yml$configuration$render_root <- "foobar"
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(td_cars))
  yml <- ymlbak
  yml$configuration$objects <- list()
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(td_cars))
  yml <- ymlbak
  yml$configuration$files <-
    list(foo.rmd = list(
      name =
        "foo.rmd", enabled = TRUE
    ))
  yml_write(yml)
  expect_error(DataPackageR:::DataPackageR(td_cars))
})
