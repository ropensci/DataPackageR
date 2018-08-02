context("yaml config manipulation")
test_that("yaml reading, adding, removing, listing, and writing", {
  file <- system.file("extdata", "tests", "subsetCars.Rmd",
    package = "DataPackageR"
  )
  expect_null(
    datapackage_skeleton(
      name = "subsetCars",
      path = tempdir(),
      code_files = c(file),
      force = TRUE,
      r_object_names = "cars_over_20"
    )
  )

  test_config <-
    structure(list(
      configuration = list(
        files = list(
          subsetCars.Rmd =
            list(enabled = TRUE)
        ),
        objects = "cars_over_20",
        render_root = "dummy"
      )
    ))
  config <- yml_find(file.path(tempdir(), "subsetCars"))
  config$configuration$render_root <- "dummy"
  attr(test_config, "path") <- attr(config, "path")
  expect_identical(config, test_config)

  config <- yml_add_files(config, "extra.rmd")
  test_config <-
    structure(list(
      configuration = list(
        files = list(
          subsetCars.Rmd = list(enabled = TRUE),
          extra.rmd = list(enabled = TRUE)
        ),
        objects = "cars_over_20",
        render_root = "dummy"
      )
    ), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"

  expect_identical(config, test_config)

  config <- yml_remove_files(config, "foo_file")
  test_config <-
    structure(list(
      configuration = list(
        files = list(
          subsetCars.Rmd = list(enabled = TRUE),
          extra.rmd = list(enabled = TRUE)
        ),
        objects = "cars_over_20",
        render_root = "dummy"
      )
    ), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)


  config <- yml_add_objects(config, "foo_obj")
  test_config <-
    structure(list(
      configuration = list(
        files = list(
          subsetCars.Rmd = list(enabled = TRUE),
          extra.rmd = list(enabled = TRUE)
        ),
        objects = c(
          "cars_over_20",
          "foo_obj"
        ),
        render_root = "dummy"
      )
    ), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint

  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)


  config <- yml_remove_objects(config, "foo_obj")
  test_config <-
    structure(list(
      configuration = list(
        files = list(
          subsetCars.Rmd = list(enabled = TRUE),
          extra.rmd = list(enabled = TRUE)
        ),
        objects = "cars_over_20",
        render_root = "dummy"
      )
    ), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)


  list <- yml_list_files(config)
  expect_identical(
    list,
    c("subsetCars.Rmd", "extra.rmd")
  )



  list <- yml_list_objects(config)
  expect_identical(list, "cars_over_20")

  # still the same after writing?
  yml_write(config)
  test_config <-
    structure(list(
      configuration = list(
        files = list(
          subsetCars.Rmd = list(enabled = TRUE),
          extra.rmd = list(enabled = TRUE)
        ),
        objects = "cars_over_20",
        render_root = "dummy"
      )
    ), path = "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmp7DyEjM/subsetCars/datapackager.yml") # nolint

  config <- yml_find(file.path(tempdir(), "subsetCars"))
  attr(test_config, "path") <- attr(config, "path")
  config$configuration$render_root <- "dummy"
  expect_identical(config, test_config)
  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})
