context("building packages")

test_that("package can be built from different locations", {
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
  expect_equal(
    basename(
      package_build(
        file.path(tempdir(), "subsetCars")
      )
    ),
    "subsetCars_1.0.tar.gz"
  )

  old <-
    setwd(file.path(tempdir(), "subsetCars")) # nolint
  on.exit(setwd(old)) # nolint
  expect_equal(basename(package_build(".")), "subsetCars_1.0.tar.gz")
  suppressWarnings(expect_error(package_build("subsetCars")))

  unlink(file.path(tempdir(), "subsetCars"),
    recursive = TRUE,
    force = TRUE
  )
})

test_that("Error on data pkg dirname different from data pkg name", {
  td <- withr::local_tempdir()
  sn <- 'skelname'
  not_sn <- paste0('not_', sn)
  datapackage_skeleton(name = sn, path = td)
  file.rename(from = file.path(td, sn), to = file.path(td, not_sn))
  err_msg <- paste("Data package name in DESCRIPTION does not match",
                   "name of the data package directory")
  expect_error(package_build(file.path(td, not_sn)), err_msg)
})

test_that("properly handle relative render_root path from yaml config", {
  # A lightly modified version of Jason's reprex
  withr::with_tempdir({
    datapackage_skeleton("new")

    utils::write.csv(data.frame(x=1:10),
              file.path('new', 'inst', 'extdata', 'ext.csv'),
              row.names=F)

    x <- "x <- read.csv(file.path('inst', 'extdata', 'ext.csv'))"
    writeLines(x, file.path('new', 'data-raw', 'x.R'))

    config <- yml_add_files("new", "x.R")
    config <- yml_add_objects(config, "x")
    config <- yml_write(config, "new")

    yml <- yaml::read_yaml(file.path("new", "datapackager.yml"))
    yml$configuration$render_root$tmp <- NULL
    yml$configuration$render_root <- "./"
    yaml::write_yaml(yml, file.path("new", "datapackager.yml"))

    expect_error(package_build())

    withr::with_dir('new', {
      expect_no_error(package_build())
    })
  })
})
