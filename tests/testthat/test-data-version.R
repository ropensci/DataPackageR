
context("data version strings")
test_that("assert_data_version", {
  f <- tempdir()
  f <- file.path(f, "foo.Rmd")
  con <- file(f)
  writeLines(
    c("---",
      'title: "foo"',
      "---",
      "",
      "```{r}",
      "tbl = table(sample(1:10,1000,replace=TRUE))",
      "```"
    ),
    con = con
  )
  close(con)
  pname <- basename(tempfile())
  datapackage_skeleton(
    name = pname,
    path = normalizePath(tempdir()),
    force = TRUE,
    r_object_names = "tbl",
    code_files = f
  )
  package_build(file.path(tempdir(), pname))
  on.exit(pkgload::unload(pname))
  pkgload::load_all(file.path(tempdir(), pname))
  suppressWarnings(expect_true(
    data_version(pkg = pname) == numeric_version("0.1.0")
  ))
  expect_true(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.1.0",
      acceptable = "equal"
    )
  )
  expect_true(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.1.0",
      acceptable = "equal_or_greater"
    )
  )
  expect_true(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.0.0",
      acceptable = "equal_or_greater"
    )
  )
  expect_true(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.0.11",
      acceptable = "equal_or_greater"
    )
  )
  expect_error(
    assert_data_version(
      data_package_name = pname,
      version_string = "1.0.0",
      acceptable = "equal_or_greater"
    )
  )
  expect_error(
    assert_data_version(
      data_package_name = pname,
      version_string = "1.1.0",
      acceptable = "equal_or_greater"
    )
  )
  expect_error(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.1.1",
      acceptable = "equal_or_greater"
    )
  )
  expect_error(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.1.1",
      acceptable = "equal"
    )
  )
  expect_error(
    assert_data_version(
      data_package_name = pname,
      version_string = "1.0.0",
      acceptable = "equal"
    )
  )
  expect_error(
    assert_data_version(
      data_package_name = pname,
      version_string = "1.1.0",
      acceptable = "equal"
    )
  )
  expect_error(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.2.0",
      acceptable = "equal_or_greater"
    )
  )
  expect_true(
    assert_data_version(
      data_package_name = pname,
      version_string = "0.0.10000001",
      acceptable = "equal_or_greater"
    )
  )
})
