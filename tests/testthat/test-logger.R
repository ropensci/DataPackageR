context("logger")
withr::with_options(list(DataPackageR_verbose = TRUE),{
  test_that(".multilog_setup", {
    expect_null(DataPackageR:::.multilog_setup(file.path(tempdir(), "test.log")))
  })
  test_that(".multilog_threshold", {
    expect_null(DataPackageR:::.multilog_thresold(INFO, TRACE))
  })
  test_that(".multilog_info", {
    expect_output(DataPackageR:::.multilog_info("message"), "INFO .* message")
    expect_true(utils::file_test("-f", file.path(tempdir(), "test.log")))
  })
  test_that(".multilog_error", {
    expect_output(DataPackageR:::.multilog_error("message"), "ERROR .* message")
  })
  test_that(".multilog_trace", {
    expect_silent(DataPackageR:::.multilog_trace("message"))
    expect_true(length(grep(pattern = "TRACE",
                            readLines(file.path(tempdir(),
                                                "test.log")))) > 0)
  })
  test_that(".multilog_warn", {
    expect_output(DataPackageR:::.multilog_warn("message"), "WARN")
  })
  test_that(".multilog_debug", {
    expect_silent(DataPackageR:::.multilog_debug("message"))
    expect_true(length(grep(pattern = "DEBUG",
                            readLines(file.path(tempdir(),
                                                "test.log")))) > 0)
  })
})
