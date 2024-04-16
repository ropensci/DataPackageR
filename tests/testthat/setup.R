# use these options for testthat tests, interactively or non-interactively,
# and restore previously set options when tests are finished
# https://testthat.r-lib.org/articles/special-files.html
withr::local_options(
  list(
    DataPackageR_interact = FALSE,
    DataPackageR_packagebuilding = FALSE,
    DataPackageR_verbose = FALSE
    ),
  .local_envir = teardown_env()
)
