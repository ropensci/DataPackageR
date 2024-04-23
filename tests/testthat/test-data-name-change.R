test_that("data object can be renamed", {

  addData <- function(dataset, pname){
    fil <- sprintf("data(%s, envir=environment())", dataset)
    writeLines(fil, file.path(tempdir(), sprintf("%s/data-raw/%s.R", pname, dataset)))

    yml <- yml_add_files(file.path(tempdir(), pname), c(sprintf("%s.R", dataset)))
    yml <- yml_add_objects(yml, dataset)
    yml_write(yml)

    package_build(file.path(tempdir(), pname))
  }

  changeName <- function(old_dataset_name, new_dataset_name, pname){
    process_path <- file.path(tempdir(), sprintf("%s/data-raw/%s.R", pname, old_dataset_name))
    fil <- c(readLines(process_path), sprintf("%s <- %s", new_dataset_name, old_dataset_name))
    writeLines(fil, process_path)

    yml <- yml_remove_objects(file.path(tempdir(), pname), old_dataset_name)
    yml <- yml_add_objects(yml, new_dataset_name)
    yml_write(yml)

    package_build(file.path(tempdir(), pname))
  }

  removeName <- function(dataset_name, script, pname){
    process_path <- file.path(tempdir(), sprintf("%s/data-raw/%s", pname, script))
    fil <- gsub(paste0("^", dataset_name, ".+$"), "", readLines(process_path))
    writeLines(fil, process_path)

    yml <- yml_remove_objects(file.path(tempdir(), pname), dataset_name)
    yml_write(yml)

    package_build(file.path(tempdir(), pname))
  }

  ## test change when one object is present
  pname <- "nameChangeTest1"
  datapackage_skeleton(pname, tempdir(), force = TRUE)
  addData("mtcars", pname)
  expect_no_error(changeName("mtcars", "mtcars2", pname))
  expect_error(removeName("mtcars2", "mtcars.R", pname), "exiting")

  ## test change when two objects are present
  pname <- "nameChangeTest2"
  datapackage_skeleton(pname, tempdir(), force = TRUE)
  addData("mtcars", pname)
  addData("iris", pname)
  expect_no_error(changeName("mtcars", "mtcars2", pname))
  expect_no_error(removeName("mtcars2", "mtcars.R", pname))

  ## test change when more than 2 objects are present
  pname <- "nameChangeTest3"
  datapackage_skeleton(pname, tempdir(), force = TRUE)
  addData("mtcars", pname)
  addData("iris", pname)
  addData("ToothGrowth", pname)
  expect_no_error(changeName("mtcars", "mtcars2", pname))
  expect_no_error(removeName("mtcars2", "mtcars.R", pname))

})
