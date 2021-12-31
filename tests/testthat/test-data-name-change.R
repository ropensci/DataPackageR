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

  pname <- "test"
  datapackage_skeleton(pname, tempdir(), force = TRUE)
  addData("mtcars", pname)
  expect_error(changeName("mtcars", "mtcars2", pname), NA)
})
