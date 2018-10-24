## ---- echo = FALSE, results = 'hide', eval = rmarkdown::pandoc_available()----
library(DataPackageR)
library(yaml)
yml <- DataPackageR::construct_yml_config(code = "subsetCars.Rmd", data = "cars_over_20")

## ---- echo = FALSE, comment="", eval = rmarkdown::pandoc_available()-----
cat(yaml::as.yaml(yml))

## ---- eval = rmarkdown::pandoc_available()-------------------------------
# Note this is done by the datapackage_skeleton. 
# The user doesn't usually need to call 
# construct_yml_config()
yml <- DataPackageR::construct_yml_config(
  code = "subsetCars.Rmd",
  data = "cars_over_20"
  )

## ----eval=FALSE----------------------------------------------------------
#  # returns an r object representation of
#  # the config file.
#  mtcars20_config <- yml_find(
#    file.path(tempdir(),"mtcars20")
#    )

## ---- comment="", eval = rmarkdown::pandoc_available()-------------------
  yml_list_objects(yml)

## ---- comment="", eval = rmarkdown::pandoc_available()-------------------
  yml_list_files(yml)

## ---- comment="", echo = 1, eval = rmarkdown::pandoc_available()---------
yml_disabled <- yml_disable_compile(
    yml,
    filenames = "subsetCars.Rmd")
cat(as.yaml(yml_disabled))

## ---- comment="", echo = 1, eval = rmarkdown::pandoc_available()---------
yml_enabled <- yml_enable_compile(
    yml,
    filenames = "subsetCars.Rmd")
cat(as.yaml(yml_enabled))

## ---- comment="", echo = 1, eval = rmarkdown::pandoc_available()---------
yml_twofiles <- yml_add_files(
    yml,
    filenames = "anotherFile.Rmd")
# cat(as.yaml(yml_twofiles))

## ---- comment="", echo = 1, eval = rmarkdown::pandoc_available()---------
yml_twoobj <- yml_add_objects(
    yml_twofiles,
    objects = "another_object")
# cat(as.yaml(yml_twoobj))

## ---- comment="", echo = 1, eval = rmarkdown::pandoc_available()---------
yml_twoobj <- yml_remove_files(
    yml_twoobj,
    filenames = "anotherFile.Rmd")
# cat(as.yaml(yml_twoobj))

## ---- comment="", echo = 1, eval = rmarkdown::pandoc_available()---------
yml_oneobj <- yml_remove_objects(
    yml_twoobj,
    objects = "another_object")
# cat(as.yaml(yml_oneobj))

## ---- eval = FALSE-------------------------------------------------------
#  yml_write(yml_oneobj, path = "path_to_package")

