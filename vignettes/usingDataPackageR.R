## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  eval = TRUE
)

## ----minimal_example, results='hide', eval = rmarkdown::pandoc_available()----
library(DataPackageR)

# Let's reproducibly package up
# the cars in the mtcars dataset
# with speed > 20.
# Our dataset will be called cars_over_20.

# Get the code file that turns the raw data
# to our packaged and processed analysis-ready dataset.
processing_code <-
  system.file("extdata", 
              "tests",
              "subsetCars.Rmd",
              package = "DataPackageR")

# Create the package framework.
DataPackageR::datapackage_skeleton(name = "mtcars20",
  force = TRUE,
  code_files = processing_code,
  r_object_names = "cars_over_20",
  path = tempdir() 
  #dependencies argument is empty
  #raw_data_dir argument is empty.
  ) 

## ----dirstructure,echo=FALSE, eval = rmarkdown::pandoc_available()-------
library(data.tree)
df <- data.frame(pathString = file.path(
  "mtcars20",
  list.files(
  file.path(tempdir(), "mtcars20"),
  include.dirs = TRUE,
  recursive = TRUE
  )
  ))
as.Node(df)

## ---- echo=FALSE, eval = rmarkdown::pandoc_available()-------------------
cat(yaml::as.yaml(yaml::yaml.load_file(file.path(tempdir(),"mtcars20","datapackager.yml"))))

## ---- eval = rmarkdown::pandoc_available()-------------------------------
# Run the preprocessing code to build cars_over_20
# and reproducibly enclose it in a package.
dir.create(file.path(tempdir(),"lib"))
DataPackageR:::package_build(file.path(tempdir(),"mtcars20"), install = TRUE, lib = file.path(tempdir(),"lib"))

## ---- echo=FALSE, eval = rmarkdown::pandoc_available()-------------------
df <- data.frame(pathString = file.path(
  "mtcars20",
  list.files(
  file.path(tempdir(), "mtcars20"),
  include.dirs = TRUE,
  recursive = TRUE
  )
  ))
  as.Node(df)

## ----rebuild_docs, eval = rmarkdown::pandoc_available()------------------
dir.create(file.path(tempdir(),"lib")) # a temporary library directory
document(file.path(tempdir(),"mtcars20"), lib = file.path(tempdir(),"lib"))

