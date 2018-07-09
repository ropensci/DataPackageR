## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  eval = TRUE
)

## ----minimal_example, results='hide'-------------------------------------
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
DataPackageR::datapackage_skeleton(
  "mtcars20",
  force = TRUE,
  code_files = processing_code,
  r_object_names = "cars_over_20",
  path = tempdir()
  ) 

## ----dirstructure,echo=FALSE---------------------------------------------
library(data.tree)
df = data.frame(pathString = file.path(
  "mtcars20",
  list.files(
  file.path(tempdir(), "mtcars20"),
  include.dirs = TRUE,
  recursive = TRUE
  )
  ))
as.Node(df)

## ---- echo=FALSE---------------------------------------------------------
cat(yaml::as.yaml(yaml::yaml.load_file(file.path(tempdir(),"mtcars20","datapackager.yml"))))

## ----eval=TRUE-----------------------------------------------------------
# Run the preprocessing code to build cars_over_20
# and reproducibly enclose it in a package.
DataPackageR:::package_build(file.path(tempdir(),"mtcars20"))

## ---- echo=FALSE---------------------------------------------------------
df = data.frame(pathString = file.path(
  "mtcars20",
  list.files(
  file.path(tempdir(), "mtcars20"),
  include.dirs = TRUE,
  recursive = TRUE
  )
  ))
  as.Node(df)

