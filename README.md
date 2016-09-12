
<!-- README.md is generated from README.Rmd. Please edit that file -->
Preprocess Data
===============

What and why?
-------------

Different assays require different forms of preprocessing and data tidying. Data may need to be cleaned, standardized, QCd, and any number of other manipulations applied before it is ready for analysis.

Even if a data management center handles quality control and standardization, the analyst may have personal preferences how the data is formatted for analysis.

This package is designed to simplify centralize tasks associated with tidying a variety of data sets arising from a single project or study.

It provides a mechanism to collect, organize and version the data munging scripts used to process incoming data into analytical data sets. The package runs these scripts to perform the data munging writes out analytical data sets, which are combined with documentation, and built into a new R package. The package and the included data are versioned. If updated data arrive, the package can be rebuilt, and the data version incremented to reflect the changes. Withitn the data cleaning code, analysts are free to use whatever tools they are comfortable with.

Usage
-----

Set up a new data package.

``` r
library(preprocessData)
setwd("/tmp")
preprocessData::datapackage.skeleton("MyNewStudy")
```

    Creating directories ...
    Creating DESCRIPTION ...
    Creating NAMESPACE ...
    Creating Read-and-delete-me ...
    Saving functions and data ...
    Making help files ...
    Done.
    Further steps are described in './MyNewStudy/Read-and-delete-me'.
    Adding DataVersion string to DESCRIPTION
    Creating data and data-raw directories

The above code creates a directory "MyNewStudy" with the skeleton of a data package.

The `DESCRIPTION` file should be filled out to describe your package. A new `DataVersion` string now appears in that file. It needs to be incremented if the package data changes.

`Read-and-delete-me` has some helpful instructions on how to proceed.

The data-raw directory is where you will place your data cleaning code. The contents of this directory are:

    MyNewStudy/data-raw
    └── datasets.R

You will edit `datasets.R`. This files effectively sources your data munging scripts. You will add them there. Data munging scripts can read data from anywhere, but it is good practice to have your "raw" data live under `/inst/extdata`.

Here are the contents on `datasets.R`:

    library(rmarkdown)
    render('myPreprocessingCode.Rmd',envir=topenv(),output_dir='../inst/extdata/Logfiles',intermediates_dir='../inst/extdata/Logfiles',clean=FALSE)
    keepDataObjects('mydataset')

    #' MyNewStudy
    #' A data package for study MyNewStudy
    #' @docType package
    #' @aliases MyNewStudy-package
    #' @title MyNewStudy
    #' @name MyNewStudy
    #' @description a description of the package.
    #' @details Additional details.
    #' @import data.table
    #' @seealso \link{mydataset}
    NULL

    #' Data from an assay, entitled mydataset
    #'@name mydataset
    #'@docType data
    #'@title Data from an assay.
    #'@format a \code{data.table} containing the following fields
    #'\describe{
    #'\item{column_name}{description}
    #'\item{column_name_2}{description}
    #'}
    #'@source Describe the source of the data (i.e. lab, etc)
    #'@seealso \link{MyNewStudy}

We look at this piece by piece.

### Data processing scripts

First, we load the rmarkdown package and then render a file called `myPreprocessingCode.Rmd`.

-   This file does not exist, you will create it (You may change the name, you may add additional files).
-   This should be an Rmarkdown file, that combines text and R code.
-   When run, it should perform the processing of your data sets(s).

The product of this particular script will be an html document that serves as a log of how the data were processed.

-   The build process (decribed later) will store the html output under `inst/Log`.

The most important product of processing script is one or more R objects.

-   The script should generate one or more R objects storing your data in the your desired format.

-   The call to `keepDataObjects()` tells the build process which objects should be retained and stored as part of the data package.
-   In this case, our script should produce an object called `mydataset`.
-   `keepDataObjects('mydataset')` tells the build process the name of the object to store in the package

If your data cleaning script produces three tables named `assay3` `assay2` and `assay3`, then the call would be \`keepDataObjects(c('assay1','assay2','assay3')).

-   You do not need to save these tables or objects to disk.
-   They just need to exist in memory when the processing script is finished running.
-   The build process will match these names to objects in memory and to existing documentation (see below).
-   If everything is in order, they will be included in the built package.

### Object Documentation

Next are two `roxygen` blocks.

The first documents the package, and the second documents the data objects produced by the package.

These should be filled in appropriately.

-   If you have multiple data objects, you can copy the second roxygen block to document the other objects.
-   It is good practice to
    -   Document all the columns of tables in your data set.
    -   Include the source of the data (i.e. what laboratory or person provided the data).

### Build your package.

Once your scripts are in place and the data objects are documented, you build the package.

-   Run the build process.

``` r{}
# Within the package directory
preprocessData:::buildDataSetPackage(".") #note for a first build this may need to be run twice.
```

You will see a lot of output. If there are errors, the script will notify you of any problems.

-   Correct any errors and rerun the build process.

If everything goes smoothly, you will have a new package built in the parent directory.

This can be distributed, installed using `R CMD INSTALL`, and data sets loaded using R's standard `data()` call.

### Data versioning

The preprocessData package calculates an md5 checksum of each data object it stores, and keeps track of them in a file called `DATADIGEST`.

-   Each time the package is rebuilt, the md5 sums of the new data objects are compared against the DATADIGEST.
-   If they don't match, the build process checks that the `DataVersion` string has been incremented in the `DESCRIPTION` file.
-   If it has not the build process will exit and produce an error message.

### Benefits

Your downstream data analysis can depend on a specific version of your data package (for example by tesing the `packageVersion()` string);

``` r{}
if(packageVersion("MyNewStudy") != "1.0.0")
  stop("The expected version of MyNewStudy is 1.0.0, but ",packageVersion("MyNewStudy")," is installed! Analysis results may differ!")
```

The preprocessData packge also provides `datasetVersion()` to extract the data set version information.

You should also place the data package source directory under `git` version control. This allows you to version control your data processing code.
