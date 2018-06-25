---
title: "Using DataPackageR"
author: "Greg Finak <gfinak@fredhutch.org>"
date: "2018-06-25"
output: 
  rmarkdown::html_vignette:
    keep_md: TRUE

vignette: >
  %\VignetteIndexEntry{A quick guide to using DataPackageR}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
  \usepackage{graphicx}
---


# NEWS

- Added the `render_root` property to the YAML configuration. Specifies where `render()` processing is done, instead of the `data-raw` directory.

# DataPackageR

A package to reproducibly process raw data into packaged, analysis-ready data sets.

 [![Build Status](https://travis-ci.org/RGLab/DataPackageR.svg?branch=master)](https://travis-ci.org/RGLab/DataPackageR)
 [![Coverage status](https://codecov.io/gh/RGLab/DataPackageR/branch/master/graph/badge.svg)](https://codecov.io/github/RGLab/DataPackageR?branch=master)
 [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/RGLab/DataPackageR?branch=master&svg=true)](https://ci.appveyor.com/project/RGLab/DataPackageR)
[![DOI](https://zenodo.org/badge/29267435.svg)](https://doi.org/10.5281/zenodo.1292095)


## Preprint and publication.

The publication describing the package is now available at [Gates Open Research](https://gatesopenresearch.org/articles/2-31/v1).

The preprint is on [biorxiv](https://doi.org/10.1101/342907).

## Goals

You have raw data that needs to be tidied and otherwise processed into a standardized analytic data set (a data set that is ready for analysis). 
You want to do the processing using best practices for reproducible research. 

### The current state of affairs

Normally, you'll write some code that does the tidying and outputs a tidy data set. 
If you want to distribute your data set, you can put it in an R package. 
The preferred mechanism is to place your data tidying code in `data-raw` in the package source tree and use the `devtools` package (specifically `devtools::use_data`) to save the data into the `data` directory. The build process will include your data set in the final package.
You'll also have to remember to document the data set in `roxygen`, and write a vignette showing how to use the data. 
For version control and easy distribution you might post the package on github. 

### Scaling up

The process outlined works well for smaller data sets. 
It can be a hassle if you have complex data that change frequently (as is often the case in biology, where data trickle in from collaborators and follow-up experiments), or more generally if you have large data sets where raw data can't be distributed as part of the package source due to size restrictions (e.g. FASTQ files for sequencing, FCS files for flow cytometry, or other "omics" data).

### DataPackageR

The `DataPackageR` package simplifies bundling of code, data and documentation into a single R package that can be versioned and distributed.
The `datapackage.skeleton()` API lets you point `DataPackageR` at your data processing code (in the form of Rmd and / or R files). These are expected to produce `data objects` to be stored in the final package. The names of these are also passed to `datapackage.skeleton()`. This produces the necessary package structure, and populations a `datapackager.yml` configuration file used by the build process.

The `package_build()` API runs the processing code specified in the `.yml` files and produces html reports of the processing as **package vignettes**. It also builds boilerplate `roxygen` documentation of the R objects specified in the `.yml`, computes checksums of stored R objects and version tags the entire data set collection.

If raw data changes, the user can rebuild the data sets in the R package with subsequent calls to `package_build()` which will re-run the processing, compare the cheksums of new R objects against those currently stored in the package. 
Any changes force an increment of the `Dataversion` string in the package DESCRIPTION file. 
When the package is installed, data sets can be accessed via the standard `data()` API, package vignettes describing the data processing can be accessed via `vignette()`, documentation via `?`, and the data version via `dataVersion(packageName)`. 


# Installation 

The usual package installation mechanism works: 

```
library(devtools)
devtools::install_github("RGLab/DataPackageR", auth_token=NULL)
```

# Usage

Set up a new data package.

We'll set up a new data package that processes the `cars` data by subsetting it to include only measurements of stopping distances of cars at speeds greater than 20 mph. It is processed using an Rmd file located in `inst/extdata/tests/subsetCars.Rmd` that produces a new object called `cars_over_20`. The package will be called `Test`. The work will be done in the system `/tmp` directory.


```r
library(data.tree)
library(DataPackageR)
tmp = normalizePath(tempdir())
processing_code = system.file("extdata","tests","subsetCars.Rmd",package="DataPackageR")
print(processing_code)
[1] "/Library/Frameworks/R.framework/Versions/3.5/Resources/library/DataPackageR/extdata/tests/subsetCars.Rmd"
setwd(tmp)
DataPackageR::datapackage.skeleton("Test", 
                                   force=TRUE, 
                                   code_files = processing_code, 
                                   r_object_names = "cars_over_20") # cars_over_20 is an R object 
Creating directories ...
Creating DESCRIPTION ...
Creating NAMESPACE ...
Creating Read-and-delete-me ...
Saving functions and data ...
Making help files ...
Done.
Further steps are described in './Test/Read-and-delete-me'.
Adding DataVersion string to DESCRIPTION
Creating data and data-raw directories
configuring yaml file
                                                                    # created in the Rmd file.
```

### Package skeleton structure 

This has created a directory, "Test" with the skeleton of a data package.

The `DESCRIPTION` file should be filled out to describe your package. It contains a new `DataVersion` string, and the
revision is automatically incremented if the packaged data changes.

`Read-and-delete-me` has some helpful instructions on how to proceed. 

The `data-raw` directory is where the data cleaning code (`Rmd`) files reside.
The contents of this directory are:


```
                                              levelName
1  Test                                                
2   ¦--git-commit-message-8fc8136dde8d.txt             
3   ¦--libloc_213_722ad5f9e07c7fe1.rds                 
4   ¦--preview-8fc8122e04f2.dir                        
5   ¦   °--README.html                                 
6   ¦--preview-8fc82b0f632f.dir                        
7   ¦   ¦--usingDataPackageR.html                      
8   ¦   °--usingDataPackageR.md                        
9   ¦--preview-8fc83fb8baef.dir                        
10  ¦   ¦--usingDataPackageR.html                      
11  ¦   °--usingDataPackageR.md                        
12  ¦--preview-8fc84f21a9bf.dir                        
13  ¦   °--README.html                                 
14  ¦--preview-8fc878f8c197.dir                        
15  ¦   ¦--usingDataPackageR.html                      
16  ¦   °--usingDataPackageR.md                        
17  ¦--preview-8fc8b46b531.dir                         
18  ¦   ¦--usingDataPackageR.html                      
19  ¦   °--usingDataPackageR.md                        
20  ¦--rs-graphics-e124d6bc-6a4c-4068-8e4a-ae9c8a2eb1a7
21  ¦   ¦--empty.png                                   
22  ¦   °--INDEX                                       
23  °--Test                                            
24      ¦--data-raw                                    
25      ¦   °--subsetCars.Rmd                          
26      ¦--datapackager.yml                            
27      ¦--DESCRIPTION                                 
28      °--Read-and-delete-me                          
```

`datapackager.yml` can be edited as necessary to include additional processing scripts (which should be placed in `data-raw`), and raw data should be located under under `/inst/extdata`. It should be copied into that path and the data munging scripts edited to read from there.

### Yaml configuration 

Here are the contents of `datapackager.yml`:


```
configuration:
  files:
    subsetCars.Rmd:
      name: subsetCars.Rmd
      enabled: yes
  objects: cars_over_20
  render_root: /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/200981
```

It includes a `files` property that has an entry for each script, with the `name:` and `enabled:` keys for each file. The `objects` property  lists the data objects produced by the scripts.

The `render_root` property specifies the directory where the Rmd files are rendered. If temporary objects are produced during the processing, they will appear in this directory without polluting the package source tree. A temporary directory is used by default.

### Build your package.

Once your scripts are in place and the data objects are documented, you build the package.
  
To run the build process:


```r
# Within the package directory
setwd(tmp)
DataPackageR:::package_build("Test") 
INFO [2018-06-25 14:19:36] Logging to /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/Test/inst/extdata/Logfiles/processing.log
INFO [2018-06-25 14:19:36] Processing data
INFO [2018-06-25 14:19:36] Reading yaml configuration
INFO [2018-06-25 14:19:36] Found /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/Test/data-raw/subsetCars.Rmd
INFO [2018-06-25 14:19:36] Processing 1 of 1: /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/Test/data-raw/subsetCars.Rmd


processing file: subsetCars.Rmd
  |                                                                         |                                                                 |   0%  |                                                                         |.........                                                        |  14%
  ordinary text without R code

  |                                                                         |...................                                              |  29%
label: setup (with options) 
List of 1
 $ include: logi FALSE

  |                                                                         |............................                                     |  43%
  ordinary text without R code

  |                                                                         |.....................................                            |  57%
label: cars
  |                                                                         |..............................................                   |  71%
  ordinary text without R code

  |                                                                         |........................................................         |  86%
label: unnamed-chunk-10
  |                                                                         |.................................................................| 100%
  ordinary text without R code
output file: subsetCars.knit.md
/usr/local/bin/pandoc +RTS -K512m -RTS subsetCars.utf8.md --to html4 --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash+smart --output /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/Test/inst/extdata/Logfiles/subsetCars.html --email-obfuscation none --self-contained --standalone --section-divs --template /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable 'theme:bootstrap' --include-in-header /var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T//RtmpLMrqrN/rmarkdown-str8fc821b62338.html --mathjax --variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' 

Output created: inst/extdata/Logfiles/subsetCars.html
INFO [2018-06-25 14:19:36] 1 required data objects created by subsetCars.Rmd
INFO [2018-06-25 14:19:36] Saving to data
INFO [2018-06-25 14:19:36] Copied documentation to R/Test.R
* Creating `vignettes`.
* Adding `inst/doc` to ./.gitignore
INFO [2018-06-25 14:19:36] Done
INFO [2018-06-25 14:19:36] Building documentation
First time using roxygen2. Upgrading automatically...
Updating roxygen version in /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/Test/DESCRIPTION
Writing NAMESPACE
Writing Test.Rd
Writing cars_over_20.Rd
INFO [2018-06-25 14:19:36] Building package
'/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
  --no-environ --no-save --no-restore --quiet CMD build  \
  '/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/Test'  \
  --no-resave-data --no-manual --no-build-vignettes 

[1] "/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/Test_1.0.tar.gz"
```

### Logging the build process

DataPackageR uses the `futile.logger` pagckage to log progress. If there are errors in the processing, the script will notify you via logging to console and to  `/private/tmp/Test/inst/extdata/Logfiles/processing.log`. Errors should be corrected and the build repeated.

If everything goes smoothly, you will have a new package built in the parent directory. In this case we have a new package 
`Test_1.0.tar.gz`. When the package is installed, it will contain a vignette `subsetCars` that can be loaded using the `vignette()` API. The vignette will detail the processing performed by the `subsetCars.Rmd` processing script. 

### The package source directory after building


```
                         levelName
1  Test                           
2   ¦--data-raw                   
3   ¦   ¦--documentation.R        
4   ¦   ¦--subsetCars.knit.md     
5   ¦   ¦--subsetCars.Rmd         
6   ¦   °--subsetCars.utf8.md     
7   ¦--data                       
8   ¦   °--cars_over_20.rda       
9   ¦--DATADIGEST                 
10  ¦--datapackager.yml           
11  ¦--DESCRIPTION                
12  ¦--inst                       
13  ¦   ¦--doc                    
14  ¦   ¦   ¦--subsetCars.html    
15  ¦   ¦   °--subsetCars.Rmd     
16  ¦   °--extdata                
17  ¦       °--Logfiles           
18  ¦           ¦--processing.log 
19  ¦           °--subsetCars.html
20  ¦--man                        
21  ¦   ¦--cars_over_20.Rd        
22  ¦   °--Test.Rd                
23  ¦--NAMESPACE                  
24  ¦--R                          
25  ¦   °--Test.R                 
26  ¦--Read-and-delete-me         
27  °--vignettes                  
28      °--subsetCars.Rmd         
```

#### Details

A number of things have changed. The subsetCars processing script now appears under `/vignettes` and `inst/doc` as a processed html report so that it will be available to view via `vignette()` once the package is installed. 
`inst/extdata/Logfiles` contains a log file of the entire build process as well as intermediate files created while parsing the R / Rmd code. Documentation Rd files appear in `/man`, these should be edite to provide further details on the data objects in the package. The data objects are stored under `/data` where we see `cars_over_20.rda`, the object we initially specified in `datapackager.yml`.


## Versioning data objects

The DataPackageR package calculates an md5 checksum of each data object it stores, and keeps track of them in a file
called `DATADIGEST`.

- Each time the package is rebuilt, the md5 sums of the new data objects are compared against the DATADIGEST.
- If they don't match, the build process checks that the `DataVersion` string has been incremented in the `DESCRIPTION` file.
- If it has not the build process will exit and produce an error message.

### DATADIGEST


The `DATADIGEST` file contains the following:


```
DataVersion: 0.1.0
cars_over_20: 3ccb5b0aaa74fe7cfc0d3ca6ab0b5cf3
```


### DESCRIPTION

The description file has the new `DataVersion` string.


```
Package: Test
Type: Package
Title: What the package does (short line)
Version: 1.0
Date: 2018-19-25
Author: Who wrote it
Maintainer: Who to complain to <yourfault@somewhere.net>
Description: More about what it does (maybe more than one line)
License: What license is it under?
DataVersion: 0.1.0
Suggests: knitr,
    rmarkdown
VignetteBuilder: knitr
RoxygenNote: 6.0.1
```

### Next steps

Your downstream data analysis can depend on a specific version of your data package (for example by tesing the `packageVersion()` string);

```r{}
if(DataPackageR::packageVersion("MyNewStudy") != "1.0.0")
  stop("The expected version of MyNewStudy is 1.0.0, but ",packageVersion("MyNewStudy")," is installed! Analysis results may differ!")
```

The DataPackageR packge also provides `datasetVersion()` to extract the data set version information. 

You should also place the data package source directory under `git` version control.
This allows you to version control your data processing code. 

### Why not use R CMD build?

If the processing script is time consuming or the data set is particularly large, then `R CMD build` would run the code each time the package is installed. In such cases, raw data may not be available, or the environment to do the data processing may not be set up for each user of the data. In such cases, DataPackageR provides a mechanism to decouple data processing from package building/installation for downstream users of the data.


## Partial builds and migrating old data packages.

Version 1.12.0 has moved away from controlling the build process using `datasets.R` and an additional `masterfile` argument. The build process is now controlled via a `datapackager.yml` configuration file located in the package root directory. 

You can migrate an old package by constructing such a config file using the `construct_yml_config()` API.


```r
#assume I have file1.Rmd and file2.R located in /data-raw, and these create 'object1' and 'object2' respectively.

config = construct_yml_config(code = c("file1.Rmd","file2.R"), data = c("object1","object2"))
cat(as.yaml(config))
configuration:
  files:
    file1.Rmd:
      name: file1.Rmd
      enabled: yes
    file2.R:
      name: file2.R
      enabled: yes
  objects:
  - object1
  - object2
  render_root: /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/540215
```

`config` is a newly constructed yaml configuration object. It can be written to the package directory:


```r
path_to_package = tempdir() #pretend this is the root of our package
yml_write(config,path = path_to_package)
```

Now the package at `path_to_package` will build with version 1.12.0 or greater.

We can also perform partial builds of a subset of files in a package by toggling the `enabled` key in the config file. This can be done with the following API:


```r
config = yml_disable_compile(config,filenames = "file2.R")
cat(as.yaml(config))
configuration:
  files:
    file1.Rmd:
      name: file1.Rmd
      enabled: yes
    file2.R:
      name: file2.R
      enabled: no
  objects:
  - object1
  - object2
  render_root: /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/RtmpLMrqrN/540215
```

Where `config` is a configuration read from a data package root directory. The `config` object needs to be written back to the package root in order for the changes to take effect. The consequence of toggling a file to `enable: no` is that it will be skipped when the package is built, but the data will be retained, and the documentation will not be altered. 





