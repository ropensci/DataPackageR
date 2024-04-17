# DataPackageR 0.15.9

## Bug fixes

* Fix bug where `.Rprofile` options setting for `DataPackageR_interact` was overwritten upon package load
* Fix bugs where document() and package_build() left the data package attached
* Fix unit test inter-dependencies
* Make package template depend on R >= 3.5.0, suppressing `.rda` serialization warnings upon install
* Close a file connection, suppressing warnings about orphaned connections
* Fix broken test that led to archiving on CRAN
* Fix package documentation method for data packages and DataPackageR itself (r-lib/roxygen2#1491)
* Fix tests for compatibility with usethis >1.5.2

## Minor improvements

* Various maintenance tweaks
* Documentation improvements
* Update maintainer and contact info
* New global option `DataPackageR_verbose` to suppress console output, e.g. during unit testing

# DataPackageR 0.15.8
* Fix to datapackager_object_read that was causing a test to break. `get` needs to have `inherits=FALSE`. 
* Other fixes for `usethis` 1.6.0
* Fixes to tests that were failing on CRAN
* In `package_build`, remove `devtools::reload` and put `devtools::unload` and in front of install.packages
* in `document`, remove `devtools::reload` and put in `devtools::unload` and install.packages

# DataPackageR 0.15.7
* Fix test and vignette bugs related to upcoming version of usethis (1.5)


# DataPackageR 0.15.6
* Fix bug in vignette and code that writes to user space during CRAN checks.

# DataPackageR 0.15.4.900
* Fix a bug in update_news.
* Create news files if it doesn't exist.


# DataPackageR 0.15.4
* New CRAN Release

# DataPackageR 0.15.3.9000

## Features and enhancements
* Reduce the console output from logging. (ropensci/DataPackageR/issues/50)
* Create a new logger that logs at different thresholds to console and to file (ropensci/DataPackageR/issues/50)
* Default on build is not to install.
* Hide console output from Rmd render.
* Nicer messages describing data sets that are created (ropensci/DataPackageR/issues/51)
* Write deleted, changed, and added data objects to the NEWS file automatically.
* Add option to overwrite (or not) via use_processing_script. Provide warning.
* Add use_ignore() to ignore files and data sets in .Rbuildignore and .gitignore and added ignore argument to use_raw_dataset().

## Bug fixes
* code argument no longer required for construct_yml_config
* Fix the documentation for datapackager_object_read() and "Migrating old packages".
* Copy over vignettes generated as pdfs into the package inst/doc
* Data objects are incrementally stored during the build process, into the render_root directory specified in the datapackager.yml config file.

# DataPackageR 0.15.3
* conditional tests when pandoc is missing (ropensci/DataPackager/issues/46)
* add use_data_object and use_processing_script (ropensci/DataPackager/issues/44)
* allow datapackage_skeleton to be called without files or data objects for interactive construction. (ropensci/DataPackager/issues/44)

# DataPackageR 0.15.2
* Add  pandoc to SystemRequirements (ropensci/DataPackager/issues/46)
* Add use_raw_dataset() method (and tests) to add data sets to inst/extdata. interactively. (ropensci/DataPackager/issues/44)

# DataPackageR 0.15.1.9000
* Development version

# DataPackageR 0.15.1
- Fix CRAN notes.

# DataPackageR 0.15.0
- Prepare for CRAN submission.


# DataPackageR 0.14.9

- Moving towards ropensci compliance
- NEWS.md updated with description of changes to data sets when version is bumped (or new package is created).
- Output of "next steps" for user when package is built
- New `document()` function to rebuild docs from `documentation.R` in `data-raw` without rebuilding the whole package.
- Improved package test.
- R scripts processed properly into vignettes.
- Packages installed and loaded after build to make vignettes and data sets accessible in same R session.
- 

# DataPackageR 0.13.6

- Added a NEWS file.
- Cleaned up the examples.
- Snake case for all exported functions.

# DataPackageR 0.13.3

- Added the `render_root` property to the YAML configuration. Specifies where `render()` processing is done, instead of the `data-raw` directory.
