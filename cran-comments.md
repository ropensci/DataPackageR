## Release 0.15.4
This minor release fixes bugs and introduces new features.

* Reduce the console output from logging. (ropensci/DataPackageR/issues/50)
* Create a new logger that logs at different thresholds to console and to file. (ropensci/DataPackageR/issues/50)
* Add use_raw_dataset(), use_processing_script(), and use_data_object() APIs for interactive package construction.
* Default on package_build is now install=FALSE.
* Hide console output from Rmd render.
* Nicer messages describing the data sets that are created on package build.(ropensci/DataPackageR/issues/51)
* Write deleted, changed, and added data objects to the NEWS file automatically.
* Add option to overwrite (or not) via use_processing_script() and provide a warning to the user.
* Add use_ignore() API to ignore files and data sets in .Rbuildignore and .gitignore. Add ignore argument to use_raw_dataset().
* "code" argument to construct_yml_config() no longer required.
* Fix the documentation for datapackager_object_read() and "Migrating old packages".
* Vignette pdfs created by package_build() now appear in inst/doc of the data package.
* Data objects are incrementally stored during the build process, into the "render_root" directory specified in the datapackager.yml config file.


## Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-10-09 r75419)
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-10-09 r75419) without pandoc.
* ubuntu  14.04.05 LTS (on travis-ci) R 3.5.1 (2017-01-27)
* ubuntu  14.04.05 LTS (on travis-ci) R Under development (unstable) (2018-10-23 r75480)
* Platform: i386-w64-mingw32/i386 (32-bit) Running under: Windows Server 2012 R2 x64 (build 9600) (under Appveyor) R version 3.5.1 Patched (2018-08-06 r75070)
* build on: Ubuntu 18.04.1 LTS x86_64-pc-linux-gnu (64-bit) (via OpenCPU CI) R version 3.5.1 (2018-07-02)
* Windows (via Winbuilder) R 3.5.1 R-release and  3.6.0 R-devel.
* Windows 10.0.1439 with R 3.5.0 (2018-04-23 r74626)

## R CMD check results

There were no ERRORs or WARNINGs 

There was 1 NOTE when pandoc was absent:

NOTE:
Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.

## Downstream dependencies

The package has no reverse dependencies.
