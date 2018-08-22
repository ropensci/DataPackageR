# DataPackageR 0.15.3
* conditional tests when pandoc is missing (ropensci/DataPackager/issues/46)
* add use_data_object and use_processing_script (ropensci/DataPackager/issues/44)

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
- Output of "next steps" for user when pakcage is built
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
