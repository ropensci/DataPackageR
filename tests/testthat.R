library(testthat)
library(DataPackageR)
# Test only if pandoc is available.
if (rmarkdown::pandoc_available()) {
  test_check("DataPackageR")
}
