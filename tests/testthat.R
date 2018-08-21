library(testthat)
library(DataPackageR)
options("DataPackageR_interact" = FALSE)
#Test only if pandoc is available.
if (rmarkdown::pandoc_available()) {
  test_check("DataPackageR")
}
options("DataPackageR_interact" = interactive())
