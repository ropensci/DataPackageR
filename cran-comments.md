## Minor release 0.15.6
* I have addresed remaining issues in the vignette and the test cases that were still trying write to the user's library causing failures on debian-gcc on CRAN for version 0.15.5
* I've tested with a read-only system and user R library to simulate the errors on the debian system.
* There is one remaining NOTE that is due to the way we load and test a package (using loadNamespace) built by DataPackageR in the vignettes.

## Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2019-03-07 r76210)
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2019-03-07r76210) without pandoc.
* ubuntu  16.04.15 LTS (on travis-ci) R 3.5.2 (2017-01-27)
* ubuntu  16.04.15 LTS (on travis-ci) R Under development (unstable) (2019-03-11 r76221)
* Ubuntu Ubuntu 18.04.2 LTS x86_64-pc-linux-gnu (64-bit) (via OpenCPU CI) R 3.5.2  (2019-03-11) 
* Windows (via Winbuilder) R version 3.5.3 (2019-03-11) 
* Windows (via Winbuilder) R Under development (unstable) (2019-03-09 r76216)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (via R-hub)


## R CMD check results

There were no ERRORs or WARNINGs 

There was 1 NOTE:
N  checking for unstated dependencies in vignettes ...
   'library' or 'require' call not declared from: ‘mtcars20’

There was 1 NOTE when pandoc was absent:

NOTE:
Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.

## Downstream dependencies

The package has no reverse dependencies.
