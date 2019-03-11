## Release 0.15.5
* This minor release fixes bugs in the build and check on debian on CRAN.
* Build failure on debian because tests were writing to a user-space library. 
* Now the code has been updated to write to a temporary library in the R's temporary directory structure.


# Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-10-09 r75419)
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-10-09 r75419) without pandoc.
* ubuntu  14.04.05 LTS (on travis-ci) R 3.5.1 (2017-01-27)
* ubuntu  14.04.05 LTS (on travis-ci) R Under development (unstable) (2018-10-23 r75480)
* Platform: i386-w64-mingw32/i386 (32-bit) Running under: Windows Server 2012 R2 x64 (build 9600) (under Appveyor) R version 3.5.1 Patched (2018-08-06 r75070)
* build on: Ubuntu 18.04.1 LTS x86_64-pc-linux-gnu (64-bit) (via OpenCPU CI) R version 3.5.1 (2018-07-02)
* Windows (via Winbuilder) R 3.5.1 R-release and  3.6.0 R-devel.
* Windows 10.0.1439 with R 3.5.0 (2018-04-23 r74626)

## Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2019-03-07 r76210)
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2019-03-07r76210) without pandoc.
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
