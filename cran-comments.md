## Release 0.15.4

This release adds support to automatically write a Dockerfile for the data package being created.

## Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-08-14 r75143)
* local OS X install (x86_64-apple-darwin16.7.0), R 3.5.1  (2018-08-14 r75143) with pandoc missing.
* ubuntu  14.04.05 LTS (on travis-ci) R 3.5.0
* ubuntu  14.04.05 LTS (on travis-ci) R Under development (unstable) (2018-08-08 r75087)
* Windows Server 2012 R2 x64 (build 9600) (under Appveyor) R 3.5.1 Patched (2018-08-06 r75070)
* Windows (via Winbuilder) R 3.5.1 R-release and  3.6.0 R-devel.
* build on: Ubuntu 18.04.1 LTS x86_64-pc-linux-gnu (64-bit) (via OpenCPU CI) R version 3.5.1 (2018-07-02)

## R CMD check results

There were no ERRORs or WARNINGs or NOTEs

There was one NOTE when the package was tested with pandoc absent:

NOTE:
Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.


## Downstream dependencies

The package has no reverse dependencies.
