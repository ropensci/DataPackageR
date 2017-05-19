@echo off

rem set arg0=%~dp0
rem shift


rem %R_HOME%/bin/Rscript --vanilla %arg0%DataPackageR %*

set rhome=%R_HOME%
set rhome=%rhome:/=\%

%rhome%\bin\R --vanilla --slave -e "DataPackageR:::.DataPackageR()" --args %*
