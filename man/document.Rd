% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processData.R
\name{document}
\alias{document}
\title{Build documentation for a data package using DataPackageR.}
\usage{
document(path = ".", install = FALSE, ...)
}
\arguments{
\item{path}{\code{character} the path to the data package source root.}

\item{install}{\code{logical} install the package. (default FALSE)}

\item{...}{additional arguments to \code{install}}
}
\value{
Called for side effects. Returns TRUE on successful exit.
}
\description{
Build documentation for a data package using DataPackageR.
}
\examples{
# A simple Rmd file that creates one data object
# named "tbl".
if(rmarkdown::pandoc_available()){
f <- tempdir()
f <- file.path(f,"foo.Rmd")
con <- file(f)
writeLines("```{r}\n tbl = data.frame(1:10) \n```\n",con=con)
close(con)
\donttest{
# construct a data package skeleton named "MyDataPackage" and pass
# in the Rmd file name with full path, and the name of the object(s) it
# creates.

pname <- basename(tempfile())
datapackage_skeleton(name=pname,
   path=tempdir(),
   force = TRUE,
   r_object_names = "tbl",
   code_files = f)

# call package_build to run the "foo.Rmd" processing and
# build a data package.
package_build(file.path(tempdir(), pname), install = FALSE)
document(path = file.path(tempdir(), pname), install = FALSE)
}
}
}
