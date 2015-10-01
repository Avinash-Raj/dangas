\name{dangas-package}
\alias{dangas-package}
\alias{dangas}
\docType{package}
\title{
Regular Expression Based String Manipulation Tools
}
\description{
This would simplify the task of using regex for string manipulations such as substring extraction,removal.
}
\details{
\tabular{ll}{
Package: \tab dangas\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-09-29\cr
License: \tab GPL-2\cr
}

}
\author{
Avinash

Maintainer: Avinash <avistylein3105@gmail.com>

}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
extract_a("<?", "?>", "foo <?php info?> bar")
extract_a("<?", "?>", "foo <?php info?> bar", delim = TRUE)
extract_all_a("<?", "?>", "foo <?php info?> bar <?r info?>")
extract_all_a("<?", "?>", "foo <?php info?> bar <?r info?>", delim = TRUE)
}
