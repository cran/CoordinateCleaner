% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoordinateCleaner-package.R
\docType{data}
\name{buffsea}
\alias{buffsea}
\title{Global Coastlines buffered by -1 degree}
\source{
\url{https://www.naturalearthdata.com/downloads/10m-physical-vectors/}
}
\description{
A \code{SpatVector} with global coastlines, with a -1 degree buffer to extent coastlines as alternative reference for \code{\link{cc_sea}}. Can be useful to identify marine species on land without flagging records in estuaries, etc.
}
\examples{

data("buffsea")
}
\keyword{gazetteers}
