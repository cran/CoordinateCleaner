% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf_age.R
\name{cf_age}
\alias{cf_age}
\title{Identify Fossils with Outlier Age}
\usage{
cf_age(
  x,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  min_age = "min_ma",
  max_age = "max_ma",
  taxon = "accepted_name",
  method = "quantile",
  size_thresh = 7,
  mltpl = 5,
  replicates = 5,
  flag_thresh = 0.5,
  uniq_loc = FALSE,
  value = "clean",
  verbose = TRUE
)
}
\arguments{
\item{x}{data.frame. Containing fossil records with taxon names, ages, 
and geographic coordinates.}

\item{lon}{character string. The column with the longitude coordinates.
To identify unique records if \code{uniq_loc  = TRUE}.
Default = \dQuote{decimalLongitude}.}

\item{lat}{character string. The column with the longitude coordinates.
Default = \dQuote{decimalLatitude}. To identify unique records if \code{uniq_loc  = T}.}

\item{min_age}{character string. The column with the minimum age. Default
= \dQuote{min_ma}.}

\item{max_age}{character string. The column with the maximum age. Default
= \dQuote{max_ma}.}

\item{taxon}{character string. The column with the taxon name. If
\dQuote{}, searches for outliers over the entire dataset, otherwise per
specified taxon. Default = \dQuote{accepted_name}.}

\item{method}{character string.  Defining the method for outlier
selection.  See details. Either \dQuote{quantile} or \dQuote{mad}.  Default
= \dQuote{quantile}.}

\item{size_thresh}{numeric.  The minimum number of records needed for a
dataset to be tested. Default = 10.}

\item{mltpl}{numeric. The multiplier of the interquartile range
(\code{method == 'quantile'}) or median absolute deviation (\code{method ==
'mad'}) to identify outliers. See details.  Default = 5.}

\item{replicates}{numeric. The number of replications for the distance
matrix calculation. See details.  Default = 5.}

\item{flag_thresh}{numeric.  The fraction of passed replicates necessary to pass the test. 
See details. Default = 0.5.}

\item{uniq_loc}{logical.  If TRUE only single records per location and time
point (and taxon if \code{taxon} != "") are used for the outlier testing.
Default = T.}

\item{value}{character string.  Defining the output value. See value.}

\item{verbose}{logical. If TRUE reports the name of the test and the number
of records flagged.}
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame}
  containing the records considered correct by the test (\dQuote{clean}) or a
  logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test
  failed/potentially problematic . Default = \dQuote{clean}.
}
\description{
Removes or flags records that are temporal outliers based on
interquantile ranges.
}
\details{
The outlier detection is based on an interquantile range test. A temporal
distance matrix among all records is calculated based on a single point selected by random
between the minimum and maximum age for each record. The mean distance for
each point to all neighbours is calculated and the sum of these distances
is then tested against the interquantile range and flagged as an outlier if
\eqn{x > IQR(x) + q_75 * mltpl}. The test is replicated \sQuote{replicates}
times, to account for dating uncertainty. Records are flagged as outliers
if they are flagged by a fraction of more than \sQuote{flag.thresh}
replicates. Only datasets/taxa comprising more than \sQuote{size_thresh}
records are tested. Distance are calculated as Euclidean distance.
}
\note{
See \url{https://ropensci.github.io/CoordinateCleaner/} for more
details and tutorials.
}
\examples{

minages <- c(runif(n = 11, min = 10, max = 25), 62.5)
x <- data.frame(species = c(letters[1:10], rep("z", 2)),
                min_ma = minages,
                max_ma = c(minages[1:11] + runif(n = 11, min = 0, max = 5), 65))

cf_age(x, value = "flagged", taxon = "")

# unique locations only
x <- data.frame(species = c(letters[1:10], rep("z", 2)),
                decimalLongitude = c(runif(n = 10, min = 4, max = 16), 75, 7),
                decimalLatitude = c(runif(n = 12, min = -5, max = 5)),
                min_ma = minages, 
                max_ma = c(minages[1:11] + runif(n = 11, min = 0, max = 5), 65))

cf_age(x, value = "flagged", taxon = "", uniq_loc = TRUE)

}
\seealso{
Other fossils: 
\code{\link{cf_equal}()},
\code{\link{cf_outl}()},
\code{\link{cf_range}()},
\code{\link{write_pyrate}()}
}
\concept{fossils}
\keyword{Coordinate}
\keyword{Fossil}
\keyword{Temporal}
\keyword{cleaning}
