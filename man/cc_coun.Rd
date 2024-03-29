% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_coun.R
\name{cc_coun}
\alias{cc_coun}
\title{Identify Coordinates Outside their Reported Country}
\usage{
cc_coun(
  x,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  iso3 = "countrycode",
  value = "clean",
  ref = NULL,
  ref_col = "iso_a3",
  verbose = TRUE,
  buffer = NULL
)
}
\arguments{
\item{x}{data.frame. Containing geographical coordinates and species names.}

\item{lon}{character string. The column with the longitude coordinates.
Default = \dQuote{decimalLongitude}.}

\item{lat}{character string. The column with the latitude coordinates.
Default = \dQuote{decimalLatitude}.}

\item{iso3}{a character string. The column with the country assignment of
each record in three letter ISO code. Default = \dQuote{countrycode}.}

\item{value}{character string.  Defining the output value. See value.}

\item{ref}{SpatVector (geometry: polygons). Providing the geographic
gazetteer. Can be any SpatVector (geometry: polygons), but the structure
must be identical to \code{rnaturalearth::ne_countries(scale = "medium",
returnclass = "sf")}.
Default = \code{rnaturalearth::ne_countries(scale = "medium", returnclass =
"sf")}}

\item{ref_col}{the column name in the reference dataset, containing the
relevant ISO codes for matching. Default is to "iso_a3_eh" which refers to
the ISO-3 codes in the reference dataset. See notes.}

\item{verbose}{logical. If TRUE reports the name of the test and the number
of records flagged.}

\item{buffer}{numeric. Units are in meters. If provided, a buffer is
created around each country polygon.}
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame}
  containing the records considered correct by the test (\dQuote{clean}) or a
  logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test
  failed/potentially problematic . Default = \dQuote{clean}.
}
\description{
Removes or flags mismatches between geographic coordinates and additional
country information (usually this information is reliably reported with
specimens). Such a mismatch can occur for example, if latitude and longitude
are switched.
}
\note{
The ref_col argument allows to adapt the function to the structure of
  alternative reference datasets. For instance, for
  \code{rnaturalearth::ne_countries(scale = "small")}, the default will fail,
  but ref_col = "iso_a3" will work.

With the default reference, records are flagged if they fall outside
  the terrestrial territory of countries, hence records in territorial waters
  might be flagged. See \url{https://ropensci.github.io/CoordinateCleaner/}
  for more details and tutorials.
}
\examples{

\dontrun{
x <- data.frame(species = letters[1:10],
                decimalLongitude = runif(100, -20, 30),
                decimalLatitude = runif(100, 35,60),
                countrycode = "RUS")

cc_coun(x, value = "flagged")#non-terrestrial records are flagged as wrong.
}

}
\seealso{
Other Coordinates: 
\code{\link{cc_aohi}()},
\code{\link{cc_cap}()},
\code{\link{cc_cen}()},
\code{\link{cc_dupl}()},
\code{\link{cc_equ}()},
\code{\link{cc_gbif}()},
\code{\link{cc_inst}()},
\code{\link{cc_iucn}()},
\code{\link{cc_outl}()},
\code{\link{cc_sea}()},
\code{\link{cc_urb}()},
\code{\link{cc_val}()},
\code{\link{cc_zero}()}
}
\concept{Coordinates}
\keyword{Coordinate}
\keyword{cleaning}
