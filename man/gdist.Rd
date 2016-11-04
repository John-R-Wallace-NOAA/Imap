\name{gdist}
\alias{gdist}
\alias{gdist.total}
\title{
Geodesic distance (great circle distance) between points
}
\description{
gdist() gives the geodesic distance between two points specified by latitude/longitude using Vincenty inverse formula for ellipsoids. 
The distance is given in nautical miles (the default), meters, kilometers, or miles. gdist.total() applies gdist() to any number 
of line segments.
}
\usage{
gdist(lon.1, lat.1, lon.2, lat.2, units = "nm", a = 6378137.0, b = 6356752.3142, verbose = FALSE)

gdist.total(longlat, units = "nm", segments = TRUE, digits = 2)
}
\arguments{
  \item{lon.1}{
Longitude of the first point.
}
  \item{lat.1}{
Latitude of the first point.
}
  \item{lon.2}{
Longitude of the second point.
}
  \item{lat.2}{
Latitude of the second point.
}
  \item{units}{
Any one of "nm": nautical miles (the default), "m": meters, "km": kilometers, or "miles": miles, for units of the returned value.
}
  \item{a}{
Major semiaxes of the ellipsoid in meters. The default is for the Earth: 6378137.0
}
  \item{b}{
Minor semiaxes of the ellipsoid in meters. The default is for the Earth: 6356752.3142
}
  \item{verbose}{
Should extra information be output, the default is FALSE.
}
  \item{longlat}{
Two dimensional (n x 2) data in the form of a matrix or dataframe. The first column should be the longitude or x-axis column and the second
the latitude or y-axis column.
}
  \item{segments}{
When there are more than two or more segments, should the distance for each segment be output. The default is TRUE.
}
  \item{digits}{
The number of rounding digits to use in the segment output. The default is 2.
}
}
\value{
The total geodesic distance in the units specified.
}
\references{
Inspired by:  \url{http://www.movable-type.co.uk/scripts/LatLongVincenty.html}

Vincenty, T.  Direct and inverse solutions of geodesics on the ellipsoid with application 
of nested equations.  Survey Review XXII, 176, April 1975.  \url{http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf}
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov} 
}
\seealso{
\code{\link{imap}}, \code{\link{draw.lines}}
}
\examples{
\dontrun{

imap() # Zoom into an area, right-click to stop.

gdist.total(draw.lines(rainbow(10), lwd=4)) # Left-click two or more times, right-click to stop.

# The total distance of the polygon's perimeter:
gdist.total(draw.polygon())  # Left-click three or more times, right-click to stop.

# The total distance of contiguous points (those not separated by NA's).
gdist.total(select.pts(imap()[[1]]$ll)) # Zoom in to an area, right-click, and then select points by creating a polygon with left-clicks.

}
}

