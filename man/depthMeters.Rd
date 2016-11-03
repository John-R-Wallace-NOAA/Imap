\name{depthMeters}
\alias{depthMeters}
\title{
Depth in meters off the West Coast of the contiguous U.S.
}
\description{
depthMeters() gives the depth, in meters, using areas vol 6 and 7 in NOAA's U.S. Coastal Relief Model.
}
\usage{
depthMeters(LongLat = c(-120.0, 33.0))
}
\arguments{
  \item{LongLat}{
A single long/lat point or a matrix or data frame with longitude in the first column and latitude in the second column.
}
  \item{blockSizeDegs}{
The block size, in degrees, of bathymetry over which the depth is extracted.  
The default is 0.5 degrees for the 1 Arc-Second volume 6 covering southern California and 2 degrees for 
the 3 Arc-Second volume 7 covering the entire West Coast of the contigous U.S.
}
  \item{SoCal_1as}{
Should 1 Arc-Second volume 6 be used. The default is TRUE.
}
  \item{method}{
Method used to extract the depth from a Raster object using the extract() funcion in the 'raster' package. Defaults to 'bilinear'.
}
  \item{plot}{
Should a plot be created showing the bathmetry and the point at which the depth is extracted. Default is true when the
number of depths wanted is less than 5 and FALSE otherwise.
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
John R. Wallace: \email{Imap.for.R@gmail.com} (Limited support)
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


