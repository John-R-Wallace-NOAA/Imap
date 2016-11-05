\name{depthMeters}
\alias{depthMeters}
\title{
Depth in meters off the West Coast of the contiguous U.S.
}
\description{
depthMeters() gives the depth, in meters, using areas vol 6 and 7 in NOAA's U.S. Coastal Relief Model. Note that, even though
the SoCal 1 Arc-Second extends to the southeastern corner of U.S. West Cost EEZ, the SoCal 3 Arc-Second coverage goes further 
south and west.
}
\usage{
depthMeters(c(-118.6442, 33.4817))
}
\arguments{
  \item{LongLat}{
A single long/lat vector or a matrix or data frame with longitude in the first column and latitude in the second column.
}
  \item{blockSizeDegs}{
The block size, in degrees, over which the depth is extracted.  
The default is 0.5 degrees on a side for the 1 Arc-Second volume 6 covering southern California and 2 degrees for 
the 3 Arc-Second volume 7 covering the entire West Coast of the contigous U.S.
}
  \item{SoCal_1as}{
Should 1 Arc-Second volume 6 be used. The default is TRUE.
}
  \item{method}{
Method used to extract the depth from a Raster object using the extract() funcion in the 'raster' package. Defaults to 'bilinear'.
}
  \item{plot}{
Should a plot be created showing the bathmetry and the point at which the depth is extracted. Default is TRUE when the
number of depths asked for is less than 5 and FALSE otherwise.
}
  \item{quiet}{
Should addition information be surpressed. Defaults to the logical negation of the 'plot' augument.
}
  \item{Zero.to.NA}{
When a long/lat point is in a volume, but outside of an area which contains data, an exact zero is extracted. Should these zeros
be converted into NA's.  The default is TRUE.
}
}
\value{
The depth in meters of the point(s) given.
}
\references{
For getting the bathymetry into R, I followed Tom Wainwright here: 
\url{http://rstudio-pubs-static.s3.amazonaws.com/53530_1af2d0b5ae1f4a36a75e611d3566f777.html#1}

U.S. Coastal Relief Model Map is here:  \url{http://www.ngdc.noaa.gov/mgg/coastal/crm.html}
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov}
}
\seealso{
\code{\link{raster}}
}
\examples{
\dontrun{
# Here is an example of the where the long/lat point is in a volume but outside of the data area: 
 Imap::depthMeters(rbind(c(0, 0), c(-120, 33), c(-135, 41)), 0.01, Zero.to.NA = F)
}
}
