\name{depthMeters}
\alias{depthMeters}
\title{
Depth in meters given a latitude and longitude point.
}
\description{
\code{depthMeters} gives the depth, in meters, using data from NOAA NCEI's Grid Extraction. Note that, even though
the SoCal 1 Arc-Second data extends to the southeastern corner of U.S. West Cost EEZ, the SoCal 3 Arc-Second coverage goes further 
south and west.
}
\usage{
depthMeters(LongLat = c(-120, 33), layer = layer, plot = ifelse(N < 5, TRUE, FALSE), SoCal_1as = TRUE, 
     blockSizeDegs = ifelse(plot, ifelse(SoCal_1as, 0.5, 2), ifelse(SoCal_1as, 0.005, 0.002), method = "bilinear", 
      verbose = plot, quiet = !plot, Zero.to.NA = TRUE, plot3D = FALSE, GoogleEarth = FALSE, alphaGoog = 0.5)  
}
\arguments{
  \item{LongLat}{
A single long/lat vector or a matrix or data frame with longitude in the first column and latitude in the second column.
}
 \item{layer}{
Which layer should be used: 'ETOPO1_ice_surface' = Entire Earth's surface with the top of the Antarctic and Greenland ice sheets depicted (1-minute resolution),
'ETOPO1_bedrock' = Entire Earth's surface with bedrock depcted on Antarctic and Greenland (1-minute resolution),
'crm' = Coastal Relief Model for the contiguous U.S. coastline (3 arc-second resolution), 'socal_3as' = Southern California (3 arc-second resolution), 
or 'socal_1as' = Southern California (1 arc-second resolution). The default is ETOPO1_ice_surface.
}
  \item{plot}{
Should a plot be created showing the bathmetry and the point at which the depth is extracted. Default is TRUE when the
number of depths asked for is less than 5 and FALSE otherwise.
}
  \item{SoCal_1as}{
Should should a smaller bounding box be used when the longlat point is within the Southern Cali 1 Arc-Sec data. The default is TRUE.
}
  \item{blockSizeDegs}{
The block size, in degrees, over which the depth is extracted.  
The default when plotting is 0.5 degrees on a side for the 1 Arc-Second data covering southern California and 2 degrees otherwise.
The default when not plotting is 0.005 degrees on a side for the 1 Arc-Second data covering southern California and 0.002 degrees otherwise.
}
 \item{method}{
Method used to extract the depth from a Raster object using the \code{extract} function in the \code{raster} package. 
Defaults to 'bilinear'.
}
  \item{verbose}{
Should information about the layer used be shown. Default is equal to the 'plot' augument.
}
  \item{quiet}{
Should addition information be surpressed. Defaults to the logical negation of the 'plot' augument.
}
  \item{Zero.to.NA}{
When a long/lat point is in a volume, but outside of an area which contains data, an exact zero is extracted. Should these zeros
be converted into NA's.  The default is TRUE.
}
  \item{plot3D}{
Plot a 3D cloud of color banded points using the 'rgl' package.
}
  \item{GoogleEarth}{
Show the raster image (converted to KML format) in Google Earth.  The default is FALSE.
}
  \item{alphaGoog}{
Set the alpha transparency level for the raster image in Google Earth. The default is 0.5.
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

# Compare the difference in fine resolution when the block size is decreased. 
  depthMeters(c(-118.6439, 33.4808), blockSizeDegs = 0.5) 
  depthMeters(c(-118.6439, 33.4808), blockSizeDegs = 0.005)

# Look at the image in Google Earth
  depthMeters(c(-118.6439, 33.4808), blockSizeDegs = 0.005, Google = T)
  
# Here is an example of the where the long/lat point is in a volume but outside of the data area: 
  depthMeters(rbind(c(0, 0), c(-120, 33), c(-135, 41)), 0.01, Zero.to.NA = F)
  
# Example of using plot3D and Google Earth
  depthMeters(c(-118.229967, 32.1018333), 0.02, plot3D = T, Goog=T)
    
# A data frame or matrix can also be used with longitude in the first column and latitude in the second.
# If you give depthMeters() 5 or more locations the 'plot' argument's default is not to create a figure for each one but to only return back the depths.
  LongLat <- data.frame(Long = c(-118.6442, -118.5227, -119.4888, -119.84, -120.00), 
                       Lat =  c(33.4817, 33.3485, 33.9634, 33.94, 33.00))
  depthMeters(LongLat)
}
}
