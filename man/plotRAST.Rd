\name{plotRAST}
\alias{plotRAST}
\title{
Plot data from NOAA's National Centers for Environmental Information, including bathymetric information
}
\description{
\code{plotRAST} creates figures from NOAA's National Centers for Environmental Information (mainly 1-minute resolution), including NOAA's U.S. Coastal Relief 
Model for the contiguous U.S. (3 arc-second resolution) and 1 arc-second resolution from Southern California. The SoCal 1 arc-second data extends 
to the southeastern corner of the U.S. West Coast's EEZ, however the SoCal 3 arc-second coverage goes further south and west.  Note also that the defaults for 
land overlay (set to TRUE) and greater density of the contour lines are bathymetric centric. (The land overlay default for the \code{imapToRAST} function is FALSE 
and contour lines on both land and sea are shown, but the default density of the coutour lines is less.)
}
\usage{
plotRAST(LongLat = NULL, polygons = NULL, longrange = c(-126, -124), latrange = c(41.5, 43.5), 
    layer =c("ETOPO1_ice_surface", 'ETOPO1_bedrock', "crm", "socal_3as", "socal_1as")[1], autoLayer = ifelse(missing(layer), TRUE, FALSE), URL = NULL, verbose = TRUE, 
    quiet = TRUE, landOverlay = TRUE, col.imap = "grey40", alphaRaster = 1, col.pts = "red", pch.pts = 16, cex.pts = 0.25, 
    col.poly = Imap::col.alpha((grDevices::colorRampPalette(colors = c("darkblue", "blue", "lightblue", "lightgreen", "yellow", "orange", "red")))(length(polygons)), alpha), 
    alpha = 0.75, border.poly = NULL, lwd.poly = 1.5, Fname = NULL, levels.contour = if(landOverlay) seq(-100, -2000, by = -100) else seq(-11000, 9000, by = 500), 
    plot = TRUE, plot3D = FALSE, GoogleEarth = FALSE, alphaGoog = 0.5, ...) 
}
\arguments{
  \item{LongLat}{
Optional Long/Lat data to be plotted. Longlat should be a two dimensional user supplied data in the form of a matrix or data frame. 
The first column should be the longitude or x-axis column and the second the latitude or y-axis column. 
}
  \item{polygons}{
Optional list with polygon data to be plotted within each elemnet of the list using the \code{lines} function.
}
  \item{longrange}{
Longitudinal range of the figure to be plotted.
}
  \item{latrange}{
Latitudinal range of the figure to be plotted.
}
  \item{layer}{
Which layer should be used: 'ETOPO1_ice_surface' = Entire Earth's surface with the top of the Antarctic and Greenland ice sheets depicted (1-minute resolution),
'ETOPO1_bedrock' = Entire Earth's surface with bedrock depcted on Antarctic and Greenland (1-minute resolution),
'crm' = Coastal Relief Model for the contiguous U.S. coastline (3 arc-second resolution), 'socal_3as' = Southern California (3 arc-second resolution), 
or 'socal_1as' = Southern California (1 arc-second resolution). 
The default is ETOPO1_ice_surface, but this can be changed when autoLayer is TRUE.
}
  \item{autoLayer}{
Should the Coastal Relief (3 arc-second) and Southern California (1 arc-second) layers (see the references) be used when possible. The default is TRUE if the 
layer argument is not explictly given.
}
 \item{URL}{
An URL from which a raster file can be downloaded. When URL is NULL (the default), the URL is generated from NOAA's NCEI Grid Extract website and the lat/long ranges.
}
 \item{verbose}{
Should information about the layer used be shown. Default is TRUE. 
} 
  \item{quiet}{
Should the \code{download.file} function from the \code{utils} package be quiet when downloading the bathymetry data and the URL used to extract the data not be shown. 
the default is TRUE.
}
  \item{landOverlay}{
Should the \code{imap} function be used for a land overlay. Default is TRUE. 
}
 \item{col.imap}{
Color of the imap() land overlay. Default is "grey40".
}
  \item{alphaRaster}{
Number between 0 and 1 to set transparency. 0 is entirely transparent, 1 is not transparent (NULL is equivalent to 1). 
Passed to the \code{plot} function in the \code{Raster} package.
}
  \item{col.pts}{
Color of the LongLat points to be plotted. The default is red.
}
  \item{pch.pts}{
Type of LongLat point to plotted, see the help for \code{points}.
}
  \item{cex.pts}{
Size of Longlat point to be plotted.
}
   \item{col.poly}{
Color of the polygon fills in the 'polygons' list to be plotted. The default is a color ramp (see the default for included colors) 
with an alpha of 0.75. The colors will be repeated as needed.
}
\item{alpha}{
Number between 0 and 1 to set transparency. 0 is entirely transparent, 1 is not transparent (NULL is equivalent to 1). 
Sets the alpha level of the colors in the 'col.poly' argument.
}
   \item{border.poly}{
Color of the polygon border in the 'polygons' list to be plotted.
}
   \item{lwd.poly}{
Line width of the polygons in the 'polygons' list to be plotted. The default is 1.5.
}
   \item{Fname}{
Name of a special graphics TIF file to use for plotting. The default is NULL which will download a new TIF file with the 
Long/Lat ranges given by 'longrange' and 'latrange'. The name of the downloaded TIF file is 'TMP.tif' and is saved in 
working directory [\code{getwd()}]. Using a previously downloaded TIF will save download time and allow work offline but with 
a reasonable internet connection is rarely needed.
}
   \item{levels.contour}{
The levels, in meters, of the contour lines to plot.
}
  \item{plot}{
Plot a raster image using the 'raster' package. The default is TRUE.
}
  \item{plot3D}{
Plot a 3D cloud of color banded points using the 'rgl' package. The default is FALSE.
}
   \item{GoogleEarth}{
Show the raster image (converted to KML format) in Google Earth. The default is FALSE. 
}
 \item{alphaGoog}{
Set the alpha transparency level for the raster image in Google Earth. The default is 0.5.
}
}
\value{
A bathymetric figure is created with Long/Lat ranges given by 'longrange' and 'latrange' and contour lines with levels given by 'levels.contour'.  
Optionally, 'LongLat' points and polygons in the 'polygons' list can be plotted over the bathymetry. 
The raster file in invisibly returned.
}
\references{
For getting the bathymetry into R, I followed Tom Wainwright's presentation: 
\url{http://rstudio-pubs-static.s3.amazonaws.com/53530_1af2d0b5ae1f4a36a75e611d3566f777.html#1}

Grid Extract: \url{https://maps.ngdc.noaa.gov/viewers/wcs-client} and U.S. Coastal Relief Model overview: \url{http://www.ngdc.noaa.gov/mgg/coastal/crm.html}
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov}
}
\seealso{
\code{\link{depthMeters}}, \code{\link{draw.polygon}} 

This function uses the double colon operator for access to the \code{raster} function [\code{raster::raster(...)}] to avoid loading the package.  
Therefore to see the help for \code{raster()}, either load the package or use: \code{?raster::raster}
}
\examples{
\dontrun{
   plotRAST(latrange = c(46, 48), longrange = c(-126, -123.8))
   
   temp.poly <- list(data.frame(x= c( -125, -125, -124.6, -124.6, -125), y = c(42.2, 42, 42, 42.2, 42.2)), 
                    data.frame(x= c( -124.8, -124.8, -124.4, -124.4, -124.8), y = c(42.1, 41.9, 41.9, 42.1, 42.1)))

   plotRAST(polygons = temp.poly, col.poly = c(col.alpha('blue', 0.5), col.alpha('red', 0.5)), border.poly='black', lwd.poly=2)
   draw.polygon('green') # Interactively draw a polygon filled with green (the default alpha level is 0.5)

   # Compare Southern CA 1 arc-sec, 3 arc-sec of the Coastal Relief Model (crm), and 1 minute data of the ETOPO1 (with ice height on Greenland and Antarctica)
   plotRAST(longrange = c(-120.25, -119.7), latrange = c(32.7, 33.3))
   dev.new()
   plotRAST(longrange = c(-120.25, -119.7), latrange = c(32.7, 33.3), layer = 'crm')
   dev.new()
   plotRAST(longrange = c(-120.25, -119.7), latrange = c(32.7, 33.3), layer = 'ETOPO1_ice_surface')

   
   # Use imapToRAST() to zoom into an area which then will be replaced by a raster image with the same bounding box coordinates.
   imapToRAST()
    
   # 3D cloud of color banded points 
   plotRAST(plot3D = TRUE)

   # Raster image shown in Google Earth 
   plotRAST(Google = TRUE)

}
}

