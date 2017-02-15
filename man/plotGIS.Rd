\name{plotGIS}
\alias{plotGIS}
\title{
Plot a bathymetric figure off the West Coast of the contiguous U.S.
}
\description{
\code{plotGIS} creates a GIS bathymetric figure off the coasts of Washington, Oregon, and California using areas vol 6 and 7 in NOAA's U.S. Coastal Relief Model. 
Note that the SoCal 1 Arc-Second extends to the southeastern corner of the U.S. West Cost EEZ, however the SoCal 3 Arc-Second coverage goes further south and west.
}
\usage{
plotGIS(LongLat = NULL, polygons = NULL, longrange = c(-126, -124), latrange = c(41.5, 43.5), SoCal_1as = TRUE, 
        method = "bilinear", quiet = TRUE, imap = TRUE, alpha = 1, col.pts = "red", pch.pts = 16, cex.pts = 0.25, 
        col.poly = col.alpha('blue', 0.5), border.poly = NULL, lwd.poly = 1.5, Fname = NULL, 
        levels.contour = seq(0, -2000, by = -100), ...)  
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
  \item{SoCal_1as}{
Should 1 Arc-Second volume 6 (see the references) be used when possible. The default is TRUE.
}
  \item{method}{
Method used to extract the depth from a Raster object using the \code{extract} funcion in the \code{raster} package. Defaults to 'bilinear'.
}
  \item{quiet}{
Should the \code{download.file} function from the \code{utils} package be quiet when downloading the bathymetry data, 
the default is TRUE.
}
  \item{imap}{
Should the \code{imap} function be used for a land overlay. Default is TRUE.
}
  \item{alpha}{
Number between 0 and 1 to set transparency. 0 is entirely transparent, 1 is not transparent (NULL is equivalent to 1). Passed to the \code{plot} function in the \code{Raster} package.
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
Colors of the polygon fill in the 'polygons' list to be plotted. The default is blue with an alpha level of 0.5. 
The colors will be repeated as needed.
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
}
\value{
A bathymetric figure off the West Coast of the contiguous U.S. is created with Long/Lat ranges given by 'longrange' and 'latrange' and contour lines with levels given by 'levels.contour'.
Otional 'LongLat' points and polygon in 'Polygons.List' with be plotted over the bathymetry. 
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
\code{\link{depthMeters}}, \code{\link{draw.polygon}} 

This function uses the double colon operator for access to the \code{raster} function 
[\code{raster::raster(...)}].  Therefore to see the help for \code{raster()}, either load the package 
or use: \code{?raster::raster}
}
\examples{
\dontrun{
   plotGIS(latrange = c(46, 48), longrange = c(-126, -123.8))
   
  temp.poly <- list(data.frame(x= c( -125, -125, -124.6, -124.6, -125), y = c(42.2, 42, 42, 42.2, 42.2)), 
                    data.frame(x= c( -124.8, -124.8, -124.4, -124.4, -124.8), y = c(42.1, 41.9, 41.9, 42.1, 42.1)))

   plotGIS(polygons = temp.poly, col.poly = c(col.alpha('blue', 0.5), col.alpha('red', 0.5)), border.poly='black', lwd.poly=2)
   draw.polygon('green') # Interactively draw a polygon filled with green (the default alpha level is 0.5)

   # Compare Southern CA 1 arc-sec to 3 arc-sec data
   plotGIS(longrange = c(-120.25, -119.7), latrange = c(32.7, 33.3))
   dev.new()
   plotGIS(longrange = c(-120.25, -119.7), latrange = c(32.7, 33.3), SoCal_1as=FALSE)
}
}

