
depthMeters <- function(LongLat, blockSizeDegs = ifelse(plot, ifelse(SoCal_1as, 0.5, 2), ifelse(SoCal_1as, 0.005, 0.002)), 
   SoCal_1as = TRUE, method = "bilinear", plot = ifelse(N < 5, TRUE, FALSE), quiet = !plot, OuterIndex = 1, Zero.to.NA = TRUE) {
  "  "
  "  Examples: depthMeters(c(-125.6875, 48.14417)) # Auto plot and auto 0.5 or 2 deg. block; Depths <- depthMeters(NWDepth[1:10, c('BEST_LON_DD', 'BEST_LAT_DD')]) # Auto no plot and 0.005 deg block "
  "  Example of SoCal 1 arcsec: depthMeters(c(-120, 33))  # Auto uses Southern Cali 1 arc-sec unless SoCal_1as = FALSE  "
  "  " 
  "  For getting the bathymetry into R, I followed Tom Wainwright here: http://rstudio-pubs-static.s3.amazonaws.com/53530_1af2d0b5ae1f4a36a75e611d3566f777.html#1  "  
  "  Using Coastal Relief Mapping project that has(3 arc-sec) bathymetry for most of the US coastline and 1 arc-sec for Southern CA.  " 
  "  U.S. Coastal Relief Model Map is here:  http://www.ngdc.noaa.gov/mgg/coastal/crm.html  "
  "  "
  "    Note that a long/lat point may be in a volume, but outside of the data area, in which case a zero depth is reported.  "
  "    Here is an example: (Imap::depthMeters(rbind(c(0, 0), c(-120, 33), c(-135, 41)), 0.01, Zero.to.NA = F))  "
  "  "
  "   Also, even though the SoCal 1 arcsec extends to the southeastern corner of U.S. West Cost EEZ, the SoCal 3 arcsec coverage does go further south and west.  "
  "   "
  
  if(!any(installed.packages()[,1] %in% "devtools"))
     install.packages("devtools")
  if(!any(installed.packages()[,1] %in% "JRWToolBox"))
       devtools::install_github('John-R-Wallace/R-ToolBox')
 
  JRWToolBox::lib(rgdal) 
  JRWToolBox::lib(raster)

  DepthM <- function(LongLat, blockSizeDegs = 0.01, method = 'bilinear', plot = TRUE, quiet = TRUE) {
 
    Long <- as.numeric(LongLat[1])
    Lat <- as.numeric(LongLat[2])

    minLon <- Long - blockSizeDegs/2
    maxLon <- Long + blockSizeDegs/2
    minLat <- Lat - blockSizeDegs/2
    maxLat <- Lat + blockSizeDegs/2

   if(Long > -123 & Long < -115.999999944 & Lat > 30.99972218222 & Lat < 36.99972223022 & SoCal_1as) 

      URL <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", "wcs.groovy?filename=socal_1as.tif&", 
             "request=getcoverage&version=1.0.0&service=wcs&", "coverage=socal_1as&CRS=EPSG:4326&format=geotiff&",
             "resx=0.000277777780000&resy=0.000277777780000&bbox=",minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")
    else

       URL <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", "wcs.groovy?filename=crm.tif&",
             "request=getcoverage&version=1.0.0&service=wcs&", "coverage=crm&CRS=EPSG:4326&format=geotiff&",
             "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")

    Fname <- "TMP.tif"
  
    optUSR <- options(warn = -2)
    on.exit(options(optUSR))
    download.file(URL, Fname, mode="wb", cacheOK=FALSE, quiet = quiet)
    BathySmall <- raster(Fname)

    if(plot) {
      plot(BathySmall)
      Imap::ilines(Imap::world.h.land, longrange = c(minLon, maxLon), latrange = c(minLat, maxLat), add = T, zoom = F)
      points(LongLat, col ='red', pch = 16)
      
    }
  
    - extract(BathySmall, LongLat, method = method)
  }
  
  if(is.null(nrow(LongLat)))
     LongLat <- as.data.frame(t(LongLat))

  N <- nrow(LongLat)
  Out <- rep(NA, N)
  for( i in 1:N) {
    if(N >= 5)
       JRWToolBox::bar(i, N)
    try(Out[i] <- DepthM(LongLat[i, , drop=FALSE], blockSizeDegs = blockSizeDegs, method = method, plot = plot, quiet = quiet), silent = TRUE)
  }
  if(any(Out %in% 0.00) & Zero.to.NA) {
     Out[Out %in% 0.00] <- NA
     warning("Depths equal to exactly zero were converted to NA.", call. = FALSE)
  }
  Out
}


