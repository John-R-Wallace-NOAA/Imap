
depthMeters  <- function(LongLat, blockSize = 0.01, method = 'bilinear', plot = ifelse(N < 5, TRUE, FALSE), quiet = TRUE) {

  '  For getting the bathymetry into R, I followed Tom Wainwright here: http://rstudio-pubs-static.s3.amazonaws.com/53530_1af2d0b5ae1f4a36a75e611d3566f777.html#1  '  
  '  Using Coastal Relief Mapping project that has fine-resolution (0.3 arc-sec) bathymetry for most of the US coastline.  ' 
  '  Map is here:  '
  '  https://www.arcgis.com/home/webmap/viewer.html?basemapUrl=//services.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer&urls=//maps.ngdc.noaa.gov/arcgis/rest/services/web_mercator/crm_hillshade/MapServer  '
  '  '
  '  devtools::install_github("John-R-Wallace/R-ToolBox")  '
  require(JRWToolBox)  
  lib(sp)      
  lib(rgdal)   
  lib(raster)

  DepthM <- function(LongLat, blockSize = 0.01, method = 'bilinear', plot = TRUE, quiet = TRUE) {
 
    minLon <- LongLat[1] - blockSize
    maxLon <- LongLat[1] + blockSize
    minLat <- LongLat[2] - blockSize
    maxLat <- LongLat[2] + blockSize

    URL <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/",
             "wcs.groovy?filename=crm.tif&",
             "request=getcoverage&version=1.0.0&service=wcs&",
             "coverage=crm&CRS=EPSG:4326&format=geotiff&",
             "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",
             minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")

    Fname <- "crm_WACoast_TMP_DEPTH_FUNTION.tif"
  
    optUSR <- options(warn = -2)
    on.exit(options(optUSR))
    download.file(URL, Fname, mode="wb", cacheOK="false", quiet = quiet)
    BathySmall <<- raster(Fname)

    if(plot) {
      plot(BathySmall)
      points(LongLat, col ='red', pch = 16)
    }
  
    - extract(BathySmall, LongLat, method = method)
  }

  Out <- NULL
  N <- nrow(LongLat)
  for( i in 1:N) {
    if(N >= 5)
       bar(i, N)
    Out <- c(Out, DepthM(LongLat[i, ], blockSize = blockSize, method = method, plot = plot, quiet = quiet))
  }

  Out
}


