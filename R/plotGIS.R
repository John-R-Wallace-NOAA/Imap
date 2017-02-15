plotGIS <- function (LongLat = NULL, Polygons.List = NULL, longrange = c(-126, -124), latrange = c(41.5, 43.5), SoCal_1as = TRUE,
                     method = "bilinear", quiet = TRUE, imap = TRUE, alpha = 1, col.pts = 'red', pch.pts = 16, cex.pts = 0.25,
                     col.poly = 'blue', lwd.poly = 1.5, Fname = NULL, levels.contour = seq(0,-2000, by=-100), ...) {
 
    if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/R-ToolBox")
    JRWToolBox::lib(raster)

    if(is.null(LongLat)) {
       plotPoints <- FALSE
       LongLat <- c(mean(longrange), mean(latrange))
    } else
        plotPoints = TRUE
    
    if (is.null(nrow(LongLat))) 
         LongLat <- as.data.frame(t(LongLat))
  
    Long <- as.numeric(LongLat[,1])
    Lat <- as.numeric(LongLat[,2])

    minLon <- longrange[1]
    maxLon <- longrange[2]
    minLat <- latrange[1]
    maxLat <- latrange[2]

   if(is.null(Fname)) {

       if (all(Long > -123) & all(Long < -115.999999944) & all(Lat > 30.99972218222) & all(Lat < 36.99972223022) & SoCal_1as)
            URL <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", 
                "wcs.groovy?filename=socal_1as.tif&", "request=getcoverage&version=1.0.0&service=wcs&", 
                "coverage=socal_1as&CRS=EPSG:4326&format=geotiff&", 
                "resx=0.000277777780000&resy=0.000277777780000&bbox=", 
                minLon, ",", minLat, ",", maxLon, ",", maxLat, 
                sep = "")
          
        else 
            URL <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", 
                "wcs.groovy?filename=crm.tif&", "request=getcoverage&version=1.0.0&service=wcs&", 
                "coverage=crm&CRS=EPSG:4326&format=geotiff&", 
                "resx=0.000833333333333334&resy=0.000833333333333334&bbox=", 
                minLon, ",", minLat, ",", maxLon, ",", maxLat, 
                sep = "")
        
       Fname <- "TMP.tif"
       optUSR <- options(warn = -2)
       on.exit(options(optUSR))
       utils::download.file(URL, Fname, mode = "wb", cacheOK = FALSE, quiet = quiet)
    }
    BathySmall <- raster::raster(Fname)
    raster::plot(BathySmall, alpha = alpha)
    raster::contour(BathySmall, maxpixels = 500000, add = T, levels = levels.contour, ...)

    if(imap)
      Imap::imap(Imap::world.h.land, longrange = c(minLon, maxLon) , latrange = c(minLat, maxLat), add = T, zoom = F)

    if(plotPoints)
        points(LongLat[LongLat[,1] >= minLon & LongLat[,1] <= maxLon & LongLat[,2] >= minLat &  LongLat[,2] <= maxLat,], col = col.pts, pch = pch.pts, cex = cex.pts)

    if(!is.null(Polygons)) {

        for ( i in 1:length(Polygons))
                 lines(Polygons.List[[i]], col = col.poly, lwd=lwd.poly)
    }

}
