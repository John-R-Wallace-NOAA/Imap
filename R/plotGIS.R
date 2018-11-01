plotGIS <- function (LongLat = NULL, polygons = NULL, longrange = c(-126, -124), latrange = c(41.5, 43.5), 
    layer = c('etopo1', 'etopo1_bedrock', 'crm', 'SoCal_1as')[1], autoLayer = TRUE, quiet = TRUE, imap = TRUE, verbose = FALSE, col.imap = "grey40", alphaRaster = 1, col.pts = "red", pch.pts = 16, 
    cex.pts = 0.25, col.poly = col.alpha((grDevices::colorRampPalette(colors = c("darkblue", "blue", "lightblue",
    "lightgreen", "yellow", "orange", "red")))(length(polygons)), alpha), alpha = 0.75, border.poly = NULL, 
    lwd.poly = 1.5, Fname = NULL, levels.contour = if(imap) seq(-100, -2000, by = -100) else seq(0, -2000, by = -100),
    GoogleEarth = FALSE, alphaGoog = 0.5, ...) 
{
    if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
        
    JRWToolBox::lib(raster)
    JRWToolBox::lib(sp)
    JRWToolBox::lib(rgdal)
    JRWToolBox::lib(grDevices)
    
    if (GoogleEarth) 
        JRWToolBox::lib(plotKML)

    if (is.null(LongLat)) {
        plotPoints <- FALSE
        LongLat <- c(mean(longrange), mean(latrange))
    }
    else plotPoints = TRUE
    
    if (is.null(nrow(LongLat))) 
        LongLat <- as.data.frame(t(LongLat))
        
    Long <- as.numeric(LongLat[, 1])
    Lat <- as.numeric(LongLat[, 2])
    
    minLon <- longrange[1]
    maxLon <- longrange[2]
    minLat <- latrange[1]
    maxLat <- latrange[2]
     
    if (is.null(Fname)) {
        if (all(Long > -162.000416666667) & all(Long < -63.9995833372533) & all(Lat > 15.9995833344534) & all(Lat < 49.0004166664667) & autoLayer) 
            layer <- 'crm'
        if (all(Long > -123) & all(Long < -115.999999944) & all(Lat > 30.99972218222) & all(Lat < 36.99972223022) & autoLayer) 
            layer <- 'SoCal_1as'
        Rez <- 1/c(60, 60, 1200, 3600)[c('etopo1', 'etopo1_bedrock', 'crm', 'SoCal_1as') %in% layer]
        if(verbose) cat("\n\nlayer = ", layer, " with ", 3600 * Rez, "-second resolution\n\n", sep="")        
        
        URL <- paste0("https://gis.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", 
                "wcs.groovy?filename=", layer, ".tif&", "request=getcoverage&version=1.0.0&service=wcs&", 
                "coverage=", layer, "&CRS=EPSG:4326&format=geotiff&", 
                "resx=", Rez, "&resy=", Rez, "&bbox=", minLon, ",", minLat, ",", maxLon, ",", maxLat)                
        if(verbose) cat("\n\nURL =", URL, "\n\n") 
                 
        Fname <- "TMP.tif"
        if(quiet) {
           optUSR <- options(warn = -2)
           on.exit(options(optUSR))
        }
        utils::download.file(URL, Fname, method = 'auto', mode = "wb", cacheOK = FALSE, quiet = quiet)
        
    }
    
    BathySmall <- raster::raster(Fname, xmn = minLon, xmx = maxLon, ymn = minLat, ymx = maxLat)
    NAvalue(BathySmall) <- BathySmall@data@min
    raster::plot(BathySmall, alpha = alphaRaster)
    
    if (!is.null(levels.contour)) 
        raster::contour(BathySmall, maxpixels = 5e+05, add = T, levels = levels.contour, ...)
        
    if (imap) 
        Imap::imap(Imap::world.h.land, longrange = c(minLon, maxLon), latrange = c(minLat, maxLat), add = T, poly = col.imap, zoom = F)
        
    if (plotPoints) 
        points(LongLat[LongLat[, 1] >= minLon & LongLat[, 1] <= maxLon & LongLat[, 2] >= minLat & LongLat[, 2] <= maxLat, ],
               col = col.pts, pch = pch.pts, cex = cex.pts)
               
    if (!is.null(polygons)) {
        col.poly <- rep(col.poly, length = length(polygons))
        for (i in 1:length(polygons)) polygon(polygons[[i]], col = col.poly[i], border = border.poly, lwd = lwd.poly)
    }
    
    if (GoogleEarth) {
        assign("alphaGoog", alphaGoog, pos = 1)
        plotKML::plotKML(BathySmall, colour_scale = rev(terrain.colors(255)), alpha = alphaGoog)
    }
}
