plotRast <- function (LongLat = NULL, Points = NULL, longrange = c(-126, 
    -124), latrange = c(41.5, 43.5), SoCal_1as = TRUE, 
    quiet = TRUE, imap = TRUE, col.imap = "grey40", alphaRaster = 1, 
    col.pts = "red", pch.pts = 16, cex.pts = 0.25, col.poly = col.alpha((grDevices::colorRampPalette(colors = c("darkblue", 
        "blue", "lightblue", "lightgreen", "yellow", "orange", 
        "red")))(length(LongLat)), alpha), alpha = 0.75, border.poly = NULL, 
    lwd.poly = 1.5, Fname = NULL, levels.contour = seq(0, -2000, 
        by = -100), GoogleEarth = FALSE, alphaGoog = 0.5, ...) 
{
    if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/R-ToolBox")
    JRWToolBox::lib(raster)
    JRWToolBox::lib(sp)
    JRWToolBox::lib(rgdal)
    JRWToolBox::lib(grDevices)
    if (GoogleEarth) 
        JRWToolBox::lib(plotKML)
	'  '	
    if (is.null(Points)) {
        plotPoints <- FALSE
        Points <- c(mean(longrange), mean(latrange))
    }
    else plotPoints = TRUE
    if (is.null(nrow(Points))) 
        Points <- as.data.frame(t(Points))
    Long <- as.numeric(Points[, 1])
    Lat <- as.numeric(Points[, 2])
    minLon <- longrange[1]
    maxLon <- longrange[2]
    minLat <- latrange[1]
    maxLat <- latrange[2]
    if (is.null(Fname)) {
        if (all(Long > -123) & all(Long < -115.999999944) & all(Lat > 
            30.99972218222) & all(Lat < 36.99972223022) & SoCal_1as) 
            URL <- paste0("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", 
                "wcs.groovy?filename=socal_1as.tif&", "request=getcoverage&version=1.0.0&service=wcs&", 
                "coverage=socal_1as&CRS=EPSG:4326&format=geotiff&", 
                "resx=0.000277777780000&resy=0.000277777780000&bbox=", 
                minLon, ",", minLat, ",", maxLon, ",", maxLat)
        else URL <- paste0("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/", 
            "wcs.groovy?filename=crm.tif&", "request=getcoverage&version=1.0.0&service=wcs&", 
            "coverage=crm&CRS=EPSG:4326&format=geotiff&", "resx=0.000833333333333334&resy=0.000833333333333334&bbox=", 
            minLon, ",", minLat, ",", maxLon, ",", maxLat)
        Fname <- "TMP.tif"
        optUSR <- options(warn = -2)
        on.exit(options(optUSR))
        utils::download.file(URL, Fname, mode = "wb", cacheOK = FALSE, 
            quiet = quiet)
    }
    BathySmall <- raster::raster(Fname, xmn = minLon, xmx = maxLon, 
        ymn = minLat, ymx = maxLat)
    NAvalue(BathySmall) <- BathySmall@data@min
    raster::plot(BathySmall, alpha = alphaRaster)
    if (!is.null(levels.contour)) 
        raster::contour(BathySmall, maxpixels = 5e+05, add = T, 
            levels = levels.contour, ...)
    if (imap) 
        Imap::imap(Imap::world.h.land, longrange = c(minLon, 
            maxLon), latrange = c(minLat, maxLat), add = T, poly = col.imap, 
            zoom = F)
    if (plotPoints) 
        points(Points[Points[, 1] >= minLon & Points[, 1] <= 
            maxLon & Points[, 2] >= minLat & Points[, 2] <= 
            maxLat, ], col = col.pts, pch = pch.pts, cex = cex.pts)
    if (!is.null(LongLat)) {
        col.poly <- rep(col.poly, length = length(LongLat))
        for (i in 1:length(LongLat)) polygon(LongLat[[i]], 
            col = col.poly[i], border = border.poly, lwd = lwd.poly)
    }
    if (GoogleEarth) {
        assign("alphaGoog", alphaGoog, pos = 1)
        plotKML::plotKML(BathySmall, colour_scale = rev(terrain.colors(255)), 
            alpha = alphaGoog)
    }
}
