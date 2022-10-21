plotRAST <- function (LongLat = NULL, polygons = NULL, longrange = c(-126, -124), latrange = c(41.5, 43.5), extent = NULL, 
    layer = c('ETOPO1_ice_surface', 'ETOPO1_bedrock', 'crm', 'socal_3as', 'socal_1as')[1], autoLayer = ifelse(missing(layer), TRUE, FALSE), URL = NULL, 
    verbose = TRUE, quiet = TRUE, landOverlay = TRUE, col.imap = "grey40", alphaRaster = 1, col.pts = "red", filled = FALSE, col.contour = NULL, pch.pts = 16, 
    cex.pts = 0.25, maxElev = NULL, minElev = NULL, col.poly = col.alpha((grDevices::colorRampPalette(colors = c("darkblue", "blue", "lightblue",
    "lightgreen", "yellow", "orange", "red")))(length(polygons)), alpha), alpha = 0.75, col.bathy = rev(grDevices::terrain.colors(50)), border.poly = NULL, 
    lwd.poly = 1.5, Fname = NULL, levels.contour = if(landOverlay) seq(-100, -2000, by = -100) else seq(-11000, 9000, by = 500),
    plot = TRUE, plot3D = FALSE, GoogleEarth = FALSE, alphaGoog = 0.5, ...) 
{
    if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
        
    JRWToolBox::lib(terra)
    JRWToolBox::lib(grDevices)
    
    if (plot3D) 
        JRWToolBox::lib(rgl)
    
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
    
    if(is.null(extent))
        extent <- c(minLon, maxLon, minLat, maxLat)
     
    if (is.null(Fname)) {
    
        if(is.null(URL)) {
    
           if (all(Long > -162.000416666667) & all(Long < -63.9995833372533) & all(Lat > 15.9995833344534) & all(Lat < 49.0004166664667) & autoLayer) # The West Coast of the contiguous USA
               layer <- 'crm'
           if (all(Long > -123) & all(Long < -115.999999944) & all(Lat > 30.99972218222) & all(Lat < 36.99972223022) & autoLayer) # Southern California Bight with 1 arc-sec resolution
               layer <- 'socal_1as'
           ptsPerLat <- c(60, 60, 1200, 3600)[c('ETOPO1_ice_surface', 'ETOPO1_bedrock', 'crm', 'socal_1as') %in% layer]
      
           if(verbose & 3600/ptsPerLat != 60) cat("\n\nlayer = ", layer, " with ", 3600/ptsPerLat, "-second resolution\n\n", sep="") 
           if(verbose & 3600/ptsPerLat == 60) cat("\n\nlayer = ", layer, " with 1-minute resolution\n\n", sep="") 
        
           if(layer %in% c('ETOPO1_ice_surface', 'ETOPO1_bedrock')) 
           
              URL <- paste0("https://gis.ngdc.noaa.gov/arcgis/rest/services/DEM_mosaics/", layer, "/ImageServer/exportImage?bbox=",
              minLon, ",", minLat, ",", maxLon, ",", maxLat, "&bboxSR=4326&size=", ptsPerLat * (maxLon - minLon), ",", ptsPerLat * (maxLat - minLat), "&imageSR=4326&format=tiff&pixelType=S16&interpolation=+RSP_NearestNeighbor&compression=LZW&f=image")
           
           else {
           
             if(layer %in% c('crm', 'socal_3as', 'socal_1as')) 
               
                URL <- paste0("https://gis.ngdc.noaa.gov/arcgis/rest/services/DEM_mosaics/DEM_all/ImageServer/exportImage?bbox=",
                   minLon, ",", minLat, ",", maxLon, ",", maxLat, "&bboxSR=4326&size=", ptsPerLat * (maxLon - minLon), ",", ptsPerLat * (maxLat - minLat), "&imageSR=4326&format=tiff&pixelType=F32&interpolation=+RSP_NearestNeighbor&compression=", 
                   "LZW&mosaicRule={%22mosaicMethod%22:%22esriMosaicAttribute%22,%22where%22:%22name=%27", layer, "%27%22}&f=image")
               
             else  
                stop("Check the name of layer argument")  
           }
        }    
                
        if(!quiet) cat("\n\nURL =", URL, "\n\n") 
                 
        Fname <- "TMP.tif"
        if(quiet) {
           optUSR <- options(warn = -2)
           on.exit(options(optUSR))
        }
        utils::download.file(URL, Fname, method = 'auto', mode = "wb", cacheOK = FALSE, quiet = quiet)
        
    }
    
    # BathySmall <- terra::rast(Fname, xmn = minLon, xmx = maxLon, ymn = minLat, ymx = maxLat)
    BathySmall <- terra::rast(Fname) # Can't override limits inside a 'SpatRaster' class object. The 'extent' argument does not reduce the figure limits, only the size of the raster plotted.
    # BathySmall <- raster::raster(Fname)
    # BathySmall.NA <- BathySmall
    # terra::NAvalue(BathySmall) <- BathySmall@data@min
    
    if(!is.null(maxElev)) {
       V <- values(BathySmall)
       V[V > maxElev] <- maxElev
       values(BathySmall) <- V
    }
    
    if(!is.null(minElev)) {
       V <- values(BathySmall)
       V[V < minElev] <- minElev
       values(BathySmall) <- V
    }
    
    if(plot) {
    
       if (filled) {  
          terra::plot(BathySmall, col = NA)
          terra::contour(BathySmall, col = Col.fixed, levels = levels.contour, filled = TRUE, plot.title = NULL)
          # terra::contour(BathySmall, add = TRUE, levels = levels.contour)
       }  else {
           terra::plot(BathySmall, alpha = alphaRaster, extent = extent, col = col.bathy)
           if (!is.null(levels.contour)) 
              terra::contour(BathySmall, add = TRUE, levels = levels.contour)    
       }
       
       if (landOverlay) {
           if (filled) {  
               world.h.land.adj <- Imap::world.h.land
               world.h.land.adj[,1] <- world.h.land.adj[,1] - 0.10
               world.h.borders.adj <- Imap::world.h.borders
               world.h.borders.adj[,1] <- world.h.borders.adj[,1] - 0.10
               Imap::imap(list(world.h.land.adj, world.h.borders.adj), longrange = c(minLon, maxLon), latrange = c(minLat, maxLat), add = TRUE, poly = col.imap, zoom = FALSE)
           } else           
               Imap::imap(list(Imap::world.h.land, Imap::world.h.borders), longrange = c(minLon, maxLon), latrange = c(minLat, maxLat), add = TRUE, poly = col.imap, zoom = FALSE)
       }
       
       if (plotPoints) 
           points(LongLat[LongLat[, 1] >= minLon & LongLat[, 1] <= maxLon & LongLat[, 2] >= minLat & LongLat[, 2] <= maxLat, ],
                  col = col.pts, pch = pch.pts, cex = cex.pts)
                  
       if (!is.null(polygons)) {
           col.poly <- rep(col.poly, length = length(polygons))
           for (i in 1:length(polygons)) polygon(polygons[[i]], col = col.poly[i], border = border.poly, lwd = lwd.poly)
       }
    }
    
    if (plot3D) {
            xy <- terra::xyFromCell(BathySmall, 1:ncell(BathySmall))
            BathySmall.xyz <- data.frame(xy, z = c(t(terra::as.matrix(BathySmall))))
            names(BathySmall.xyz) <- c("Longitude", "Latitude", "Elevation")
            BathySmall.xyz$Splits <- JRWToolBox::factor.f(BathySmall.xyz$Elevation, 
                  (max(BathySmall.xyz$Elevation, na.rm = T) - min(BathySmall.xyz$Elevation, na.rm = T))/254)
            colTable <- data.frame(Splits = levels(BathySmall.xyz$Splits), Color = rev(terrain.colors(255)))
            BathySmall.xyz <- JRWToolBox::match.f(BathySmall.xyz, colTable, "Splits", "Splits", "Color")
            rgl::plot3d(BathySmall.xyz, col = BathySmall.xyz$Color)
    }
    
    if (GoogleEarth) {
           assign("alphaGoog", alphaGoog, pos = 1)
           plotKML::plotKML(BathySmall, colour_scale = rev(terrain.colors(255)), alpha = alphaGoog)
    }
   
    invisible(BathySmall)
}

if(F) {
# https://www.ncei.noaa.gov/maps/grid-extract/


dev.new()
raster::plot(BS, ext= c(-126, -124, 42, 43), col = rev(terrain.colors(255)))


dev.new()
raster::plot(BS, ext= c(-126, -124, 42, 43), col = rev(grDevices::terrain.colors(50)))

dev.new() 
plotRAST(ext= c(-125.5, -124, 42, 43), col.bathy = rev(terrain.colors(255)), levels.contour = NULL)
 
dev.new() 
plotRAST(ext= c(-125.5, -124, 42, 43), col.bathy = rev(grDevices::terrain.colors(50)), levels.contour = NULL)
  
 
dev.new() 
plotRAST(col.bathy = heat.colors(50))

dev.new()
Imap::plotRAST()

} 



