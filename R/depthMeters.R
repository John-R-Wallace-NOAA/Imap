depthMeters <- function (LongLat = c(-120, 33), layer = c('ETOPO1_ice_surface', 'ETOPO1_bedrock', 'crm', 'socal_3as', 'socal_1as')[1], 
   plot = ifelse(N < 5, TRUE, FALSE), SoCal_1as = TRUE, blockSizeDegs = ifelse(plot, ifelse(SoCal_1as, 0.5, 2), ifelse(SoCal_1as, 0.005, 0.002)), 
   method = "bilinear", verbose = plot, quiet = TRUE, Zero.to.NA = TRUE, plot3D = FALSE, GoogleEarth = FALSE, alphaGoog = 0.5) 
{
    "  "
    "  Examples: depthMeters(c(-125.6875, 48.14417)) # Auto plot and auto 0.5 or 2 deg. block; Depths <- depthMeters(NWDepth[1:10, c('BEST_LON_DD', 'BEST_LAT_DD')]) # Auto no plot and 0.005 deg block "
    "  Example of SoCal 1 arcsec: depthMeters(c(-120, 33))  # Auto uses Southern Cali 1 arc-sec unless SoCal_1as = FALSE  "
    "  "
    "  For getting the bathymetry into R, I followed Tom Wainwright here: http://rstudio-pubs-static.s3.amazonaws.com/53530_1af2d0b5ae1f4a36a75e611d3566f777.html#1  "
    "  Using Coastal Relief Mapping project that has(3 arc-sec) bathymetry for most of the US coastline and 1 arc-sec for Southern CA.  "
    "  U.S. Coastal Relief Model Map is here:  http://www.ngdc.noaa.gov/mgg/coastal/crm.html  "
    "  "
    "    Note that a long/lat point may be in a volume, but outside of the data area, in which case a zero depth is reported.  "
    "    Here is an example: (Imap::depthMeters(rbind(c(0, 0), c(-120, 33), c(-135, 41)), 0.01, Zero.to.NA = F))   "
    "  "
    "   Also, even though the SoCal 1 arcsec extends to the southeastern corner of U.S. West Cost EEZ, the SoCal 3 arcsec coverage does go further south and west.  "
    "   "
    if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
    JRWToolBox::lib(raster)
      
    DepthM <- function(LongLat, layer = 'ETOPO1_ice_surface', plot = TRUE, SoCal_1as = TRUE, blockSizeDegs = 0.01, method = "bilinear", verbose = TRUE, quiet = FALSE, plot3D = FALSE, GoogleEarth = FALSE, alphaGoog = 0.5) {
   
        Long <- as.numeric(LongLat[1])
        Lat <- as.numeric(LongLat[2])
        if (Long > -123 & Long < -115.999999944 & Lat > 30.99972218222 & Lat < 36.99972223022 & SoCal_1as) # Southern California Bight with 1 arc-sec resolution
              SoCal_1as <- TRUE
        else
              SoCal_1as <- FALSE      
               
        minLon <- Long - blockSizeDegs/2
        maxLon <- Long + blockSizeDegs/2
        minLat <- Lat - blockSizeDegs/2
        maxLat <- Lat + blockSizeDegs/2
       
        BathySmall <- Imap::plotRAST(layer = layer, longrange = c(minLon, maxLon), latrange = c(minLat, maxLat), plot = plot, verbose = verbose, 
                               quiet = quiet, plot3D = plot3D, GoogleEarth = GoogleEarth, alphaGoog = alphaGoog)
        
        if (plot) {
           points(LongLat, col = "red", pch = 16)
           cat("The default bounding box size when plotting is larger than the default box when not plotting, this may affect the depth value reported.\n\n")
        }   
        
        -raster::extract(BathySmall, LongLat, method = method)
    }  
    
    if (is.null(nrow(LongLat))) 
        LongLat <- as.data.frame(t(LongLat))
    N <- nrow(LongLat)
    Out <- rep(NA, N)
    
    for (i in 1:N) {
        if (N >= 5) 
            JRWToolBox::bar(i, N)
        try(Out[i] <- DepthM(LongLat[i, , drop = FALSE], layer = layer, plot = plot, SoCal_1as = SoCal_1as, blockSizeDegs = blockSizeDegs, method = method, verbose = verbose, 
                               quiet = quiet, plot3D = plot3D, GoogleEarth = GoogleEarth, alphaGoog = alphaGoog), silent = TRUE)
    }
    
    if (any(Out %in% 0) & Zero.to.NA) {
        Out[Out %in% 0] <- NA
        warning("Depths equal to exactly zero were converted to NA.", call. = FALSE)
    }
    
    Out
}



