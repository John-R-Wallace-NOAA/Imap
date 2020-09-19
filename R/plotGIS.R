plotGIS <- function (LongLat = NULL, polygons = NULL, longrange = c(-126, -124), latrange = c(41.5, 43.5), 
    layer = c('ETOPO1_ice_surface', 'ETOPO1_bedrock', 'crm', 'socal_3as', 'socal_1as')[1], autoLayer = ifelse(missing(layer), TRUE, FALSE), URL = NULL, 
    verbose = TRUE, quiet = TRUE, landOverlay = TRUE, col.imap = "grey40", alphaRaster = 1, col.pts = "red", pch.pts = 16, 
    cex.pts = 0.25, col.poly = col.alpha((grDevices::colorRampPalette(colors = c("darkblue", "blue", "lightblue",
    "lightgreen", "yellow", "orange", "red")))(length(polygons)), alpha), alpha = 0.75, border.poly = NULL, 
    lwd.poly = 1.5, Fname = NULL, levels.contour = if(landOverlay) seq(-100, -2000, by = -100) else seq(-11000, 9000, by = 500),
    GoogleEarth = FALSE, alphaGoog = 0.5, ...) 
{
    .Deprecated('plotRAST')
  
    plotRAST(...)
}




