#' Interactive Mapping
#' 
#' Zoom in and out of maps or any supplied lines or points with control for
#' color, fill, and aspect.
#' 
#' The functions ilines() calls imap() with the defaults for 'fill' and
#' 'aspect' set to FALSE and 2, respectively.  The function ipts() calls imap()
#' with a call to 'type' set to "p" for points, 'axes' set to "std", and 'fill'
#' set to FALSE.  arguments.
#' 
#' @aliases imap ilines ipts
#' @param longlat Two dimensional user supplied data in the form of a matrix,
#' dataframe, a list of of such data, or a list of lists as returned by this
#' function (see Value).  The first column should be the longitude or x-axis
#' column and the second the latitude or y-axis column.  As with the lines()
#' function a row of NA's will produce a break in the line (equivalent to a
#' pen-up on a plotter).
#' @param longrange The initial longitudinal or x-axis range to be plotted.
#' @param latrange The initial latitudinal or y-axis range to be plotted.
#' @param zoom If TRUE (default) the zooming feature will be enabled.  Zooming
#' is accomplished by left clicking in two different locations on the figure to
#' define a rectangle that will be zoomed into.  Left clicking outside the plot
#' region (but somewhere in the figure region) will zoom out.  Double left
#' clicking on the same spot will reset the plot.  Right click to stop.
#' @param col A single value or vector for the color of the lines.  If a list
#' of is supplied to 'longlat' then each item of the list will, in turn, get a
#' color from the vector of colors supplied.  The colors will be reused if
#' necessary.
#' @param fill If TRUE (default) the polygon fill feature will be enabled. The
#' function ilines() calls imap with the default for fill = FALSE.
#' @param poly A single value or vector for the fill color(s) of the
#' polygon(s).  If a list of is supplied to 'longlat' then each item of the
#' list will, in turn, get a fill color from the vector of colors supplied.
#' The colors will be reused if necessary.  If a NA is in the vector, no
#' polygon fill will be done of the corresponding item in the 'longlat' list.
#' @param lwd A single value or vector for the width of the lines.
#' @param keep.attr Will the attributes of line color, line width, and polygon
#' fill color be taken from the supplied list of lists (as returned by this
#' function) or should the supplied values be used.  Default is TRUE. See the
#' examples.
#' @param add.all Should all the items in 'longlat' be added to the current
#' plot.  Default is FALSE.
#' @param bg The background color. The default is "grey81".  The background
#' color is not a value returned by this function.
#' @param tol The tolerance used when double left clicking on the same spot to
#' reset the data.  If very low values of data are supplied or zooming in is
#' extreme, this value may need to be lowered.  Default is 0.05 .
#' @param \dots Any extra arguments are checked for the 'plt.' prefix.  Any
#' such argument will be applied to the plot() function, all others will be
#' applied to the lines() function. (For those wanting to know how this was
#' done, look at the top of the imap.ll() function and search on 'plot.dots'
#' and 'lines.dots'.)
#' @param aspect See the help for imap.ll() for information on the 'aspect'
#' argument.
#' @param axes See the help for imap.ll() for information on the 'axes'
#' argument.
#' @return Polygons are often broken by zooming in, so saving the results
#' invisibly returned by imap() is often best done for lines and points when
#' fill = FALSE. However the 'poly' argument is saved for each item in the
#' value returned.  The arguments 'longrange' and 'latrange' with 'zoom =
#' FALSE' can be used to return to a zoomed in area with polygon colors intact.
#' See the examples below.
#' 
#' The value returned is a list of lists, where each item in the primary list
#' is an item from the original list given to argument 'longlat' (or created
#' from the object given to 'longlat'). Each primary item has the following
#' components.
#' 
#' \item{ll}{The longlat lines (or pts) after zooming for this primary item.}
#' \item{col}{The color for the lines of this primary item.} \item{lwd}{The
#' line width for the lines of this primary item.} \item{poly}{The polygon
#' color (or NA) for the polygon of this primary item.}
#' @author John R. Wallace: \email{Imap.for.R@@gmail.com} (Limited support)
#' @seealso \code{\link{imap.ll}}, \code{\link{select.pts}},
#' \code{\link{world}}, \code{\link{gdist}}
#' @examples
#' 
#' \dontrun{
#'         if(.Platform$OS.type == "windows")
#'             windows.options(width=13, height=9)  # Set the window to be wider than high.
#' 
#' 	imap()  # On the figure, left-click two corners of an area you would like to zoom in on.
#'                 # Creating a rectangle outside the figure region will zoom out locally.
#'                 # Double left-click on the same spot to zoom back out to the original data. Right-click to stop.
#'                  
#'         imap(antarctic = TRUE)  # Reset with two left-clicks in the same spot for a more symmetrical picture.
#' 
#' 	imap(list(world.h.land, imap(world.h.land)[[1]]$ll))  # Whatever area is selected in the first use of imap()
#'                                                               # will be a different color in the second imap().
#' 
#'         col.alpha('magenta', alpha = 0.5)  # Look at the color value for magenta that has an alpha transparency value of 0.5 .
#' 
#' 	# Use this color so there is transparency where rivers and borders are the in the same location.
#'         # Also note that the land was made a lighter grey by going from grey40 to grey50.
#'         imap(list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.borders, world.h.rivers),
#'             longrange = c(-89, -99), latrange = c(41, 50), col=c("black", "blue",  "forestgreen", "dodgerblue",
#'             "cyan", col.alpha('magenta', alpha = 0.5)), poly = c("grey50", "blue", "forestgreen", "dodgerblue", NA, NA), zoom = FALSE)
#'         
#'         # All the world's rivers with zoom turned back on (may take a while to finish).
#'         imap(list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.borders, world.h.rivers),
#'             col=c("black", "blue", "forestgreen", "dodgerblue", "cyan", col.alpha('magenta', alpha = 0.5)), 
#'             poly = c("grey50", "blue", "forestgreen", "dodgerblue", NA, NA))
#'  
#'         test <- ilines(list(npacific, select.lines(ilines(npacific)[[1]]$ll))) # Zoom, right-click, select one or more lines, right-click.
#' 	test[[2]]$lwd <- 3                                                     # Change attributes manually and
#'         ilines(test)                                                           # keep the default keep.attr = TRUE, or
#'         test <- ilines(test, col=c("magenta", "goldenrod"), lwd = c(2,5), keep.attr = FALSE)  # set keep.attr = FALSE and use parameter options.
#' 
#'         imap(longrange = c(-84.5, -81), latrange = c(44.5, 47)) # An example of ponds on an island in the Great Lakes Region.
#'                                                                 # Use zoom = F for a static map, to which additional data may be added.
#' 
#'         # To create a file from a finished map use dev.copy2eps(), dev.copy2pdf(), or the following function for 'png'.
#'         dev.copy2png <- function(file = 'Rplot.png', factor.exp = 1, ...) {
#' 
#'         dev.copy(png, file=file, width=par()$fin[1] * factor.exp, height=par()$fin[2] * factor.exp, units="in", res=72, ...)
#'         dev.off()
#'         }
#' 
#' 	
#'         # Use with 'Maps' library	
#'         require(maps)
#'         require(mapproj)   
#' 
#'         map.to.imap <- function (map.list) {
#'           
#'              matrix(cbind(map.list[[1]], map.list[[2]]), ncol = 2)
#'         }
#'  
#' 	# Compare this map
#' 	imap(map.to.imap(map('usa')))
#' 	
#' 	# With this one
#'         windows()
#' 	imap(map.to.imap(map('usa', proj="bonne", param=45)), aspect=1)
#' 	
#' 	# Plot without axes 
#' 	imap(map.to.imap(map('usa', proj="bonne", param=45)), aspect=1, axes=FALSE)	
#' 
#' 	# Select an area to highlight and then re-zoom
#' 	ilines(list(map.to.imap(map(projection = "gnomonic")),
#'            ilines(map.to.imap(map(projection = "gnomonic")), aspect=1.5, axes=FALSE)[[1]]$ll), aspect=1.5, axes=FALSE)
#' }
#' 
#' @export imap
imap <-
function (longlat = list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.borders), 
    longrange, latrange, zoom = TRUE, col= c("black", "blue", "forestgreen", "dodgerblue", "cyan"), fill = TRUE, 
    poly = c("grey40", "blue", "forestgreen", "dodgerblue", NA), lwd = 1, keep.attr = TRUE, add.all = FALSE, bg = "grey81", tol = 0.05, ...) 
{
    par(bg = bg)

  
    if (is.matrix(longlat) | is.data.frame(longlat)) {
        LongLat <- longlat
        longlat <- list()
        longlat[[1]] <- list(ll=as.matrix(LongLat), col = col[1], lwd = lwd[1], poly=poly[1])
        rm(LongLat)
    }   

    if (!is.list(longlat[[1]])) {
    
        N <- length(longlat)
        col <- rep(col, length = N)
        lwd <- rep(lwd, length = N)
        poly <- rep(poly, length = N)
      
        LongLat <- longlat
        longlat <- list()
        for(i in 1:N)
             longlat[[i]] <- list(ll = LongLat[[i]], col=col[i], lwd=lwd[i], poly = poly[i])
        rm(LongLat)

     }  else {

        if(!keep.attr) {

           N <- length(longlat)
           col <- rep(col, length = N)
           lwd <- rep(lwd, length = N)
           poly <- rep(poly, length = N)
      
           LongLat <- longlat
           longlat <- list()
           for(i in 1:N)
               longlat[[i]] <- list(ll = LongLat[[i]]$ll, col=col[i], lwd=lwd[i], poly = poly[i])
           rm(LongLat)
        }
    }
   
    if (missing(longrange) | missing(latrange)) {
        max.ll <- apply(data.frame(lapply(longlat, function(x) apply(x[['ll']], 
            2, max, na.rm = TRUE))), 1, max)
        min.ll <- apply(data.frame(lapply(longlat, function(x) apply(x[['ll']], 
            2, min, na.rm = TRUE))), 1, min)
    }
    if (missing(longrange)) 
        longrange <- c(min.ll[1], max.ll[1])
    if (missing(latrange)) 
        latrange <- c(min.ll[2], max.ll[2])

    ll.out <- list()
    
    for (i in 1:length(longlat)) {

        ll.out[[i]] <- list(ll=matrix(

             imap.ll(longlat[[i]][['ll']], longrange, latrange, add = ifelse(add.all, TRUE, ifelse(i == 1, 
                FALSE, TRUE)), zoom = FALSE, col = longlat[[i]][['col']], poly = ifelse(fill, longlat[[i]][['poly']], NA),
                lwd = longlat[[i]][['lwd']], ...), 
              
        ncol = 2), col=longlat[[i]][['col']], lwd=longlat[[i]][['lwd']], poly = longlat[[i]][['poly']])
   }   
  
    if (zoom) {
        if (is.list(c1 <- locator(1))) {
            points(c1, pch = 3, cex = 3, col = 2)
            points(c2 <- locator(1), pch = 3, cex = 3, col = 2)
            if (abs(c1$x - c2$x) < tol & abs(c1$y - c2$y) < tol) {
                points(c1, pch = 0, cex = 3, col = 3)
                ll.out <- imap(longlat, zoom = TRUE, fill = fill, bg = bg, keep.attr = keep.attr, tol = tol, ...)
            }
            else ll.out <- imap(longlat, c(c1$x, c2$x), c(c1$y, 
                c2$y), zoom = TRUE, fill = fill, bg = bg, keep.attr = keep.attr, tol = tol, ...)
        }
    }

    for (i in length(ll.out):1) {

        if(all(is.na(ll.out[[i]]$ll)))
            ll.out[[i]] <- NULL   
    }        

    invisible(ll.out)
}

