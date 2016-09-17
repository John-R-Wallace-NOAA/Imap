

#' Interactive Mapping
#' 
#' The main function imap() and its allies ilines() and ipts() enables the user
#' to zoom in, zoom out, and reset a collection of polygons, lines, or points.
#' The function select.lines() allows the interactive selction of one or more
#' lines, whereas select.pts() has the user create a polygon to select a
#' collection of points within or outside of the polygon. The user has control
#' over the color and width of the lines, the color of the polygon fill, and
#' the aspect ratio.
#' 
#' \tabular{ll}{ Package: \tab IMAP\cr Type: \tab Package\cr Version: \tab
#' 1.32\cr Date: \tab 2010-02-011\cr License: \tab GPL (>= 2)\cr LazyLoad: \tab
#' yes\cr } Without any input data imap() defaults to using version 2.0 of the
#' GSHHS (Global Self-consistent Hierarchical High-resolution Shorelines)
#' polygons.  However, any two dimensional user supplied data or a list of
#' multiple data sets can be used. When the user has finished interacting with
#' the data the result is invisibly returned as a list of lists which, if
#' saved, can in turn be replotted with imap.  Zooming is accomplished by
#' left-clicking in two different locations on the figure to define a rectangle
#' that will be zoomed into.  Left-clicking outside the plot region (but
#' somewhere in the figure region) will zoom out locally.  Double left-clicking
#' on the same spot will reset the figure.  Right-click to stop.
#' 
#' @name IMAP-package
#' @aliases IMAP-package IMAP
#' @docType package
#' @author John R. Wallace: \email{Imap.for.R@@gmail.com} (Limited support)
#' @examples
#' 
#'     #	See examples under the main 'imap() function.
#' 
NULL





#' Select points; draw polygons; draw lines
#' 
#' Functions for selecting points and drawing polygons and lines.
#' 
#' The function select.pts() returns points that are inside (or outside) a
#' polygon.  The polygon is interactively drawn about points (or lines) which
#' normally would have been previously plotted.  After at least three vertices
#' of the polygon have been selected by left-clicking on the figure the
#' interaction is eventually stopped by a right-click. The resulting polygon is
#' closed and filled with color 'col' after alpha level 'alpha' has been
#' applied via the col.alpha() fucntion.
#' 
#' Note that col.alpha() conveniently puts together the color and alpha level
#' into a new color that can be directly used by functions like lines() and
#' polygon().
#' 
#' If two or more colors are given to draw.lines() they will be recycled
#' through the drawing of the line segments.  See the Examples Section.
#' 
#' @aliases select.pts draw.polygon inside.polygon draw.lines col.alpha
#' @param pts A (n x 2) matrix of points.
#' @param list.of.lists.obj Which object in the list of lists, as returned by
#' imap(), should be used to select points from.  Objects with no selected
#' points are dropped by imap() and therefore using str() on the value returned
#' by imap() can be useful in revealing which object number is wanted when, for
#' example, islands or ponds on islands are to be selected.
#' @param outside.poly Should the points which are outside the polygon be
#' selected. The default is FALSE, which gives the points inside the polygon.
#' @param col The color to fill the polygon for draw.polygon() or the line
#' color for draw.lines(). The parameter can be either a color name, a
#' hexadecimal string of the form "#FF00FF80", or an integer i meaning
#' palette()[i].
#' @param alpha An alpha transparency value where 0 is fully transparent and 1
#' is opaque.
#' @param lty The polygon line type.
#' @param h A hull or polygon defined by a matrix ([k+1] x 2) of (ordered)
#' vertices, [with last row = first row].
#' @param \dots Additional graphical parameters for the functions points(),
#' polygon() or lines().
#' @return The function select.pts() explicitly returns a (n x 2) data frame of
#' the values selected.  However, draw.polygon() and draw.lines() return their
#' values invisibly.
#' @note If points inside a polygon were not selected try again with a less
#' irregular polygon.
#' @author John R. Wallace: \email{Imap.for.R@@gmail.com} (Limited support)
#' @seealso \code{\link{imap}}, \code{\link{gdist.total}},
#' \code{\link{polygon}}
#' @references The inside.polygon() function is revised from the the function
#' "Inside()" by Joseph S. Verducci (Snews: 09 Feb 1999).  In particular,
#' exactly equal adjacent x values will cause the original function to fail.
#' See
#' \url{http://www.biostat.wustl.edu/archives/html/s-news/2002-07/msg00020.html}
#' @examples
#' 
#' \dontrun{
#' 
#' plot(tmp <- cbind(1:100, rnorm(100)))
#' select.pts(tmp) # Left-click three or more times, right-click to stop.
#' 
#' big.island <- select.pts(imap())  # Zoom into the Hawaiian Islands, right-click to stop, then put a polygon around the Big Island.
#' imap(list(world.h.land, big.island))  # Re-zoom into the Hawaiian Islands.  Use ilines() if a line was selected which is not a polygon.
#' 
#' imap()  # Zoom in to an area, right-click to stop.
#' draw.lines()  # Left-click two or more times, right-click to stop.
#' gdist.total(draw.lines(rainbow(12), lwd = 4, lty = 2))  # Left-click two or more times, right-click to stop.
#' draw.polygon('purple', alpha = 0.2) # Left-click three or more times, right-click to stop.
#' 
#' 
#' col.alpha('cyan', alpha = 0.3)
#' 
#' hist(rnorm(1e5), col = col.alpha('cyan', alpha = 0.3))
#' hist(rnorm(1e5, 2), col = col.alpha('magenta', alpha = 0.3), add=T)
#' }
#' 
NULL





#' GSHHS - A Global Self-consistent, Hierarchical, High-resolution Shoreline
#' Database
#' 
#' Both the high and full resolutions of GSHHS have been converted for use in
#' R.  The high resolution of GSHHS is provided with the Imap package, the full resolution is available on GitHub by using the Git.World.f.HiRez() function. 
#' 
#' The full resolution will not work well on low-end computers. Start with
#' 'ilines(world.f.land)' if there is a problem. Occasionally starting over
#' with graphics.off() or using gc() for garbage collection may also help.
#' 
#' All the GSHHS files are available: land, lake, island, pond.in.island,
#' rivers, borders with either 'world.h' or 'world.f' prefix.
#' 
#' The 'npacific' dataset is a small matrix of the Northern Pacific coastline
#' used in some examples involving lines (not polygons).
#' 
#' @name world
#' @aliases world world.h.land world.h.lake world.h.island
#' world.h.pond.in.island world.h.rivers world.h.borders npacific
#' @docType data
#' @format A (n x 2) matrix with line breaks created with a row of NA's.
#' Longitude is in the first column with lines in the Western Hemisphere being
#' negative numbers. The latitude is in the second column with lines in the
#' Southern Hemisphere being negative.
#' @references Wessel, P. and Smith, W.H.F., 1996. A global, self-consistent,
#' hierarchical, high-resolution shoreline database. J. Geophys. Res., 101,
#' 8741-8743.
#' @source http://www.soest.hawaii.edu/wessel/gshhs/gshhs.html
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' imap(list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.rivers))
#' 
#' # The works with full resolution; high-end computers only.
#' imap(list(world.f.land, world.f.lake, world.f.island, world.f.pond.in.island, world.f.borders, world.f.rivers),
#'             col=c("black", "blue", "forestgreen", "dodgerblue", "cyan", col.alpha('magenta', alpha = 0.5)), 
#'             poly = c("grey50", "blue", "forestgreen", "dodgerblue", NA, NA))
#' 
#' 
#' # The function below will plot selected polygons from 'world.h.land'.
#' imap.world <- function (x = 1:20, poly = rainbow(n), ...) 
#' {
#'     
#'     n <- length(x)
#'     a <- 0
#'     polys <- vector("list", n)
#'     land.index <- (1:nrow(world.h.land))[is.na(world.h.land[,1])]
#' 
#'     for (i in 1:n) 
#'         polys[[i + a]] <- world.h.land[land.index[x[i]]:land.index[x[i] + 1], ]
#' 
#'     
#'     imap(polys, col = 'black', poly = poly, keep.attr = TRUE, ...)
#'     
#'     invisible(polys)
#' }
#' 
#' imap.world()
#' 
#' imap.world(c(5,4,30,50))
#' 
#' }
#' 
NULL



