\name{imap}
\alias{imap}
\alias{ilines}
\alias{ipts}
\title{
Interactive Mapping
}
\description{
Zoom in and out of maps or any supplied lines or points with control for color, fill, and aspect.
}
\usage{
imap(longlat = list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.borders), 
        longrange, latrange, zoom = TRUE, col = c("black", "blue", "forestgreen", "dodgerblue", "cyan"), fill = TRUE, 
        poly = c("grey40", "blue", "forestgreen", "dodgerblue", NA), lwd = 1, keep.attr = TRUE, add.all = FALSE, bg = "grey81", tol = 0.05, 
        cex.xlab = 1, cex.ylab = 1, cex.axis = 1, lwd.ticks = 1, ...)


ilines(..., fill = FALSE, aspect = 2)

ipts(longlat = npacific, axes = 'std', ...)
}
\arguments{
  \item{longlat}{
     Two dimensional user supplied data in the form of a matrix, dataframe, a list of of such data, or a list of lists as returned by
     this function (see Value).  The first column should be the longitude or x-axis column and the second the latitude or y-axis column.
     As with the lines() function a row of NAs will produce a break in the line (equivalent to a pen-up on a plotter).
}
  \item{longrange}{
     The initial longitudinal or x-axis range to be plotted.
}
  \item{latrange}{
     The initial latitudinal or y-axis range to be plotted.
}
  \item{zoom}{
     If TRUE (default) the zooming feature will be enabled.  Zooming is accomplished by left clicking in two different locations on the figure to define a
     rectangle that will be zoomed into.  Left clicking outside the plot region (but somewhere in the figure region) will zoom out.  Double left clicking
     on the same spot will reset the plot.  Right click to stop.  
}
  \item{col}{
     A single value or vector for the color of the lines.  If a list of is supplied to 'longlat' then each item of the list will, in turn, get
     a color from the vector of colors supplied.  The colors will be reused if necessary.
}
  \item{fill}{
   If TRUE (default) the polygon fill feature will be enabled. The function ilines() calls imap with the default for fill = FALSE.
}
  \item{poly}{
     A single value or vector for the fill color(s) of the polygon(s).  If a list of is supplied to 'longlat' then each item of the list will, in turn, get
     a fill color from the vector of colors supplied.  The colors will be reused if necessary.  If a NA is in the vector, no polygon fill will
     be done for the corresponding item in the 'longlat' list.
}
  \item{lwd}{
     A single value or vector for the width of the lines. 
}
  \item{keep.attr}{
     Will the attributes of line color, line width, and polygon fill color be taken from the supplied list of lists (as returned by this function) or should
     the supplied values be used.  Default is TRUE. See the examples.
}
  \item{add.all}{
     Should all the items in 'longlat' be added to the current plot.  Default is FALSE.
}
  \item{bg}{
     The background color. The default is "grey81".  The background color is not a value returned by this function.
}
  \item{tol}{
     The tolerance used when double left clicking on the same spot to reset the data.  If very low values of data are supplied or zooming in is
     extreme, this value may need to be lowered.  Default is 0.05 .
}
  \item{cex.xlab}{
     Character expansion for the 'x axis' label.
}
  \item{cex.ylab}{
     Character expansion for the 'y axis' label.
}
  \item{cex.axis}{
     Character expansion for the tick-mark labels.
}
  \item{lwd.ticks}{
     Line width of the tick marks.
}
  \item{\dots}{
     Any extra arguments are checked for the 'plt.' prefix.  Any such argument will be applied to the plot() function, all others will be applied to the
     lines() function. (For those wanting to know how this was done, look at the top of the imap.ll() function and search on 'plot.dots' and 'lines.dots'.)
}
 \item{aspect}{
     See the help for imap.ll() for information on the 'aspect' argument.
}
  \item{axes}{
     See the help for imap.ll() for information on the 'axes' argument.
}
}
\details{
The functions ilines() calls imap() with the defaults for 'fill' and 'aspect' set to FALSE and 2, respectively.  The function ipts() calls imap() with a call to 
'type' set to "p" for points, 'axes' set to "std", and 'fill' set to FALSE. 
arguments.
}
\value{
Polygons are often broken by zooming in, so saving the results invisibly returned by imap() is often best done for lines and points when fill = FALSE.
However the 'poly' argument is saved for each item in the value returned.  The arguments 'longrange' and 'latrange' with 'zoom = FALSE' can be used to 
return to a zoomed in area with polygon colors intact.  See the examples below.

The value returned is a list of lists, where each item in the primary list is an item from the original list given to argument 'longlat' (or created from
the object given to 'longlat'). Each primary item has the following components.


\item{ll}{The longlat lines (or pts) after zooming for this primary item.}
\item{col}{The color for the lines of this primary item.}
\item{lwd}{The line width for the lines of this primary item.}
\item{poly}{The polygon color (or NA) for the polygon of this primary item.}
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov}
}
\seealso{
\code{\link{imap.ll}}, \code{\link{select.pts}}, \code{\link{world}}, \code{\link{gdist}}
}
\examples{
\dontrun{
        if(.Platform$OS.type == "windows")
            windows.options(width=13, height=9)  # Set the window to be wider than high.

	imap()  # On the figure, left-click two opposite corners of a rectangular area you would like to zoom in on.
                # Creating a large rectangle outside the figure region will zoom out locally.
                # Double left-click on the same spot to zoom back out to the original data. Right-click to stop.
                 
        imap(antarctic = TRUE)  # Reset with two left-clicks in the same spot for a more symmetrical picture.

	imap(list(world.h.land, imap(world.h.land)[[1]]$ll))  # Whatever area is selected in the first use of imap()
                                                              # will be a different color in the second imap().

        col.alpha('magenta', alpha = 0.5)  # Look at the color value for magenta that has an alpha transparency value of 0.5 .

	# Use this color so there is transparency where rivers and borders are the in the same location.
        # Also note that the land was made a lighter grey by going from grey40 to grey50.
        imap(list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.borders, world.h.rivers),
            longrange = c(-89, -99), latrange = c(41, 50), col=c("black", "blue",  "forestgreen", "dodgerblue",
            "cyan", col.alpha('magenta', alpha = 0.5)), poly = c("grey50", "blue", "forestgreen", "dodgerblue", NA, NA), zoom = FALSE)
        
        # All the world's rivers with zoom turned back on (may take a while to finish).
        imap(list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.borders, world.h.rivers),
            col=c("black", "blue", "forestgreen", "dodgerblue", "cyan", col.alpha('magenta', alpha = 0.5)), 
            poly = c("grey50", "blue", "forestgreen", "dodgerblue", NA, NA))
 
        test <- ilines(list(npacific, select.lines(ilines(npacific)[[1]]$ll))) # Zoom, right-click, select one or more lines, right-click.
	test[[2]]$lwd <- 3                                                     # Change attributes manually and
        ilines(test)                                                           # keep the default keep.attr = TRUE, or
        test <- ilines(test, col=c("magenta", "goldenrod"), lwd = c(2,5), keep.attr = FALSE)  # set keep.attr = FALSE and use parameter options.

        imap(longrange = c(-84.5, -81), latrange = c(44.5, 47)) # An example of ponds on an island in the Great Lakes Region.
                                                                # Use zoom = F for a static map, to which additional data may be added.

        # To create a file from a finished map use dev.copy2eps(), dev.copy2pdf(), or the following function for 'png'.
        dev.copy2png <- function(file = 'Rplot.png', factor.exp = 1, ...) {

        dev.copy(png, file=file, width=par()$fin[1] * factor.exp, height=par()$fin[2] * factor.exp, units="in", res=72, ...)
        dev.off()
        }

	
        # Use with 'Maps' library	
        require(maps)
        require(mapproj)   

        map.to.imap <- function (map.list) {
          
             matrix(cbind(map.list[[1]], map.list[[2]]), ncol = 2)
        }
 
	# Compare this map
	imap(map.to.imap(map('usa')))
	
	# With this one
        windows()
	imap(map.to.imap(map('usa', proj="bonne", param=45)), aspect=1)
	
	# Plot without axes 
	imap(map.to.imap(map('usa', proj="bonne", param=45)), aspect=1, axes=FALSE)	

	# Select an area to highlight and then re-zoom
	ilines(list(map.to.imap(map(projection = "gnomonic")),
           ilines(map.to.imap(map(projection = "gnomonic")), aspect=1.5, axes=FALSE)[[1]]$ll), aspect=1.5, axes=FALSE)
}
}

