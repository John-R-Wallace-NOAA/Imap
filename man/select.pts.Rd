\name{select.pts, draw.polygon, inside.polygon, draw.lines, col.alpha}
\alias{select.pts}
\alias{draw.polygon}
\alias{inside.polygon}
\alias{draw.lines}
\alias{col.alpha}
\title{
Select points; draw polygons; draw lines
}
\description{
Functions for selecting points and drawing polygons and lines.
}
\usage{
select.pts(pts,  list.of.lists.obj = 1, outside.poly = FALSE, col = "blue", alpha = 0.5, lty = 1, ...)

draw.polygon(col = "blue", alpha = 0.5, lty = 1, ...)

inside.polygon(pts, h)

draw.lines(col = "red", alpha = 0.5, ...)

col.alpha(col, alpha = 0.5)
}
\arguments{
  \item{pts}{
A (n x 2) matrix of points.
}
  \item{list.of.lists.obj}{
Which object in the list of lists, as returned by imap(), should be used to select points from.  Objects with no selected points 
are dropped by imap() and therefore using str() on the value returned by imap() can be useful in revealing which object number is 
wanted when, for example, islands or ponds on islands are to be selected.
}
  \item{outside.poly}{
Should the points which are outside the polygon be selected. The default is FALSE, which gives the points inside the polygon.
}
  \item{col}{
The color to fill the polygon for draw.polygon() or the line color for draw.lines(). The parameter can be either a color name, 
a hexadecimal string of the form "#FF00FF80", or an integer i meaning palette()[i].
}
  \item{alpha}{
An alpha transparency value where 0 is fully transparent and 1 is opaque.
}
  \item{lty}{
The polygon line type.
}
  \item{h}{
A hull or polygon defined by a matrix ([k+1] x 2) of (ordered) vertices, [with last row = first row].
}
  \item{\dots}{
Additional graphical parameters for the functions points(), polygon() or lines().
}
}
\details{
The function select.pts() returns points that are inside (or outside) a polygon.  The polygon is interactively drawn about points (or lines) which normally would 
have been previously plotted.  After at least three vertices of the polygon have been selected by left-clicking on the figure the interaction is 
eventually stopped by a right-click. The resulting polygon is closed and filled with color 'col' after alpha level 'alpha' has been applied via the 
col.alpha() fucntion.

Note that col.alpha() conveniently puts together the color and alpha level into a new color that can be directly used by functions like lines() and polygon().

If two or more colors are given to draw.lines() they will be recycled through the drawing of the line segments.  See the Examples Section.

}
\value{
The function select.pts() explicitly returns a (n x 2) data frame of the values selected.  However, draw.polygon() and draw.lines() return their values invisibly. 
}
\references{
The inside.polygon() function is revised from the the function "Inside()" by Joseph S. Verducci (Snews: 09 Feb 1999). 
In particular, exactly equal adjacent x values will cause the original function to fail.
See  \url{http://www.biostat.wustl.edu/archives/html/s-news/2002-07/msg00020.html} 
}
\author{
John R. Wallace: \emailemail{John.Wallace@noaa.gov} 
}
\note{
If points inside a polygon were not selected try again with a less irregular polygon.
}
\seealso{
\code{\link{imap}}, \code{\link{gdist.total}}, \code{\link{polygon}}
}
\examples{
\dontrun{

plot(tmp <- cbind(1:100, rnorm(100)))
select.pts(tmp) # Left-click three or more times, right-click to stop.

big.island <- select.pts(imap())  # Zoom into the Hawaiian Islands, right-click to stop, then put a polygon around the Big Island.
imap(list(world.h.land, big.island))  # Re-zoom into the Hawaiian Islands.  Use ilines() if a line was selected which is not a polygon.

imap()  # Zoom in to an area, right-click to stop.
draw.lines()  # Left-click two or more times, right-click to stop.
gdist.total(draw.lines(rainbow(12), lwd = 4, lty = 2))  # Left-click two or more times, right-click to stop.
draw.polygon('purple', alpha = 0.2) # Left-click three or more times, right-click to stop.


col.alpha('cyan', alpha = 0.3)

hist(rnorm(1e5), col = col.alpha('cyan', alpha = 0.3))
hist(rnorm(1e5, 2), col = col.alpha('magenta', alpha = 0.3), add=T)
}
}
