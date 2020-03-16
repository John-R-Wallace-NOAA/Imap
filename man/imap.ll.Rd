\name{imap.ll}
\alias{imap.ll}
\alias{select.lines}
\title{
The Imap plotting function - usually called by imap()
}
\description{
imap.ll() is the underlying mapping function that is called by imap() for each item in the 'longlat' list.
}
\usage{
imap.ll(area = npacific, longrange, latrange, poly = NA, antarctic = FALSE, arctic = FALSE, oz = FALSE, axes = "map", 
       grid = FALSE, aspect = 1.5, add = FALSE, zoom = TRUE, lines.out.of.bounds = TRUE, tol = 0.05, 
       cex.xlab = 1, cex.ylab = 1, cex.axis = 1, lwd.ticks = 1, ...)

select.lines(area = npacific, longrange, latrange, poly = NA, antarctic = FALSE, arctic = FALSE, oz = FALSE, axes = "map",
       grid = FALSE, aspect = 1.5, add = FALSE, zoom = TRUE, lines.out.of.bounds = TRUE, tol = 0.005, ...)
}
\arguments{
  \item{area}{
      A 2 dimension matrix or data frame with the first column the longitude or x-axis column and the second column the latitude or y-axis column.
      A simple two item list where the first item is the longitude (or x) and the second item is the latitude (or y) can also be supplied.  As with 
      the lines() function a row of NAs will produce a break in the lines (equivalent to a pen-up on a plotter).
}
  \item{longrange}{
     The initial longitudinal or x-axis range to be plotted.
}
  \item{latrange}{
     The initial latitudinal or y-axis range to be plotted.
}
  \item{poly}{
     A single value for the fill color of the polygon(s).
}
  \item{antarctic}{
     Should the perspective be looking down on the South Pole. Default is FALSE.  If TRUE, axes will be set to FALSE.
}
  \item{arctic}{
     Should the perspective be looking down on the North Pole. Default is FALSE.  If TRUE, axes will be set to FALSE.
}
  \item{oz}{
     Should the perspective be with the South Pole on top (As one in Australia (Oz) might like.). Default is FALSE.
}
  \item{axes}{
     One of "map" (the default), "std", "latOnly", or FALSE.  Using "map" will show longitude and latitude markings, 
     "std" will give the standard plot markings, "latOnly" will give only latitude markings, and FALSE 
     will give no axes, labels, nor outside box.
}
  \item{grid}{
     If TRUE thin grid lines will be shown. Default is FALSE.
}
  \item{grid.col}{
     The color for the grid lines when grid = TRUE. Default is col.alpha("grey34").
}
  \item{aspect}{
    A figure aspect for which larger values mean the figure will be more high than wide. Lower values will give a figure that is wider than high.
    The default is 1.5.
}
  \item{add}{
     If TRUE, 'area' will added to the current plot.  Default is FALSE.
}
  \item{zoom}{
     If TRUE (default) the zooming feature will be enabled.  Zooming is accomplished by left-clicking in two different locations on the figure to define a
     rectangle that will be zoomed into.  Left-clicking outside the plot region (but somewhere in the figure region) will zoom out.  Double left clicking
     on the same spot will reset the plot.  Right-click to stop.
}
  \item{lines.out.of.bounds}{
      Vestigial. May only need to be set to FALSE for older versions of Splus. Default is TRUE.
}
  \item{tol}{
     The tolerance used when double left clicking on the same spot to reset the data.  If very low values of data are supplied or the zooming in is
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
          Any extra arguments are checked the a 'plt.' prefix.  Any such argument will be applied to the plot() function, all others will be applied to the
     lines() function. (For those looking to see how this was done, look at the top of the imap.ll() function and search on 'plot.dots' and 'lines.dots')
}
}
\details{
The function select.lines has similar arguments to imap.ll but is used to select line seqments. See the Examples Section.
}
\value{
Only data contained within the last area to be zoomed in on is invisibly returned as a (n x 2) matrix.
}

\author{
John R. Wallace: \email{John.Wallace@noaa.gov}
}

\seealso{
\code{\link{imap}}, \code{\link{select.pts}}
}

\examples{
\dontrun{

ilines(list(npacific, select.lines(npacific)))  # Select one or more line seqments with left-click(s) and then a right-click to stop.
 
Africa <- select.lines(world.h.land) # Select the African polygon.
imap(zoom = FALSE)
imap(Africa, poly = 'purple', add = T, zoom = FALSE)

}
}

