\name{IMAP-package}
\alias{IMAP-package}
\alias{IMAP}
\docType{package}
\title{
Interactive Mapping
}
\description{
The main function imap() and its allies ilines() and ipts() enables the user to zoom in, zoom out, 
and reset a collection of polygons, lines, or points.  The function select.lines() allows the
interactive selction of one or more lines, whereas select.pts() has the user create
a polygon to select a collection of points within or outside of the polygon. The user has
control over the color and width of the lines, the color of the polygon fill, and the aspect ratio.
}
\details{
\tabular{ll}{
Package: \tab IMAP\cr
Type: \tab Package\cr
Version: \tab 1.32\cr
Date: \tab 2010-02-011\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
Without any input data imap() defaults to using version 2.0 of the GSHHS (Global 
Self-consistent Hierarchical High-resolution Shorelines) polygons.  However, any 
two dimensional user supplied data or a list of multiple data sets can be used.
When the user has finished interacting with the data the result is invisibly returned
as a list of lists which, if saved, can in turn be replotted with imap.  Zooming is 
accomplished by left-clicking in two different locations on the figure to define a
rectangle that will be zoomed into.  Left-clicking outside the plot region (but 
somewhere in the figure region) will zoom out locally.  Double left-clicking on the same spot
will reset the figure.  Right-click to stop.
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov}
}
\examples{
    #	See examples under the main 'imap() function.
}
