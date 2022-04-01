\name{adjustPolygon}
\alias{adjustPolygon}
\title{
Interactively move vertices of a polygon
}
\description{
\code{adjustPolygon} interactively moves vertices of a polygon and saves the result.
}
\usage{
adjustPolygon(xy, colLine = "red", colPoly = "purple", alpha = 0.25, borderPoly = 'black', add = FALSE, closePoly = TRUE, lty = "solid", colBg = par()$bg, ...)
}
\arguments{
  \item{xy}{
A matrix or data frame where the x-axis values (perhaps longitude) are in the first column and the y-axis column (perhaps latitude) are in the second column.
}
  \item{colLine}{
The color of line that is drawn while the points are adjusted. The default is red.
}
  \item{colPoly}{
The color for filling the polygon after the moving of the verticies has been finished.
}
  \item{alpha}{
An alpha transparency value for \code{colPoly} where 0 is fully transparent and 1 is opaque. The default is 0.25.
}
 \item{colBorder}{
The color of the polygon border after the moving of the verticies has been finished. The default is black. (The same default as \code{polygon}.)
}
  \item{add}{
When \code{add} is FALSE (the default) \code{plot} is called first. When add = TRUE the polygon is added to the current figure.
}
  \item{closePoly}{
When \code{closePoly} is TRUE (the default) the first point in the polygon is added to the end of polygon, ensuring the polygon is closed.
}
  \item{lty}{
The line type for the drawn lines and polygon. The default is: "solid" (equal to lty = 1).
}
  \item{colBg}{
The color to be used for the background of the device region. The default is the current background in par(): par()$bg
}
  \item{...}{
Addtional arguments passed to the \code{lines} and \code{polygon} functions.
}
}
\value{
A matrix or data frame (equal to the form of the \code{xy} argument), with the final values as adjusted by the interactive manipulations of the vertices.
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov} 
}
\seealso{
\code{\link{draw.polygon}}, \code{\link{select.pts}}
}
\examples{
\dontrun{

# Click on a point and then click where to move the point. Repeat. 
# Right click to stop and the polygon will fill with 'colPoly' color inside a border colored with 'borderPoly'.
EEZ_test <- adjustPolygon(EEZ.Polygon.WestCoast) 
adjustPolygon(EEZ_test, colLine = 'green', colPoly = 'blue', borderPoly = 'green')

imap() # Zoom into the west coast of the contiguous United States, allowing room for the EEZ polygon.
adjustPolygon(EEZ.Polygon.WestCoast, add = TRUE)  # Adjust the polygon as directed above.

}
}

