\name{imapToGIS}
\alias{imapToGIS}
\title{
\code{imap} to \code{plotGIS}
}
\description{
\code{imapToGIS} will zoom into an area using \code{imap} and then use the same bounding box to replace the figure with a raster image using \code{plotGIS}.
}
\usage{
imapToGIS(landOverlay = FALSE, verbose = verbose, ...)
}
\arguments{
  \item{landOverlay}{
Use \code{imap} to overlay land onto the figure.  Default is FALSE.
}
 \item{verbose}{
Should information about the layer used. Default is TRUE. 
}
  \item{...}{
Additional arguments are passed to \code{plotGIS}
}
}
\value{
A list with the latitude and longitude ranges used is invisibly returned.
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov} 
}
\seealso{
\code{\link{imap}}, \code{\link{plotGIS}}
}
\examples{
\dontrun{
# imap() is used to zoom into an area. When the interaction is stopped, the figure will be replaced with a raster image.
imapToGIS()
# For faster download times, use the 'etopo1' layer with its lower resolution for all areas.
imapToGIS(autoLayer = F) 
}
}

