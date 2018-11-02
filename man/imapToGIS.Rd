\name{imapToGIS}
\alias{imapToGIS}
\title{
\code{imap} to \code{plotGIS}
}
\description{
\code{imapToGIS} will zoom into an area using \code{imap} and then use the same bounding box to replace the figure with a raster image using \code{plotGIS}.
}
\usage{
imapToGIS(landOverlay = FALSE, verbose = TRUE, ...)
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

# Compare the bathymetry in Southern California with the 3 available resolutions. (Zoom into a smallish part of So. Cal. on the water with or without land.)
BB <- imapToGIS(autoLayer = F) 
dev.new()
plotGIS(latrange = BB$latrange, longrange = BB$longrange, autoLayer = F, landOverlay = F, verbose = T) # Start over for the exact same look as below
dev.new()
plotGIS(latrange = BB$latrange, longrange = BB$longrange, layer = 'crm', autoLayer = F, landOverlay = F, verbose = T)
dev.new()
plotGIS(latrange = BB$latrange, longrange = BB$longrange, landOverlay = F, verbose = T)
}
}

