\name{imapToRAST}
\alias{imapToRAST}
\title{
\code{imap} to \code{plotRAST}
}
\description{
\code{imapToRAST} will zoom into an area using \code{imap} and then use the same bounding box to replace the figure with a raster image using \code{plotRAST}.
}
\usage{
imapToRAST(landOverlay = FALSE, ...)
}
\arguments{
  \item{landOverlay}{
Use \code{imap} to overlay land onto the figure.  Default is FALSE.
}
  \item{...}{
Additional arguments passed to \code{plotRAST}
}
}
\value{
A list with the latitude and longitude ranges used is invisibly returned.
}
\author{
John R. Wallace: \email{John.Wallace@noaa.gov} 
}
\seealso{
\code{\link{imap}}, \code{\link{plotRAST}}
}
\examples{
\dontrun{
# imap() is used to zoom into an area. When the interaction is stopped, the figure will be replaced with a raster image.
imapToRAST()

# For faster download times, use the 'etopo1' layer with its lower resolution for all areas.
imapToRAST(autoLayer = F) 

# Compare the bathymetry in Southern California with the 3 available resolutions. (Zoom into a smallish part of So. Cal. on the water with or without land.)
BB <- imapToRAST(layer = 'ETOPO1_ice_surface') 
dev.new()
plotRAST(latrange = BB$latrange, longrange = BB$longrange, layer = 'ETOPO1_ice_surface', landOverlay = F) # Start over with the first figure for the same look as below
dev.new()
plotRAST(latrange = BB$latrange, longrange = BB$longrange, layer = 'crm', landOverlay = F)
dev.new()
plotRAST(latrange = BB$latrange, longrange = BB$longrange, landOverlay = F)

}
}

