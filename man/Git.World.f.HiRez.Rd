\alias{Git.World.f.HiRez}
\name{world}
\alias{world}
\alias{world.h.land}
\alias{world.h.lake}
\alias{world.h.island}
\alias{world.h.pond.in.island}
\alias{world.h.rivers}
\alias{world.h.borders}
\alias{npacific}
\docType{data}
\title{
GSHHS - A Global Self-consistent, Hierarchical, High-resolution Shoreline Database
}
\description{
Both the high and full resolutions of GSHHS have been converted for use in R.  The high resolution of GSHHS is provided
with the Imap package, the full resolution is available from GitHub and can be downloaded to your global environment 
using the \code{Git.World.f.HiRez} function:

> Git.World.f.HiRez()						

The full resolution may not work well on low-end computers. Start with 'ilines(world.f.land)' if there is a problem.
Occasionally starting over with graphics.off() or using gc() for garbage collection may also help.
}
\format{
A (n x 2) matrix with line breaks created with a row of NA's. Longitude is in the first column with lines in the Western 
Hemisphere being negative numbers. The latitude is in the second column with lines in the Southern Hemisphere being negative.
}
\details{
All the GSHHS files are available: land, lake, island, pond.in.island, rivers, borders with either 'world.h' or 'world.f' prefix.

The 'npacific' dataset is a small matrix of the Northern Pacific coastline used in some examples involving lines (not polygons).
}
\source{
http://www.soest.hawaii.edu/wessel/gshhs/gshhs.html
}
\references{
Wessel, P. and Smith, W.H.F., 1996. A global, self-consistent, hierarchical, high-resolution
   shoreline database. J. Geophys. Res., 101, 8741-8743.
}
\examples{
\dontrun{

imap(list(world.h.land, world.h.lake, world.h.island, world.h.pond.in.island, world.h.rivers))

# The works with full resolution; high-end computers only.
imap(list(world.f.land, world.f.lake, world.f.island, world.f.pond.in.island, world.f.borders, world.f.rivers),
            col=c("black", "blue", "forestgreen", "dodgerblue", "cyan", col.alpha('magenta', alpha = 0.5)), 
            poly = c("grey50", "blue", "forestgreen", "dodgerblue", NA, NA))


# The function below will plot selected polygons from 'world.h.land'.
imap.world <- function (x = 1:20, poly = rainbow(n), ...) 
{
    
    n <- length(x)
    a <- 0
    polys <- vector("list", n)
    land.index <- (1:nrow(world.h.land))[is.na(world.h.land[,1])]

    for (i in 1:n) 
        polys[[i + a]] <- world.h.land[land.index[x[i]]:land.index[x[i] + 1], ]

    
    imap(polys, col = 'black', poly = poly, keep.attr = TRUE, ...)
    
    invisible(polys)
}

imap.world()

imap.world(c(5,4,30,50))

}
}
\keyword{datasets}
