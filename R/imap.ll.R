#' The Imap plotting function - usually called by imap()
#' 
#' imap.ll() is the underlying mapping function that is called by imap() for
#' each item in the 'longlat' list.
#' 
#' The function select.lines has similar argumnets to imap.ll but is used to
#' select line seqments. See the Examples Section.
#' 
#' @aliases imap.ll select.lines
#' @param area A 2 dimension matrix or data frame with the first column the
#' longitude or x-axis column and the second column the latitude or y-axis
#' column.  A simple two item list where the first item is the longitude (or x)
#' and the second item is the latitude (or y) can also be supplied.  As with
#' the lines() function a row of NA's will produce a break in the lines
#' (equivalent to a pen-up on a plotter).
#' @param longrange The initial longitudinal or x-axis range to be plotted.
#' @param latrange The initial latitudinal or y-axis range to be plotted.
#' @param poly A single value for the fill color of the polygon(s).
#' @param antarctic Should the perspective be looking down on the South Pole.
#' Default is FALSE.  If TRUE, axes will be set to FALSE.
#' @param arctic Should the perspective be looking down on the North Pole.
#' Default is FALSE.  If TRUE, axes will be set to FALSE.
#' @param oz Should the perspective be with the South Pole on top (As one in
#' Australia (Oz) might like.). Default is FALSE.
#' @param axes One of "map" (the default), "std", or FALSE.  Using "map" will
#' show longitude and latitude markings, "std" will give the standard plotting
#' labels, and FALSE will give no axes at all.
#' @param grid If TRUE a thin dashed grid will be shown. Default in FALSE.
#' @param aspect A figure aspect for which larger values mean the figure will
#' be more high than wide.  Lower values will give a figure that is wider than
#' high.  The default is 1.5.
#' @param add If TRUE, 'area' will added to the current plot.  Default if
#' FALSE.
#' @param zoom If TRUE (default) the zooming feature will be enabled.  Zooming
#' is accomplished by left-clicking in two different locations on the figure to
#' define a rectangle that will be zoomed into.  Left-clicking outside the plot
#' region (but somewhere in the figure region) will zoom out.  Double left
#' clicking on the same spot will reset the plot.  Right-click to stop.
#' @param lines.out.of.bounds Vestigial. May only need to be set to FALSE for
#' older versions of Splus. Default is TRUE.
#' @param tol The tolerance used when double left clicking on the same spot to
#' reset the data.  If very low values of data are supplied or the zooming in
#' is extreme, this value may need to be lowered.  Default is 0.05 .
#' @param \dots Any extra arguments are checked the a 'plt.' prefix.  Any such
#' argument will be applied to the plot() function, all others will be applied
#' to the lines() function. (For those looking to see how this was done, look
#' at the top of the imap.ll() function and search on 'plot.dots' and
#' 'lines.dots')
#' @return Only data contained within the last area to be zoomed in on is
#' invisibly returned as a (n x 2) matrix.
#' @author John R. Wallace: \email{Imap.for.R@@gmail.com} (Limited support)
#' @seealso \code{\link{imap}}, \code{\link{select.pts}}
#' @examples
#' 
#' \dontrun{
#' 
#' ilines(list(npacific, select.lines(npacific)))  # Select one or more line seqments with left-click(s) and then a right-click to stop.
#'  
#' Africa <- select.lines(world.h.land) # Select the African polygon.
#' imap(zoom = FALSE)
#' imap(Africa, poly = 'purple', add = T, zoom = FALSE)
#' 
#' }
#' 
#' @export imap.ll
imap.ll <- 
function (area = npacific, longrange, latrange, poly = NA, antarctic = FALSE, arctic = FALSE, 
    oz = FALSE, axes = "map", grid = FALSE, aspect = 1.5, add = FALSE, zoom = TRUE,
    lines.out.of.bounds = TRUE, tol = 0.05, ...) 
{
    all.dots <- list(...)
    plot.dots <- all.dots[grepl("plt.", names(all.dots))]
    names(plot.dots) <- substring(names(plot.dots), 5)
    lines.dots <- all.dots[!grepl("plt.", names(all.dots))]
    if (is.data.frame(area)) 
        area <- as.matrix(area)
    if (is.matrix(area)) {
        long <- area[, 1]
        lat <- area[, 2]
    }
    else {
        if (is.list(area)) {
            long <- area[[1]]
            lat <- area[[2]]
        }
        else {
            stop("Area object must be a matrix \n\t\t\t(data frames qualify) or a list")
        }
    }
    if (antarctic) 
        lat.cc <- lat + 90
    if (arctic) 
        lat.cc <- 90 - lat
    if (antarctic | arctic) {
        long.cc <- -long
        long <- lat.cc * cos((long.cc * pi)/180)
        lat <- lat.cc * sin((long.cc * pi)/180)
        aspect <- 1
        axes <- FALSE
    }
    if (oz) 
        lat <- -lat
    if (missing(longrange)) 
        longrange <- range(long, na.rm = TRUE)
    if (missing(latrange)) 
        latrange <- range(lat, na.rm = TRUE)
    longrange <- sort(longrange)
    latrange <- sort(latrange)
    longrange[1] <- longrange[1] - abs(longrange[2] - longrange[1])/200
    longrange[2] <- longrange[2] + abs(longrange[2] - longrange[1])/200
    latrange[1] <- latrange[1] - abs(latrange[2] - latrange[1])/200
    latrange[2] <- latrange[2] + abs(latrange[2] - latrange[1])/200
    if (!add) {
        plot(longrange, latrange, xlab = "", ylab = "", type = "n", 
            axes = FALSE)
        if (aspect <= 0) 
            stop("Aspect ratio must be greater than zero.")
        par(new = TRUE, pty = "m")
        usr <- par()$usr
        pin <- c(0.76, 0.76) * par()$din
        ud <- c(usr[2] - usr[1], usr[4] - usr[3])
        x <- ((1/aspect) * ud[1] * pin[2])/ud[2]
        if (x <= pin[1]) 
            par(pin = c(x, pin[2]))
        else par(pin = c(pin[1], (aspect * ud[2] * pin[1])/ud[1]))
        xaxp <- par()$xaxp
        xticks <- round(seq(xaxp[1], xaxp[2], len = xaxp[3] + 
            1), 5)
        yaxp <- par()$yaxp
        yticks <- round(seq(yaxp[1], yaxp[2], len = yaxp[3] + 
            1), 5)
        if (axes == "map") {
            do.call(plot, c(list(x = longrange, y = latrange, 
                xlab = "Longitude", ylab = "Latitude", type = "n", 
                xaxt = "n", yaxt = "n"), plot.dots))
            long.labels <- ifelse(abs(xticks) == 180, "180", 
                ifelse(xticks == 0, "0", ifelse(xticks > 0, paste(xticks, 
                  "E", sep = ""), ifelse(xticks < -180, paste(xticks + 
                  360, "E", sep = ""), paste(-xticks, "W", sep = "")))))
            lat.labels <- ifelse(yticks == 0, "0", ifelse(yticks > 
                0, paste(yticks, ifelse(oz, "S", "N"), sep = ""), 
                paste(-yticks, ifelse(oz, "N", "S"), sep = "")))
            axis(1, at = xticks, labels = long.labels)
            axis(3, at = xticks, labels = long.labels, mgp = c(3, 
                0.5, 0))
            axis(2, at = yticks, labels = lat.labels, srt = 90)
            axis(4, at = yticks, labels = lat.labels, srt = 90)
        }
        if (axes == "std") 
            do.call(plot, c(list(x = longrange, y = latrange, 
                xlab = ifelse(is.null(dimnames(area)[[2]]), "", 
                  dimnames(area)[[2]][1]), ylab = ifelse(is.null(dimnames(area)[[2]]), 
                  "", dimnames(area)[[2]][2])), plot.dots))
        if (grid) 
            abline(v = xticks, h = yticks, lty = 2, lwd = 0)
    }
    lat[is.na(lat)] <- latrange[1] + .Machine$double.eps
    long[is.na(long)] <- longrange[1] + .Machine$double.eps
    tf <- (long >= longrange[1] & long <= longrange[2]) & (lat >= 
        latrange[1] & lat <= latrange[2])
    "tf.1 <<- tf"
    "print(c(length(tf), sum(tf)))"
    lat[lat == latrange[1] + .Machine$double.eps] <- NA
    long[long == longrange[1] + .Machine$double.eps] <- NA
    if (sum(tf) > 2) 
        tf.na <- !(is.na(lat[tf]) & c(FALSE, is.na(lat[tf])[1:(length(lat[tf]) - 
            1)]))
    else tf.na <- NA
    "print(c(length(tf), sum(tf)))"
    "print(sum(tf)/length(tf))"
    if(lines.out.of.bounds) {
        do.call(lines, c(list(x = long, y = lat), lines.dots))
        if(!is.na(poly))
             polygon(x = long, y = lat, col = poly, border = NA)
    } else {
        do.call(lines, c(list(x = long[tf][tf.na], y = lat[tf][tf.na]), lines.dots))
        if(!is.na(poly))
             polygon(x = long[tf][tf.na], y = lat[tf][tf.na], col = poly, border = NA)
    } 
    z.area <- area[tf, drop = TRUE][tf.na, drop = FALSE]

    if (zoom) {
        if (is.list(c1 <- locator(1))) {
            points(c1, pch = 3, cex = 3, col = 2)
            points(c2 <- locator(1), pch = 3, cex = 3, col = 2)
            longrange <- c(c1$x, c2$x)
            latrange <- c(c1$y, c2$y)
            if (abs(c1$x - c2$x) < tol & abs(c1$y - c2$y) < tol) {
                points(c1, pch = 0, cex = 3, col = 3)
                rm(longrange, latrange, inherits = TRUE)
                z.area <- imap.ll(area, poly = poly, 
                  antarctic = antarctic, arctic = arctic, oz = oz, 
                  axes = axes, grid = grid, aspect = aspect, zoom = zoom, 
                  lines.out.of.bounds = lines.out.of.bounds, tol = tol, ...)
            }
            else z.area <- imap.ll(area, longrange, latrange, poly = poly, 
                antarctic = antarctic, arctic = arctic, 
                oz = oz, axes = axes, grid = grid, aspect = aspect, zoom = zoom,
                lines.out.of.bounds = lines.out.of.bounds, tol = tol, ...)
        }
    }
    invisible(matrix(z.area, ncol = 2))
}

