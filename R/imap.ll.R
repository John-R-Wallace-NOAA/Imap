imap.ll <- function (area = npacific, longrange, latrange, poly = NA, antarctic = FALSE, arctic = FALSE, oz = FALSE, axes = "map", grid = FALSE, grid.col = col.alpha("grey34"),
    aspect = 1.5, add = FALSE, zoom = TRUE, lines.out.of.bounds = TRUE, tol = 0.05, xlab = list("Longitude", cex = cex.xlab), ylab = list("Latitude", cex = cex.ylab), 
    cex.xlab = 1, cex.ylab = 1, cex.axis = 1, lwd.ticks = 1, last = TRUE, pin = NULL, ...) 
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
        if(is.null(pin)) {
            usr <- par()$usr
            pin <- c(0.76, 0.76) * par()$din
            ud <- c(usr[2] - usr[1], usr[4] - usr[3])
            x <- ((1/aspect) * ud[1] * pin[2])/ud[2]
            if (x <= pin[1]) 
                par(pin = c(x, pin[2]))
            else par(pin = c(pin[1], (aspect * ud[2] * pin[1])/ud[1]))
        } else
           par(pin = pin)
        
        xaxp <- par()$xaxp
        xticks <- round(seq(xaxp[1], xaxp[2], len = xaxp[3] + 1), 5)
        yaxp <- par()$yaxp
        yticks <- round(seq(yaxp[1], yaxp[2], len = yaxp[3] + 1), 5)
        
        if(length(axes) == 1 && axes == "map") axes <- 1:4
        if(length(axes) == 1 && axes == "latOnly") axes <- c(2, 4)
        if(length(axes) == 1 && axes == "std") axes <- 0
        
        if(any(axes %in% 1:4)) {
            do.call(plot, c(list(x = longrange, y = latrange, xlab = xlab, ylab = ylab, type = "n", xaxt = "n", yaxt = "n"), plot.dots))
            long.labels <- ifelse(abs(xticks) == 180, "180", ifelse(xticks == 0, "0", ifelse(xticks > 0, paste(xticks, "E", sep = ""), 
                ifelse(xticks < -180, paste(xticks + 360, "E", sep = ""), paste(-xticks, "W", sep = "")))))
            lat.labels <- ifelse(yticks == 0, "0", ifelse(yticks > 0, paste(yticks, ifelse(oz, "S", "N"), sep = ""), paste(-yticks, ifelse(oz, "N", "S"), sep = "")))
            
            if(any(axes %in% 1))
               axis(1, at = xticks, labels = long.labels, cex.axis = cex.axis, lwd.ticks = lwd.ticks)
               
            if(any(axes %in% 3))
               axis(3, at = xticks, labels = long.labels, mgp = c(3, 0.5, 0), cex.axis = cex.axis, lwd.ticks = lwd.ticks)   
                      
            if(any(axes %in% 2))
              axis(2, at = yticks, labels = lat.labels, srt = 90, cex.axis = cex.axis, lwd.ticks = lwd.ticks)
              
            if(any(axes %in% 4))  
              axis(4, at = yticks, labels = lat.labels, srt = 90, cex.axis = cex.axis, lwd.ticks = lwd.ticks)
        }
        
        if(length(axes) == 1 && axes == 0)
                do.call(plot, c(list(x = longrange, y = latrange, xlab = list(ifelse(is.null(dimnames(area)[[2]]), "", dimnames(area)[[2]][1]), cex = cex.xlab), 
                   ylab = list(ifelse(is.null(dimnames(area)[[2]]), "", dimnames(area)[[2]][2]), cex = cex.ylab)), type = "n", plot.dots))
                   
        if (grid & last) {
            xaxp <- par()$xaxp
            xticks <- round(seq(xaxp[1], xaxp[2], len = xaxp[3] + 1), 5)
            yaxp <- par()$yaxp
            yticks <- round(seq(yaxp[1], yaxp[2], len = yaxp[3] + 1), 5)
            
            if(any(axes %in% c(2, 4)) & !any(axes %in% c(1, 3)))
               abline(h = yticks, lty = 1, lwd = 0, col = grid.col) 
            else  {
               if(!any(axes %in% c(2, 4)) & any(axes %in% c(1, 3)))
                  abline(v = xticks, lty = 1, lwd = 0, col = grid.col) 
               else 
                  abline(v = xticks, h = yticks, lty = 1, lwd = 0, col = grid.col)   
            }      
        }
    }
    
    lat[is.na(lat)] <- latrange[1] + .Machine$double.eps
    long[is.na(long)] <- longrange[1] + .Machine$double.eps
    tf <- (long >= longrange[1] & long <= longrange[2]) & (lat >= latrange[1] & lat <= latrange[2])
    "tf.1 <<- tf"
    "print(c(length(tf), sum(tf)))"
    lat[lat == latrange[1] + .Machine$double.eps] <- NA
    long[long == longrange[1] + .Machine$double.eps] <- NA
    if (sum(tf) > 2) 
        tf.na <- !(is.na(lat[tf]) & c(FALSE, is.na(lat[tf])[1:(length(lat[tf]) - 1)]))
    else tf.na <- NA
    "print(c(length(tf), sum(tf)))"
    "print(sum(tf)/length(tf))"
    
    if(lines.out.of.bounds) {
        if(!is.na(poly))
             polygon(x = long, y = lat, col = poly, border = NA)
         do.call(lines, c(list(x = long, y = lat), lines.dots))    
    } else {
        if(!is.na(poly))
             polygon(x = long[tf][tf.na], y = lat[tf][tf.na], col = poly, border = NA)
        do.call(lines, c(list(x = long[tf][tf.na], y = lat[tf][tf.na]), lines.dots))    
      cat("\n\nNew Edits\n\n")        
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
                z.area <- imap.ll(area, poly = poly, antarctic = antarctic, arctic = arctic, oz = oz, axes = axes, grid = grid, aspect = aspect, zoom = zoom, 
                  lines.out.of.bounds = lines.out.of.bounds, tol = tol, ...)
            }
            else z.area <- imap.ll(area, longrange, latrange, poly = poly, 
                antarctic = antarctic, arctic = arctic, oz = oz, axes = axes, grid = grid, aspect = aspect, zoom = zoom, 
                  lines.out.of.bounds = lines.out.of.bounds, tol = tol, ...)
        }
    }
    invisible(matrix(z.area, ncol = 2))
}




