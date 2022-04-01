adjustPolygon <- function (xy, colLine = "red", colPoly = "purple", alpha = 0.25, borderPoly = 'black', add = FALSE, 
                           closePoly = TRUE, lty = "solid", colBg = par()$bg, ...)  {
    
    X <- xy[,1]
    Y <- xy[,2]
    
    if(!add) 
        plot(X, Y, xlab = ifelse(is.null(colnames(xy)[1]), "", colnames(xy)[1]), ylab = ifelse(is.null(colnames(xy)[2]), "", colnames(xy)[2]), 
             type = "n", xaxt = "n", yaxt = "n", bty = "n")
    
    lines(X, Y, col = colLine, lty = lty, type = "o", ...)
    
    while (length(PtRow <- identify(X, Y, labels = "", n = 1)) == 1) {
        newPt <- locator(1)
        lines(X, Y, lty = lty, col = ifelse(colBg == "transparent", "white", colBg), type = "o", ...)
        X[PtRow] <- newPt$x
        Y[PtRow] <- newPt$y
        lines(X, Y, lty = lty, col = colLine, type = "o", ...)
    }
    
    lines(X, Y, lty = lty, col = ifelse(colBg == "transparent", "white", colBg), type = "o", ...)
    if (closePoly) {
        X[length(X)] <- X[1]
        Y[length(Y)] <- Y[1]
    }
    
    polygon(X, Y, col = Imap::col.alpha(colPoly, alpha), border = borderPoly, lty = lty, ...)
    
    out <- cbind(X, Y)
    colnames(out) <- colnames(xy)
    if(is.data.frame(xy))
         out <- data.frame(out)
    invisible(out)
}
