
imapToGIS <- function (landOverlay = FALSE, ...) 
{
    imap()
    TMP <- par()$usr
    longrange <- TMP[1:2]
    latrange <- TMP[3:4]
    plotGIS(latrange = latrange, longrange = longrange, landOverlay = landOverlay, 
        verbose = verbose, ...)
    invisible(list(latrange = latrange, longrange = longrange))
}
