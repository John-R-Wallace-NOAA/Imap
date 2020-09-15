
imapToRAST <- function (landOverlay = FALSE, ...) 
{
    Imap::imap()
    TMP <- par()$usr
    longrange <- TMP[1:2]
    latrange <- TMP[3:4]
    Imap::plotRAST(latrange = latrange, longrange = longrange, landOverlay = landOverlay,  ...)
    invisible(list(latrange = latrange, longrange = longrange))
}

