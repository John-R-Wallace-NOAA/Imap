imapToGIS <- function(landOverlay = FALSE, verbose = TRUE, ...) {

  imap()
  TMP <- par()$usr
  
  longrange <- TMP[1:2]
  latrange <- TMP[3:4]
  
  plotGIS(latrange = latrange, longrange = longrange, landOverlay = landOverlay, verbose = verbose, ...)
  invisible(list(latrange = latrange, longrange = longrange))
}
