imapToGIS <- function(imap = FALSE, ...) {

  imap()
  TMP <- par()$usr
  
  longrange <- TMP[1:2]
  latrange <- TMP[3:4]
  
  plotGIS(latrange = latrange, longrange = longrange, imap = imap, ...)
  invisible(list(latrange = latrange, longrange = longrange))
}
