
updateImap <- function (quiet = FALSE, ...) 
{
    if(length(grep('Imap', search())) != 0)
        detach(pos = grep('Imap', search()))
    JRWToolBox::lib("John-R-Wallace-NOAA/Imap", quiet = quiet, ...)
}

