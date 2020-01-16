# Imap
Interactive mapping in R

Install or upgrade with:

    if (!require('devtools')) install.packages('devtools')  # Get devtools if you don't already have it.
    
    oldOpts <- options(download.file.method = "auto")  # Sometimes remotes::install_github() throws an error without this
    remotes::install_github("John-R-Wallace-NOAA/Imap", force = TRUE)
    options(oldOpts)
```diff 
- After upgrading, restarting R may be needed before the help pages will work (the Imap.rdb is not corrupt).
```

