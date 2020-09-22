# Imap
Interactive mapping in R

Install or upgrade with:

    if (!require('devtools')) install.packages('devtools')  # Get devtools if you don't already have it.
    
    remotes::install_github("John-R-Wallace-NOAA/Imap", force = TRUE)
    
    # # Some R installations may require: download.file.method = "auto" in options():
    # oldOpts <- options(download.file.method = "auto")  
    # remotes::install_github("John-R-Wallace-NOAA/Imap", force = TRUE)
    # options(oldOpts)
    
```diff 
- After upgrading this package, restarting R may be needed before the help pages will work (the Imap.rdb is not corrupt).
```

