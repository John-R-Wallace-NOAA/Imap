

sunRiseSet <- function(Dates.Locs, timezone="America/Los_Angeles") {

'  # This function is a wrapper so that vectors of date and lat/long can be given  '
'  # Dates.Locs needs to be a data frame with column names of: Date, Lat, and Long '
'  # sunrise.set() is from:  http://r.789695.n4.nabble.com/maptools-sunrise-sunset-function-td874148.html  '
'  # Example: '
'  #    (Dates.Locs <- data.frame(Date = c("2008-11-08", "2017-03-24"), Lat = c(33.46, 47 + 38/60 + 40/3600), Long = c(-84.25, -(122 + 18/60 + 24/3600))))  '
'  #    sunRiseSet(Dates.Locs)  '
'  #    cbind(Dates.Locs, sunRiseSet(Dates.Locs)) '
'  '
'  # Locations and Dates need to be all in the same time zone for now.... '
'  '

    sunrise.set <- function(lat, long, date, timezone="UTC", num.days=1) {
        require(maptools)
        #this needs to be long lat#
        lat.long <- matrix(c(long, lat), nrow=1)
        day <- as.POSIXct(date, tz=timezone)
        sequence <- seq(from=day, length.out=num.days , by="days")
        sunrise <- sunriset(lat.long, sequence, direction="sunrise", POSIXct=TRUE)
        sunset <- sunriset(lat.long, sequence, direction="sunset", POSIXct=TRUE)
        ss <- data.frame(sunrise, sunset)
        ss <- ss[,-c(1,3)]
        colnames(ss)<-c("sunRise", "sunSet")
        ss[1] <- convert.hr.min.sec.to.decimal.hrs(substring(strftime(ss[1,1]), 12))
        ss[2] <- convert.hr.min.sec.to.decimal.hrs(substring(strftime(ss[1,2]), 12))
        ss
    }

   renum <- function (x, no.num = F) {
       if (no.num) 
          dimnames(x)[[1]] <- rep("", nrow(x))
       else dimnames(x)[[1]] <- 1:nrow(x)
       x
   }

   convert.hr.min.sec.to.decimal.hrs <- function (hr.min.sec) 
   {
      if (hr.min.sec %in% "<NA>") 
        return(NA)
      tmp <- as.numeric(strsplit(hr.min.sec, split = ":")[[1]])
      tmp[1] + tmp[2]/60 + tmp[3]/3600
   }


# --------------------------------

  N <- nrow(Dates.Locs)

  Out <- NULL
  for ( i in 1:N) {

         Out <- rbind(Out, sunrise.set(Dates.Locs$Lat[i], Dates.Locs$Long[i], Dates.Locs$Date[i], timezone=timezone))
  }

  renum(Out)

}


        
