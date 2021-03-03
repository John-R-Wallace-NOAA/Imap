Git.World.f.HiRez <- function() {

   usr.warn <- options(warn = -1)
   on.exit(options(usr.warn))   
     
   download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/GSHHG_High_Rez_for_R/master/world.f.land.A.RData", "world.f.land.A.RData")
   download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/GSHHG_High_Rez_for_R/master/world.f.land.B.RData", "world.f.land.B.RData")
   download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/GSHHG_High_Rez_for_R/master/world.f.borders.lakes.rivers.RData", "world.f.borders.lakes.rivers.RData")
   
   base::load("world.f.land.A.RData")
   base::load("world.f.land.B.RData")
   base::load("world.f.borders.lakes.rivers.RData", .GlobalEnv)

  
   assign("world.f.land", rbind(world.f.land.A, world.f.land.B), pos = 1)
   remove(world.f.land.A, world.f.land.B, pos = 1)
   
   file.remove("world.f.land.A.RData", "world.f.land.B.RData", "world.f.borders.lakes.rivers.RData")

   base::ls(pattern = 'world', pos = 1)
}


