daily.depth <- function(track){
  require(date)
  track$jday <- as.numeric(mdy.date(track$month,track$day,track$year))
  zmax <- data.frame(aggregate(x = track$depth, by = list(track$jday), FUN = "max"))
  zmin <- data.frame(aggregate(x = track$depth, by = list(track$jday), FUN = "min"))
  zavg <- data.frame(aggregate(x = track$depth, by = list(track$jday), FUN = "mean"))
  zmed <- data.frame(aggregate(x = track$depth, by = list(track$jday), FUN = "median"))
  jday <- data.frame(date.mdy(zmax[,1]))
  depths <- data.frame(zmax, zmin[,2], zavg[,2], zmed[,2], date.mmddyyyy(zmax[,1]),jday)
  names(depths) <- c("jday", "zmax", "zmin", "zavg", "zmed", "datestamp",names(jday))
  #depths <- depths[,c(6,2:5,1)]
  return(depths)
}

