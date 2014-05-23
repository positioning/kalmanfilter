#=================================================================
# Libraries
#=================================================================
require(GeoLight)
require(fields)
source("C:/R/scripts/actiogram.r")

#=================================================================
# Functions
#=================================================================
{
# Thin time series to the specified sampling interval for actigram 
#-----------------------------------------------------------------
thin.ts <- function(dat, sample.freq){
  #nn = 1
  #while(length(nn) > 0) {
  tt <- with(dat, paste(year,"/",month,"/",day," ", hour, ":", min, ":", "00", sep=""))
  tt <- strptime(tt, "%Y/%m/%d %H:%M:%S")
  tt <- as.POSIXct(tt, tz="UTC")
  dat <- dat[order(tt),]
  tt <- difftime(c(tt[2:length(tt)],0), tt, units="secs")
  dat <- dat[-which(tt < sample.freq),]
  dat <- na.omit(dat)
  tt <- with(dat, paste(year,"/",month,"/",day," ", hour, ":", min, ":", "00", sep=""))
  tt <- strptime(tt, "%Y/%m/%d %H:%M:%S")
  tt <- as.POSIXct(tt, tz="UTC")
  dat <- dat[order(tt),]
  tt <- difftime(c(tt[2:length(tt)],0), tt, units="secs") 
  nn <- which(tt < sample.freq)
  print(nn)
  #}
  return(dat)
}

# Local threshold level
#-------------------------
find.threshold <- function(dat, zlim=NULL, deep.end=NULL, binsize=NULL){ 
  # Recommended values for depth: 0-500 meters, 10 m bin, don't go deeper than 200 m
  if (is.null(zlim)){
     zlim <- range(na.omit(dat))
	 deep.end <- zlim[2] * 0.6
	 binsize <- zlim[2] * 0.02
	 zlim[1] = 0
  }
  hh <- hist(dat, breaks=seq(zlim[1],zlim[2],binsize), plot = FALSE) 
  hh$counts[which(hh$counts==0)]<- NA # remove zero counts
  ii <- which(hh$counts<=quantile(na.omit(hh$counts),probs = seq(0, 1, 0.05))[6]) # 25%
  ti <- hh$breaks[ii]
  ti <- subset(ti, ti <= deep.end)
  ti <- mean(subset(ti, ti > 10))
  return(ti)
}

daily.threshold <- function(look, zlim=NULL, deep.end=NULL, binsize=NULL){
  thz = NULL
  for (j in 1:nrow(look$z)){
    thz = c(thz, find.threshold(look$z[j,], zlim, deep.end, binsize))
  }
  return(thz)
}

# Find times according the threshold of the day
#----------------------------------------------
find.times <- function(dat, xx, threshold, smooth.level=0.01, dif = 0.01, showplot=T, ...){
 sp <- seq(0,48,length.out=2880) 
 lo <- loess(dat~xx, span=smooth.level)
 # Look for the difference between the loess fit and the threshold
 md <- as.vector(abs(predict(lo, sp)-threshold))
 aa <- quantile(na.omit(md),probs = seq(0, 1, dif))[[2]] 
 # %difference between smoothed curve and threshold
 jj <- which(md < aa)
 jk <- which(diff(jj)>10)+1  # successive time points have to be far enough apart
 ss <- c(jj[1],jj[jk]) # should yield 4 points over 48 hours
 fx <- 2880/length(dat)  # rescale the index back to the original sampling freq
 ss <- round(ss/ fx)
 if (showplot){
   plot(dat, ...)
   lines(predict(lo), col=4, lwd=2)
   abline(h=threshold, col=2, lwd=2)
   points(x=ss,y=rep(threshold,length(ss)), pch=20, col="green",cex=1.5)
 }
 # Data for 1-min time series
 dd <- data.frame(sp, predict(lo, sp)); names(dd) <- c("dhr","val")
 return(list(ss=ss,tdat=dd))
}

get.times <- function(mydat, xx, threshold, smooth.level, dif = 0.01, rows=NULL, showplot=T, txt=NULL){
  timess <- data.frame(NA,NA,NA,NA,NA,NA,NA)
  names(timess) <- c("day","n","smooth","t1","t2","t3","t4")
  smoothts <- data.frame(NA,NA,NA)
  names(smoothts) <- c("day","dhr","val")
  if (is.null(rows)) {rows <- 1:nrow(mydat)}
  for (j in rows){
    if (!showplot) print(j)  
    dat <- mydat[j,]
    tss <- find.times(dat, xx, threshold[j], smooth.level, dif, showplot, main = ifelse(is.null(txt),j,txt[j]))
	tdat <- data.frame(j, tss$tdat); names(tdat) <- c("day","dhr","val")
	smoothts <- rbind(smoothts, tdat)
	tss <- tss$ss
	pad <- -888
    if (length(tss)<4) pad <- rep(-999,4-length(tss))
    tss2 <- c(j, length(tss), smooth.level, tss)
	if (pad==-999) tss2 <- c(tss2, pad)
	tss2[which(tss2==-999)] <- NA
    timess <- rbind(timess,tss2)
    Sys.sleep(2)
  }
  return(list(tss=timess[-1,],smts=smoothts[-1,]))
}

smooth.times <- function(myt, smooth.level=0, delta=0.05){
  ki <- which(myt$n>4)
  while (length(ki)>0)
  {
   smooth.level = smooth.level + delta
   ki <- which(myt$n>4)
   if (length(ki)==0) break
   kj <- myt$day[ki]
   print(paste("Working on smoothing level: ", smooth.level))
   myt2 <- get.times(look$z[kj,],a1$y,thz[kj],smooth.level,showplot=F,txt = kj)
   myt2$day <- kj
   myt[ki,] <- myt2
  }
  return(myt)
}

# Handle special cases with mixed-up order, 3 or 2 timestamps
#------------------------------------------------------------
shift.times <- function(myt){
  # Look at the whole block, anything from 2-4 timestamps
  ii <- c(abs(diff(myt$t1)),0)
  ii <- which(ii > max(ii) * 0.75) # different from the rest
  ia <- ii[seq(1,length(ii),2)]+1
  ib <- ii[seq(2,length(ii),2)]
  # special case when the indices can't pair up
  if (length(ib)<length(ia)) ib <- c(ib, nrow(myt))
  for (ij in 1:length(ia)) {myt[ia[ij]:ib[ij],] <- myt[ia[ij]:ib[ij],c(1:3,5:7,4)]}
  # Only 3/2 timestamps
  ka <- c(which(myt$n==2),which(myt$n==3))
  if (length(ka)>0) {
    qt <- quantile(myt$t1)[3] + 120 # in minutes
    ia <- intersect(ka, which(myt$t1 > qt))
	myt[ia,5:7] <- myt[ia,4:6]
	myt[ia,4] <- NA
  }
  return(myt)
}

# Turn decimal hours back to dates
#---------------------------------
get.dates <- function(dates,dhr){
  ss <- date.mdy(dates)
  ss <- with(ss, paste(year,"/",month,"/",day, sep=""))
  ss <- as.POSIXct(strptime(ss, "%Y/%m/%d"), tz="UTC") + dhr*60*60 # this addition is in seconds
  return(ss)
}

# Generate sunrise sunset times
#--------------------------------------------------
sun.times <- function(act,look, myt, type=c(1,2)){
  ihr <-  ncol(look$z)/48 # Re-scale to decimal hours
  myt[,4:7] <- myt[,4:7]/ ihr
  xx = act$x[1]
  lx = pretty(act$x)[1]*1.002
  with(act,image.plot(x=x,y=y,z=z, col=terrain.colors(128))); box()
  with(myt, lines(day+xx, t1, lwd=2))
  text(lx, myt$t1[1]+1, paste("Time 1", ifelse(type[1]==1,"Sunrise","Sunset")), cex = .8)
  with(myt, lines(day+xx, t2, lwd=2))
  text(lx, myt$t2[1]+1, paste("Time 2", ifelse(type[2]==1,"Sunrise","Sunset")), cex = .8)
  with(myt, lines(day+xx, t3, lwd=2))
  text(lx, myt$t3[1]+1, paste("Time 3", ifelse(type[1]==1,"Sunrise","Sunset")), cex = .8)
  with(myt, lines(day+xx, t4, lwd=2))
  text(lx, myt$t4[1]+1, paste("Time 4", ifelse(type[2]==1,"Sunrise","Sunset")), cex = .8)
  
  # Average out the 4 times into 2 times, i.e. times of sunrise and sunset
  Sys.setenv(TZ='UTC')
  s1 <- sort(c(get.dates(act$x,myt$t1),get.dates(act$x,myt$t3)))
  s2 <- sort(c(get.dates(act$x,myt$t2),get.dates(act$x,myt$t4)))
  s1 <- data.frame(substring(as.character(s1),1,10),s1); names(s1) <- c("date","ds")
  s2 <- data.frame(substring(as.character(s2),1,10),s2); names(s2) <- c("date","ds")
  ss1 <- aggregate(s1$ds, by = list(s1$date), FUN="mean")$x
  ss2 <- aggregate(s2$ds, by = list(s2$date), FUN="mean")$x

  # Have to manually decide whether time series is sunrise times (1) or sunset times (2)
  ss1 <- data.frame(type[1],ss1); names(ss1) <- c("type","date")
  ss2 <- data.frame(type[2],ss2); names(ss2) <- c("type","date")
  tss <- rbind(ss2,ss1)
  tss <- tss[order(tss$date),]
  tss <- with(tss,data.frame(as.numeric(substring(date,1,4)),as.numeric(substring(date,6,7)),as.numeric(substring(date,9,10)),type,date))
  names(tss) <- c("year","month","day","type","date")
  return(tss)
}

# Threshold method with GeoLight
#--------------------------------------------------
threshold.method <- function(tss,start.xy=NULL,ee=-6){
  idat <- data.frame(tss$date, 1, tss$type)
  names(idat) <- c("tFirst","tSecond","type")
  idat$tSecond <- c(idat$tFirst[-1],NA)
  nn <- nrow(idat)-2
  idat <- idat[1:nn,]
  if (!is.null(start.xy)) ee <- with(idat,getElevation(tFirst,tSecond,type,start.xy))
  xy <- with(idat, coord(tFirst,tSecond,type,ee))
  lx = range(na.omit(xy[,1])) 
  ly = range(na.omit(xy[,2]))
  lx[1] = lx[1] - diff(lx)*0.5
  lx[2] = lx[2] + diff(lx)*0.5
  tripMap(xy, xlim=lx, ylim=ly)
  return(xy)
}

# Format input for KF suite
#----------------------------------------------------------------
make.trackfile <- function(tss, xy, start.xy, end.xy, lon.offset=360){
  nn <- nrow(tss)-2
  track <- cbind(tss[seq(1,nn,2),c(3,2,1)],xy[seq(1,nn,2),])
  names(track)[c(4,5)] <- c("lon","lat")
  track$lon <- track$lon+lon.offset
  track[1,c(4,5)] <- start.xy
  track <- rbind(track,track[nrow(track),])
  track[nrow(track),c(4,5)] <- end.xy
  return(track)
}

}

#=================================================================
# Data 
#=================================================================

## Example 1
#- - - - - - - 
# Borrowing the mooring dataset from "trackit" package
data(mooring, package='trackit')
dat = mooring

# Specify sampling frequency in seconds and the variable to use
sample.freq = 60
zvar="light"
# Start and end points, 0-360 lon! 
start.xy = c(166.47,-22.48)
end.xy = c(166.47,-22.48)

## Example 2
#- - - - - - -
# mdat <- read.csv(url('http://www.soest.hawaii.edu/pfrp/elec.tagdata/224_20.csv'))
# mdat <- read.csv(url('http://www.soest.hawaii.edu/pfrp/elec.tagdata/219_40.csv'))
# mdat <- read.csv(url('http://www.soest.hawaii.edu/pfrp/elec.tagdata/218_60.csv'))
# names(mdat) <- tolower(names(mdat))
# dat <- with(mdat, data.frame(1998, as.numeric(substring(date,1,2)),as.numeric(substring(date,4,5)),
                  # as.numeric(substring(time,1,2)),as.numeric(substring(time,4,5)),0, 
				  # depth, light.level, external.temperature))
# names(dat) <- c("year","month","day","hour","min","sec","depth","light","temp")
# dat$year[min(which(mdat$date=='01/01')):nrow(dat)] <- 1999
# dat <- na.omit(dat)
# dat$depth[which(dat$depth<0)] <- 0

# # source("C:\\R\\scripts\\sorttrack.R")
# # dat <- sort.track(dat)
# # dat <- dat[16000:310000,]

# # dat$ds <- as.POSIXct(strptime(with(dat, paste(year,"/",month,"/",day," ", hour, ":", min, ":", "00", sep="")), "%Y/%m/%d %H:%M:%S"), tz="UTC")
# # gdat <-  with(dat, twilightCalc(ds,light,ask=F))

# Specify sampling frequency in seconds and the variable to use
sample.freq = 60
zvar="light"
# Start and end points, 0-360 lon! 
start.xy = c(360-166.7,24)
end.xy = start.xy

## Example 3
#- - - - - - -
data(hoopoe1)
hoopoe1$ds <- as.character(hoopoe1$datetime)
dat <- with(hoopoe1, data.frame(as.numeric(substring(ds,1,4)), as.numeric(substring(ds,6,7)), as.numeric(substring(ds,9,10)), as.numeric(substring(ds,12,13)),as.numeric(substring(ds,15,16)),0, 0, light,0))
names(dat) <- c('year','month','day','hour','min','sec','depth','light','temp')
sample.freq = 600
zvar="light"

xy <- with(hoopoe2, coord(tFirst,tSecond,type,degElevation=-5.95))
tripMap(coord,xlim=c(-20,20),ylim=c(5,50))
hoopoe2$ds <- as.character(hoopoe2$tFirst)
track <- with(hoopoe2, data.frame(as.numeric(substring(ds,1,4)), as.numeric(substring(ds,6,7)), as.numeric(substring(ds,9,10)), as.numeric(substring(ds,12,13)),as.numeric(substring(ds,15,16)),0, xy[,1],xy[,2]))
names(track) <- c('year','month','day','hour','min','sec','lon','lat')
# Thin track
track <- na.omit(track[seq(1,nrow(track),2),])
# Shift track lon by 180 for kf stuff to work
track$lon <- track$lon+180
kf <- kftrack(track[,c(3,2,1,7,8)])
points(kf$most.prob.track[,1]-180,kf$most.prob.track[,2], pch=20, col="green")

## Example 4
# GTOPP tuna data
#- - - - - - - - -
library(trackit)
source("C:\\R\\scripts\\trimit.r")
dat <- read.csv("P:\\PUBLIC\\TOPP\\5103508LO01A1016DC.csv")
track <- data.frame(dat[,c(8:13,3,4,6)])
names(track) <- c('year','month','day','hour','min','sec','depth','light','temp')
track <- two.layer.depth.corr(track)

start.xy = c(-75.01227,34.848465)
end.xy = c(13.31,40.74)
sample.freq = 120
zvar="light"

scroll(track)
prep.track<-prepit(track[seq(1,nrow(track),4),], 
                   fix.first=c(360-75.01227,34.848465,2003,1,20,12,0,0),
                   fix.last=c(15.93495,37.5197,2004,10,30,0,0,0), scan=T)
#pt2 <- .trimit(prep.track)
fit <- trackit(prep.track)

#=================================================================
# Workflow
#=================================================================

# 1. Build an actigram
a1 <- actigram(dat,"dat", sample.freq, mytitle="", mylabel="", zvar=zvar, saveplot=F)

# 2. Smooth an actigram
# dx or dy, the smaller the number, the larger the smoothing; while for theta, larger = more smoothing
look <- image.smooth(a1$z, theta=2.5, dy=200, dx = 0.5)
# Other tricks - run two.layer.depth.corr from trackit

# For hoopoe
look <- image.smooth(a1$z, theta=2.5, dy=10, dx = 1)
thz <- daily.threshold(look)
ii <- which(is.nan(thz)==T)
thz[ii] <- mean(thz[-ii]) # Fill gaps
myt <- get.times(mydat=look$z,xx=a1$y,threshold=thz,smooth.level=0.01,showplot=T)
myt0 <- myt
myt <- myt$tss
tss <- sun.times(a1, look, myt, type=c(1,2))
xy <- threshold.method(tss,ee=-5.95) # elevation angle from paper/ calibrated known pos.

# For blocker
thz = NULL
for (j in 1:nrow(look$z)){
  #print(j)
  thz = c(thz, find.threshold(look$z[j,], zlim, deep.end, binsize))
}
myt <- get.times(mydat=look$z,xx=a1$y,threshold=thz,smooth.level=0.05,showplot=T)
myt[which(myt$t1>300),]=myt[which(myt$t1>300),c(1:3,7,4:6)]
tss <- sun.times(a1, look, myt, type=c(1,2))
xy <- threshold.method(tss)
xy1 <- xy; xy1[,1] <- xy1[,1]-5*15
xy1 = xy1[1:1327,] 
tripMap(xy1,xlim=c(-100,20),ylim=c(10,60))
points(start.xy[1], start.xy[2], pch=25, cex=2, bg="green")
points(end.xy[1], end.xy[2], pch=24, cex=2, bg="red")
lof = 200
loc = 230 # max(track$lon)
start.xy = c(-75.01227+lof,34.848465)
end.xy = c(13.31+lof,40.74)
track <- make.trackfile(tss[1:nrow(xy1),], xy1, start.xy, end.xy, lon.offset=lof)

source("C:\\R\\scripts\\get-reynolds.R")
sst.path<-get.reynolds(track[,c(3,2,1)],0,360,18,50,minus180=T,offsetx=lof,offset.cutoff=loc)

source("C:\\R\\scripts\\dailysst.R")
dat2 <- data.frame(dat[,c(8:13,3,4,6)])
names(dat2) <- c('year','month','day','hour','min','sec','depth','light','temp')
sst <- daily.sst(dat2, 10)
track$sst <- NA
track$ds <- with(track, paste(year,month,day,sep="-"))
sst$ds <- with(sst, paste(year,month,day,sep="-"))
track$sst <- sst$sst[match(track$ds,sst$ds)]

track$lat[which(track$lat < -20)] <- NA
track$lat[which(track$lat > 50)] <- NA
track$count <- as.numeric(is.na(track$lon)) + as.numeric(is.na(track$lat)) + as.numeric(is.na(track$sst))
track <- track[-which(track$count > 1), 1:6]
# take out a row suspicious lon
track <- track[-which(track$lon<120),]

library(kftrack)
kf <- kftrack(na.omit(track[,1:5]), bx.a=F)
library(ukfsst)
source("C:\\R\\scripts\\guess.R")
ii <- guess(track[1:6] , 200)
ii[3] <- 1200
kk = as.vector(kf$est) 
ii = c(kk[c(1:5)],0,kk[c(6:7)],1,kk[c(8:9)],300)
fit<-kfsst(track, theta.init=ii, theta.active=c(1,1,1,0,0,0,1,1,1,1,1,0))

library(sp)
sxy = SpatialPoints(cbind(fit$most.prob.track[,1]-lof,fit$most.prob.track[,2]))
tripMap(sxy,xlim=c(-100,20),ylim=c(10,60))
start.xy = c(-75.01227,34.848465)
end.xy = c(13.31,40.74)
points(start.xy[1], start.xy[2], pch=25, cex=2, bg="green")
points(end.xy[1], end.xy[2], pch=24, cex=2, bg="red")

# Optional. Check smoothing
# x11(9,6); par(mfrow=c(1,2),mar=c(3,3,1,1))
# with(a1,image.plot(x=x,y=y,z=z, col=terrain.colors(128))); box()
# image.plot(x=a1$x,y=a1$y,z=look$z, col=terrain.colors(128)); box()

# 3. Generate daily threshold
thz <- daily.threshold(look)

# 4. Obtain hour stamps when the threshold has been crossed
# Do the first pass using the least amount of smoothing (0.01).
# Smoothing is used to reduce the influence of sudden dives/ changes
myt <- get.times(mydat=look$z,xx=a1$y,threshold=thz,smooth.level=0.01,showplot=T)

# 5. Smooth until at have <=4 timestamps
myt0 <- myt
myt <- myt$tss
myt <- smooth.times(myt)

# 5b. Optional. Check the timepoints
# par(mfrow=c(2,2)); plot(myt$t1); plot(myt$t2); plot(myt$t3); plot(myt$t4)
# 5c. Try shift mis-ordered timepoints
# myt <- shift.times(myt)

# 6. Get the sunrise/ sunset times
# Check the "type" for sunrise (1) and sunset (2) 
tss <- sun.times(a1, look, myt, type=c(2,1))

# 7. Good old threshold method with GeoLight
xy <- threshold.method(tss) # degElevation = -8.6 for Example 2
# 7a. Show the mooring location
points(start.xy[1], start.xy[2], pch=25, cex=2, bg="green")

# 8. Construct track for input for kftrack
track <- make.trackfile(tss, xy, start.xy, end.xy, lon.offset=0)

#9. Run a simple fit
require(kftrack)
kf <- kftrack(na.omit(track))
points(kf$most.prob.track, pch=20, col="purple")
# 9a. Run again without outliers
track$lat[which(track$lat > -20)] <- NA
kf <- kftrack(na.omit(track))
points(kf$most.prob.track, pch=20, col="hotpink")
