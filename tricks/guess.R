guess <- function(track, rad){
  z <- function(a1, a2){
    lon <- -a1/(60*cos(a2/60*pi/180))+lon[1]
    lat <- a2/60
    return(cbind(lon,lat))
  }

  zi <- function(lon, lat){
    a1 <- -(lon-lon[1])*60*cos(lat*pi/180)
    a2 <- lat*60
    return(cbind(a1,a2))
  }
  ndsol.one<-function(date){
    mdy<-date.mdy(date)
    day<-mdy$day
    month<-mdy$month
    year<-mdy$year
    if(date<mdy.date(month=6, day=21, year=year)){
      ret<-date-mdy.date(month=12, day=21, year=year-1)
    }else{
      if(date<mdy.date(month=12, day=21, year=year)){
        ret<-date-mdy.date(month=6, day=21, year=year)
      }else{
        ret<-date-mdy.date(month=12, day=22, year=year)
      }
    }
    return(ret)
  }  
  ndsol<-function(date){
    sapply(1:length(date), function(i)ndsol.one(date[i]))
  }

  track[track=="NaN"] <- NA
  track <- na.omit(track)
  lon <- track[,4]
  lat <- track[,5]
  sst <- track[,6]
  a1a2 <- zi(lon,lat)
  a1 <- a1a2[,1]
  a2 <- a1a2[,2]
  date <- mdy.date(day=track[,1], month=track[,2], year=track[,3]) 
  dayAL <- date-date[1]
  n <- nrow(track)

  u <- (a1[n]-a1[1])/dayAL[n]
  v <- (a2[n]-a2[1])/dayAL[n]

  a1nd <- a1-u*dayAL
  a2nd <- a2-v*dayAL

  fita1nd <- locfit(a1nd~dayAL, weights=c(1.0e6, rep(1,n-2), 1.0e6)) 
  fita2nd <- locfit(a2nd~dayAL, weights=c(1.0e6, rep(1,n-2), 1.0e6)) 

  preda1nd <- predict(fita1nd, newdata=dayAL)
  preda2nd <- predict(fita2nd, newdata=dayAL)
  
  D <- 1/(4*(n-2))*(sum(diff(preda1nd)^2/diff(dayAL))-
       (sum(diff(preda1nd))^2/sum(diff(dayAL)))+
       sum(diff(preda2nd)^2/diff(dayAL))-
       (sum(diff(preda2nd))^2/sum(diff(dayAL))))

  predlonlat <- z(preda1nd+u*dayAL, preda2nd+v*dayAL)
  predlon <- predlonlat[,1]
  predlat <- predlonlat[,2]

  sx <- sd((lon-predlon)[-c(1,n)])

  #y<-cos(2*pi*ndsol(date)/365.25)^2
  sy <- sd((lat-predlat)[-c(1,n)])
  b0<-0
  a0<-1

  fitsst <- locfit(sst~dayAL) 
  predsst <- predict(fitsst, newdata=dayAL)
  ssst <- sd(sst-predsst)  

  ret<-c(u, v, D, 0, 0, 0, sx, sy, ssst, a0, b0, rad) 

  ret
  }							
