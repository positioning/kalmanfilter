.First.lib <- function (lib, pkg){
  cat("\n")
  cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
  cat("Welcome to trackit\n")
  if (.Platform$OS.type == "windows") {
      cat("\n")
      cat("WINDOWS USERS: Please press (Ctrl+W) or use the menu \n")
      cat("(Misc->Buffered output) to switch the delayed output off.\n")
  }
  cat("\n")
  cat(paste(rep("=", options()$width), collapse = ""), "\n\n")

  thisenv <- as.environment(which(search()=="package:trackit"))
  data(angle2light, envir=thisenv)
  data(deltat, envir=thisenv)
  data(gmt3, envir=thisenv)
  local({dmy<-deltat[,c('day','month','year')]
         JDE.matrix<-function(dmy)JDE(year=dmy[3],month=dmy[2],day=dmy[1])
         deltat$JDE<-apply(dmy,1,JDE.matrix)
         rm(dmy,JDE.matrix)}, envir=thisenv)
}

# Julian Ephemeris Day, Chapter 7 (uniform/dynamical time scale)
# vectorized version 
JDE<-function(year, month, day){
  idx<-month<=2
  year[idx]<-year[idx]-1
  month[idx]<-month[idx]+12
  A <- year%/%100
  julian<-(year<1582)|((year==1582)&(month<10))|((year==1582)&(month==10)&(day<=4)) 
  B<-ifelse(julian,0.0,2-A+A%/%4) 
  JD <- trunc(365.25*(year+4716)) + trunc(30.6001*(month+1)) + day + B - 1524.5
  return(JD)
}

approxDeltaT<-function(jde, table){
  if(min(jde)<table$JDE[1])stop("Table of DT-UT starts too late")
  if(max(jde)>table$JDE[nrow(table)])stop("Table of DT-UT ends too early ")
  return(approx(table$JDE,table$DTminusUT, jde)$y/86400)#there is 86400s/day 
}

JD <- function(year, month, day, type=c("dynamical", "universal")){
  ret<-list()
  ret$dynamical<-JDE(year=year, month=month, day=day)
  dt <- approxDeltaT(ret$dynamical, deltat)
  ret$universal<-ret$dynamical-dt  
  return(ret[[type]])
}

deg.to.rad<-function(d)
{
  pi.180<-pi/180.0
  return(d*pi.180)
}

rad.to.deg<-function(r)
{
  pi.180<-pi/180.0
  r.pi.180<-1.0/pi.180
  return(r.pi.180*r)
}

solar.coordinates<-function(year, month, day, hour=0, min=0, sec=0)
{
  jde <- JD(year,month,day+hour/24+min/1440+sec/86400, "dynamical")
  solar.coordinates.local(jde)
}

solar.coordinates.local<-function(jde)
{
  T <- (jde-2451545.0)/36525 # (25.1)
  L0 <- 280.46646 + 36000.76983*T + 0.0003032*T*T # (25.2)
  L0<-L0%%360
  M <- 357.52911 + T*(35999.05029 - T*0.0001537) # (25.3)
  e <- 0.016708634 - T*(0.000042037 + T*0.0000001267) # (25.4)
  M<-deg.to.rad(M%%360)
  C <- (1.914602 - T*(0.004817 + T*0.000014))*sin(M) +
       (0.019993 - 0.000101*T)*sin(2*M) +
       0.000289*sin(3*M)
  STL = L0+C
  ob<-obliquity(T)
  lambda <- STL - 0.00569 - 0.00478*sin(deg.to.rad(ob$Omega)) # (25.8)
  lambda<-deg.to.rad(lambda%%360)
  STL<-deg.to.rad(STL%%360)
  eps<-deg.to.rad(ob$eps.app%%360)
  tan.alpha <- cos(eps)*sin(lambda)/cos(lambda) # (25.6)
  sin.delta <- sin(eps)*sin(lambda)             # (25.7)
  alpha <- 360*atan2(cos(eps)*sin(lambda),cos(lambda))/(2*pi)
  delta <- 360*asin(sin.delta)/(2*pi)
  alpha.app <- alpha%%360
  delta.app <- delta
  return(list(alpha.app=alpha.app,delta.app=delta.app))
}
  
obliquity<-function(T)
{
  c1 <- 23.43929  #(23+(26/60)+(21.448/3600))
  c2 <- -46.8150
  c3 <- -0.00059
  c4 <- 0.001813
  eps0 <- c1 + T*(c2 + T*(c3 + T*c4))/3600 # (22.2)
  LSun <- 288.4665 + 36000.7698*T
  LMoon <- 218.3165 + 481267.8813*T
  Omega <- 125.04452 - 1934.136261*T + 0.0020708*T*T + T*T*T/450000
  Omega<-deg.to.rad(Omega%%360)
  LSun<-deg.to.rad(LSun%%360)
  LMoon<-deg.to.rad(LMoon%%360)
  # these are accurate to 0.5 sec
  delta.psi <- (-17.2*sin(Omega) - 1.32*sin(2*LSun) - 0.23*sin(2*LMoon) +0.21*sin(2*Omega))/3600
  delta.eps <- (9.2*cos(Omega) + 0.57*cos(2*LSun) + 0.1*cos(2*LMoon) - 0.09*cos(2*Omega))/3600
  eps <- eps0 + delta.eps 
  eps.app <- eps + 0.00256*cos(Omega)#this line corrects to apparent position (p.165)
  return(list(eps=eps, eps.app=eps.app ,eps0=eps0,delta.psi=delta.psi,delta.eps=delta.eps,
              Omega=rad.to.deg(Omega)))
}

# sidereal time at Greenwich in degrees
sidereal.UT<-function(year, month, day, hour=0, min=0, sec=0)
{
  T <- (JD(year,month,day, "universal")-2451545.0)/36525 # (12.1)
  #TT <- 100.46061837 + T*(36000.770053608 + T*(0.000387933 -T/38710000))
  theta0<-280.46061837+
          360.98564736629*(JD(year,month,day+hour/24+min/1440+sec/86400, "universal")-
                           2451545.0)+0.000387933*T*T-T*T*T/38710000
  return(theta0%%360)
}

solar.altitude<-function(lon, lat, year, month, day, hour=0, min=0, sec=0){
  lon<-(360-lon)%%360
  solar.coord<-solar.coordinates(year=year, month=month, day=day, hour=hour, min=min, sec=sec)
  H<-sidereal.UT(year=year, month=month, day=day, hour=hour, min=min, sec=sec)-lon-
     solar.coord$alpha.app
  sinh<-sin(deg.to.rad(lat))*sin(deg.to.rad(solar.coord$delta.app))+
        cos(deg.to.rad(lat))*cos(deg.to.rad(solar.coord$delta.app))*cos(deg.to.rad(H))
  return(asin(sinh)/(2*pi)*360)
}

.sys<-function (cmd){
  if (.Platform$OS.type == "windows") {
    shell(cmd, invisible=TRUE)
  } else {
    system(cmd)
  }
}

light.simulator<-function(u=0, v=0, D=100, ss1=80, ss2=15, ss3=2.5, rho=0.05, bsst=0, sssst=0.5, rad=80,
              sundata=angle2light, sstdates=NULL,  
              fix.first=c(166.5,-22.5, 2003, 1, 1), t=seq(0,365,by=1/300),  
              window=c(.05,0.01), tmpfile='input.dat', datfile='ukf.dat', truefile='truth.dat', 
              keepfiles=c(tmp=FALSE, dat=TRUE, true=FALSE), localsstfolder=NULL, 
              sst.fun=function(x,...)get.sst.from.server(x,...), from.ystr=c(3,6), from.dstr=c(7,9), 
              to.ystr=c(11,14), to.dstr=c(15,17)){
  oldDigits<-options()$digits; options(digits=22)
  fix.first[1]<-(360-fix.first[1])%%360 # Now it is west pos 
  ret<-list()
  ret$fix.first<-fix.first
  ret$fix.first[1]<-360-ret$fix.first[1]
  ret$window<-window

  addCorrNoise<-function(filen=datfile){
    xx<-readLines(filen)
    getI<-function(str)as.integer(xx[grep(str,xx)+1])
    getIVec<-function(str){
      xxx<-as.integer(strsplit(xx[grep(str,xx)+1], " ")[[1]])
      xxx[!is.na(xxx)]
    }
    ncol<-getI('ncol')
    nrow<-getI('nrow')
    nstep<-getI('nstep')
    idx1<-getIVec('idx1')
    idx2<-getIVec('idx2')
    datIdx1<-grep('obsMat',xx)+1
    datIdx2<-datIdx1+nrow-1
    dat<-matrix(as.numeric(unlist(strsplit(xx[datIdx1:datIdx2],' '))), nrow=nrow, byrow=TRUE)[,-1] 
    time<-dat[,2]
    light<-dat[,10]
    
    noise<-function(idx){
      tt<-time[idx1[idx]:idx2[idx]]
      n<-length(tt)
      S<-matrix(ss1,nrow=n,ncol=n)
      S<-S+outer(tt,tt,function(x,y)ss2*exp(-abs(x-y)/rho))
      mvrnorm(mu=rep(0,nrow(S)), Sigma=S)
    }

    light<-light+as.vector(unlist(sapply(1:nstep, noise)))
    dat[,10]<-light
    
    header<-xx[1:(datIdx1-1)]
    writeLines(header,con=filen)  
    write.table(dat, row.names=FALSE, col.names=FALSE, file=filen, append=TRUE)
    tail<-xx[(datIdx2+1):length(xx)]
    cat(tail,sep='\n',file=filen, append=TRUE)  
    if(!keepfiles['tmp']){
      unlink(tmpfile)
    }else{
      ret$tmpfile<<-file.path(tmpfile)
    }
    if(!keepfiles['dat']){
      unlink(datfile)
    }else{
      ret$postfile<<-file.path(datfile)
    }
  }  
  

  Mpg<-60
  PiDiv180<-pi/180

  z<-function(a1,a2){
    t1<-a2/Mpg*PiDiv180
    t2<-Mpg*cos(t1)
    ret<-cbind(-a1/t2+fix.first[1],a2/Mpg)
    return(ret)
  } 

  zInv<-function(y){
    ret<-c(-(y[1]-fix.first[1])*Mpg*cos(y[2]*PiDiv180),y[2]*Mpg)
    return(ret)
  }

  dt<-diff(t)
  aFirst<-zInv(fix.first)
  a1<-cumsum(c(aFirst[1],rnorm(dt,mean=(-u)*dt,sd=sqrt(2*D*dt))))
  a2<-cumsum(c(aFirst[2],rnorm(dt,mean=v*dt,sd=sqrt(2*D*dt))))
  lonlat<-z(a1,a2)
  lonlatE<-cbind((360-lonlat[,1])%%360,lonlat[,2])  
  ret$fix.last<-lonlatE[nrow(lonlatE),]
  ret$lonlat<-lonlatE

  dateFirst<-mdy.date(day=fix.first[5], month=fix.first[4], year=fix.first[3])
  dateAll<-date.mdy(dateFirst+t)
  rem<-(t-floor(t))
  hour<-floor(rem*24+1.0e-6) 
  rem<-rem-hour/24
  min<-floor(rem*24*60+1.0e-6) 
  rem<-rem-min/24/60
  sec<-floor(rem*24*60*60+1.0e-6) 
  rem<-rem-min/24/60/60
  dateMat<-cbind(dateAll$year,dateAll$month,dateAll$day,hour,min,sec)

  ######################################################################
  if(!is.null(sstdates)){   
    strVec<-c("# Run program in simulation mode only 0=FALSE, 1=TRUE\n","1\n")
    if(!is.null(localsstfolder)){
      .sstFileVector<<-paste(localsstfolder,dir(localsstfolder), sep='/')
    }else{
      lonr<-range(ret$lonlat[,1])
      latr<-range(ret$lonlat[,2])
      path<-sst.fun(as.data.frame(dateMat),lonr[1],lonr[2],latr[1],latr[2])
    }
    strVec<-c(strVec,"\n#Number of sst data files\n",length(.sstFileVector),"\n")
    strVec<-c(strVec,"#List of file names\n")
    for(i in 1:length(.sstFileVector)){
      strVec<-c(strVec,.sstFileVector[i],'\n')
    }
    bnvec<-basename(.sstFileVector)
    y1<-as.numeric(substr(bnvec,from.ystr[1], from.ystr[2]))
    y2<-as.numeric(substr(bnvec,to.ystr[1], to.ystr[2]))
    d1<-as.numeric(substr(bnvec,from.dstr[1], from.dstr[2]))
    d2<-as.numeric(substr(bnvec,to.dstr[1], to.dstr[2]))
    date1<-(mdy.date(year=y1, month=1, day=1)+d1-1)
    date2<-(mdy.date(year=y2, month=1, day=1)+d2-1)
    middate<-date.mdy(0.5*(date1+date2))
    datevec<-JD(middate$year,middate$month,middate$day, type='universal')
    strVec<-c(strVec,"#Corresponding mid-dates (rounded upwards)\n")
    strVec<-c(strVec,paste(datevec, collapse=' '),"\n")
    strVec<-c(strVec,"\n")
    strVec<-c(strVec,"# radius\n",rad,"\n","# bsst\n",bsst,"\n")
    cat(strVec,file='sstsim.dat')        
    tttime<-JD(dateMat[,1],dateMat[,2],
               dateMat[,3]+dateMat[,4]/24+dateMat[,5]/60/24+dateMat[,6]/60/60/24, 
               type='universal')
    ssttime<-JD(sstdates[,1],sstdates[,2],
                sstdates[,3]+sstdates[,4]/24+sstdates[,5]/60/24+sstdates[,6]/60/60/24, 
                type='universal')
    sstlon<-approx(tttime,ret$lonlat[,1],xout=ssttime)$y
    sstlat<-approx(tttime,ret$lonlat[,2],xout=ssttime)$y
    sstout<-cbind(ssttime,sstlon,sstlat)
    cat("# Number of sst obs\n",nrow(sstout),"\n",file='sstsim.dat', append=TRUE)
    write.table(sstout,row.names=FALSE, col.names=FALSE, quote=FALSE, 
                file='sstsim.dat', append=TRUE)
    cat("dummy\n",file='ukf.dat')
    if (.Platform$OS.type == "windows") {
      error.code<-.sys(paste(.path.package("trackit"),"/admb/bin/ukf.exe", sep = ""))
    }else {
      file.copy(paste(.path.package('trackit'),"admb/bin/ukf",sep="/"),"ukf", TRUE)
      .sys("chmod u+x ukf")
      error.code<-.sys(paste("./ukf", sep = ""))
    }
    sst<-read.table('sstsim.out')
    sst[,2]<-sst[,2]+rnorm(nrow(sst), sd=sqrt(sssst))
    sst<-cbind(sstdates,sst[,2])
    # feed this to admb and read the output back ... add noise 
  }
  ######################################################################



  ret$date<-as.numeric(mdy.date(year=dateMat[,1], month=dateMat[,2], day=dateMat[,3]))+
            dateMat[,4]/24+dateMat[,5]/24/60+dateMat[,6]/24/60/60
  	
  angle<-solar.altitude(lon=lonlatE[,1], lat=lonlatE[,2], 
                        year=dateMat[,1], month=dateMat[,2], day=dateMat[,3], 
                        hour=dateMat[,4], min=dateMat[,5], sec=dateMat[,6])
  ret$angle<-angle
  sunapp<-sundata
  sunappfun<-approxfun(sunapp)
  ret$light<-sunappfun(angle)+rnorm(length(angle), sd=sqrt(ss3))

  # Write file
  cat("# Simulated data file\n", file=tmpfile)
  cat("# scanit\n1\n", file=tmpfile, append=TRUE)
  cat("# fixFirst (lon lat year month day hour min sec)\n", file=tmpfile, append=TRUE)
  cat(fix.first, " 0 0 0\n", file=tmpfile, append=TRUE)
  cat("0 0\n", file=tmpfile, append=TRUE)
  cat("0 0\n", file=tmpfile, append=TRUE)
  cat("# fixLast\n", file=tmpfile, append=TRUE)
  cat(lonlat[nrow(lonlat),]," ",dateMat[nrow(dateMat),1:6], "\n", file=tmpfile, append=TRUE)
  cat("0 0\n", file=tmpfile, append=TRUE)
  cat("0 0\n", file=tmpfile, append=TRUE)
  cat("# fraction of day used around each solar event\n", file=tmpfile, append=TRUE)
  cat(window, "\n", file=tmpfile, append=TRUE)
  cat("# dims\n", file=tmpfile, append=TRUE)
  cat(nrow(lonlat), "\n", file=tmpfile, append=TRUE)
  cat(9, "\n", file=tmpfile, append=TRUE)
  cat("# year month day hour min sec depth light temp\n", file=tmpfile, append=TRUE)
  write.table(cbind(dateMat, 0, ret$light, 0), row.names=FALSE, col.names=FALSE, file=tmpfile, append=TRUE)
  cat("# Exclude dates\n", file=tmpfile, append=TRUE)
  cat("0\n", file=tmpfile, append=TRUE)
  if(keepfiles['true']){
    write.table(ret$lonlat, file=truefile, row.names=FALSE, col.names=FALSE)
    ret$truefile<-file.path(truefile)
  }
  dirname<-paste(tempdir(), 'sim', sep='')
  dir.create(dirname)
  oldwd<-getwd()
  setwd(dirname)
  file.copy(paste(oldwd,tmpfile,sep="/"),"input.dat", TRUE)
  file.copy(paste(.path.package("trackit"),'admb/src/deltaT.dat',sep="/"),"deltaT.dat", TRUE)
  file.copy(paste(.path.package("trackit"),'admb/src/lunar.dat',sep="/"),"lunar.dat", TRUE)
  cat("# dummy file", file='prepro.dat')
  if (.Platform$OS.type == "windows") {
    error.code<-.sys(paste(.path.package("trackit"),"/admb/bin/prepro.exe", sep = ""))
  }else {
      file.copy(paste(.path.package('trackit'),"admb/bin/prepro",sep="/"),"prepro", TRUE)
      .sys("chmod u+x prepro")
      error.code<-.sys(paste("./prepro", sep = ""))
  }
  file.copy("ukf.dat", paste(oldwd,datfile,sep="/"), TRUE)
  ############################################################
  ret$has.sst <- !is.null(sstdates)
  if(ret$has.sst){
    badIdx<-c(grep('sim',strVec)+0:1, grep('radius',strVec)+0:1,grep('bsst',strVec)+0:1) 
    tail<-c(strVec[-badIdx],"\n")
    
    sstJDU<-JD(sst[,1],sst[,2],sst[,3]+sst[,4]/24+sst[,5]/1440+sst[,6]/86400, type='universal')
    sstout<-cbind(sstJDU,sst[,7]) 
    tail<-c(tail,"\n#Number of sst observations\n",nrow(sstout),"\n")
    for(i in 1:nrow(sstout)){
      tail<-c(tail,paste(sstout[i,], collapse=' '),"\n")
    }
  }else{
    tail<-c("\n#Number of sst data files\n",0,"\n")
  }
  cat(tail, file=paste(oldwd,datfile,sep="/"), append=TRUE) 
  ret$tail<-tail
  ############################################################

  setwd(oldwd)
  unlink(dirname, TRUE)
  addCorrNoise()

  class(ret)<-"trackit.sim"
  options(digits=oldDigits)
  return(ret)
}

prepit<-function(track, fix.first, fix.last, scan=TRUE, window=c(0.05,0.01), 
                 tmpfile='input.dat', datfile='ukf.dat', keepfiles=c(FALSE,FALSE), 
                 internal=TRUE, sst=NULL, from.ystr=c(3,6), from.dstr=c(7,9), 
                 to.ystr=c(11,14), to.dstr=c(15,17), localsstfolder=NULL){

  oldDigits<-options()$digits; options(digits=22)

  xx<-as.numeric(mdy.date(track$month, track$day, track$year))+track$hour/24+track$min/24/60+track$sec/24/60/60
  if(any(diff(xx)<0))stop('Dates (including times) are not sorted in increasing order.')
  
  if((!internal)&(!keepfiles[2]))stop("Either \'internal\' or \'keepfiles[2]\' has to be TRUE, or nothing is returned")
  cat("Writing to temp files ... ")
  ret<-list()
  ret$fix.first<-fix.first[1:8]
  if(length(fix.first)==11){
    ret$fix.first.var<-matrix(fix.first[c(9,11,11,10)],nrow=2,ncol=2)
  }else{
    ret$fix.first.var<-matrix(0,nrow=2,ncol=2)
  }
  ret$fix.last<-fix.last[1:8]
  if(length(fix.last)==11){
    ret$fix.last.var<-matrix(fix.last[c(9,11,11,10)],nrow=2,ncol=2)
  }else{
    ret$fix.last.var<-matrix(0,nrow=2,ncol=2)
  }
  fix.first[1]<-(360-fix.first[1])%%360 # Now it is west pos 
  fix.last[1]<-(360-fix.last[1])%%360   # Now it is west pos 
  ret$scan<-scan
  ret$window<-window
  vec2str<-function(x)paste(x,collapse=" ")
  ret$prehead<-c(
    "# Data file written from the trackit package",
    "# scanIt",
    ifelse(scan,1,0),
    "# fixFirst",
    vec2str(fix.first[1:8]),
    vec2str(ret$fix.first.var[1,]),
    vec2str(ret$fix.first.var[2,]),
    "# fixLast",
    vec2str(fix.last[1:8]), 
    vec2str(ret$fix.last.var[1,]),
    vec2str(ret$fix.last.var[2,]),
    "# fraction of day used around each solar event",
    vec2str(window),
    "# dims",
    nrow(track), 
    ncol(track), 
    "# year month day hour min sec depth light temp"
  )
  writeLines(ret$prehead, tmpfile)
  write.table(track, row.names=FALSE, col.names=FALSE, file=tmpfile, append=TRUE)
  cat("# Exclude dates\n 0 \n", file=tmpfile, append=TRUE)
  cat("Done.\n")
  dirname<-paste(tempdir(), 'prep', sep='')
  dir.create(dirname)
  oldwd<-getwd()
  setwd(dirname)
  file.copy(paste(oldwd,tmpfile,sep="/"),"input.dat", TRUE)
  file.copy(paste(.path.package("trackit"),'admb/src/deltaT.dat',sep="/"),"deltaT.dat", TRUE)
  file.copy(paste(.path.package("trackit"),'admb/src/lunar.dat',sep="/"),"lunar.dat", TRUE)
  cat("# dummy file", file='prepro.dat')
  if (.Platform$OS.type == "windows") {
    error.code<-.sys(paste(.path.package("trackit"),"/admb/bin/prepro.exe", sep = ""))
  }else {
      file.copy(paste(.path.package('trackit'),"admb/bin/prepro",sep="/"),"prepro", TRUE)
      .sys("chmod u+x prepro")
      error.code<-.sys(paste("./prepro", sep = ""))
  }
  file.copy("ukf.dat", paste(oldwd,datfile,sep="/"), TRUE)
  setwd(oldwd)
  unlink(dirname, TRUE)
  alllines<-readLines(datfile)
  skiplines<-grep('obsMat', alllines)
  ret$posthead<-alllines[1:skiplines]

  if(internal){
    cat("Creating internal object ... ")
    ret$data<-matrix(scan(datfile, skip=skiplines, quiet=TRUE), ncol=13, byrow=TRUE)
    cat("Done.\n")
  }  
  
  ############################################################
  tail<-'\n'
  ret$has.sst<-!is.null(sst)
  if(ret$has.sst){ 
    if(!is.null(localsstfolder)){
      .sstFileVector<<-paste(localsstfolder,dir(localsstfolder), sep='/')
    }
    tail<-c(tail,"\n#Number of sst data files\n",length(.sstFileVector),"\n")
    tail<-c(tail,"#List of file names\n")
    for(i in 1:length(.sstFileVector)){
      tail<-c(tail,.sstFileVector[i],'\n')
    }
    bnvec<-basename(.sstFileVector)
    y1<-as.numeric(substr(bnvec,from.ystr[1], from.ystr[2]))
    y2<-as.numeric(substr(bnvec,to.ystr[1], to.ystr[2]))
    d1<-as.numeric(substr(bnvec,from.dstr[1], from.dstr[2]))
    d2<-as.numeric(substr(bnvec,to.dstr[1], to.dstr[2]))
    date1<-(mdy.date(year=y1, month=1, day=1)+d1-1)
    date2<-(mdy.date(year=y2, month=1, day=1)+d2-1)
    middate<-date.mdy(0.5*(date1+date2))
    datevec<-JD(middate$year,middate$month,middate$day, type='universal')
    tail<-c(tail,"#Corresponding mid-dates (rounded upwards)\n")
    tail<-c(tail,paste(datevec, collapse=' '),"\n")
    tail<-c(tail,"\n")
    
    sstJDU<-JD(sst[,1],sst[,2],sst[,3]+sst[,4]/24+sst[,5]/1440+sst[,6]/86400, type='universal')
    sstout<-cbind(sstJDU,sst[,7]) 
    tail<-c(tail,"\n#Number of sst observations\n",nrow(sstout),"\n")
    for(i in 1:nrow(sstout)){
      tail<-c(tail,paste(sstout[i,], collapse=' '),"\n")
    }
  }else{
    tail<-c(tail,"\n#Number of sst data files\n",0,"\n")
  }
  cat(tail, file=datfile, append=TRUE) 
  ret$tail<-tail
  ############################################################

  if(keepfiles[1]){
    ret$prefile<-file.path(tmpfile)
  }else{
    unlink(tmpfile)
  }
  if(keepfiles[2]){
    ret$postfile<-file.path(datfile)
  }else{
    unlink(datfile)
  }
  class(ret)<-"trackit.scan"
  options(digits=oldDigits)
  return(ret)
}

.trimit<-function(scan){
  oldDigits<-options()$digits; options(digits=22)
  if(is.null(scan$data))stop("This function can only be used if internal=TRUE in call to prepit()")
  hid<-(scan$data[,1]-floor(scan$data[,1]))*24
  hid<-(hid+(12-mean(hid)))%%24
  plot(hid, ylab="Hours into day")
  idx<-as.integer(unique((scan$data[,12])))
  remove<-logical(length(idx))
  no<-as.numeric(table(scan$data[,12]))
  xx<-1:nrow(scan$data)
  xscale<-nrow(scan$data)-1
  while(TRUE){
    xy<-locator(1)
    if(is.null(xy))break
    sunsetno<-scan$data[which.min(((xx-xy$x)/xscale)^2+((hid-xy$y)/24)^2),12]
    remove[sunsetno]<-!remove[sunsetno]
    locIdx<-which(scan$data[,12]==sunsetno)
    points(xx[locIdx],hid[locIdx], col=ifelse(remove[sunsetno],'red','black'))
  }
  keep<-scan$data[,12]%in%idx[!remove]
  scan$data<-scan$data[keep,]
  scan$data[,12]<-pmatch(scan$data[,12],unique(scan$data[,12]), duplicates.ok = TRUE)
  scan$posthead[8]<-nrow(scan$data)
  scan$posthead[12]<-length(unique(scan$data[,12]))
  no<-no[!remove]
  scan$posthead[14]<-paste(as.integer(cumsum(c(1,no[-length(no)]))),collapse = " ")
  scan$posthead[16]<-paste(as.integer(cumsum(no)),collapse = " ")
  options(digits=oldDigits)
  return(scan)
}


trackit<-function(prep.track, a2lpoints=15,
                  u.init=0, v.init=0, D.init=100, ss1.init=1, ss2.init=5, ss3.init=1, rho.init=0.01, 
                  bsst.init=0, sssst.init=0.01, rad.init=200, dep1.init=0, dep2.init=0, 
                  phi.init=c(60,rep((200-60)/(a2lpoints-1),a2lpoints-1)),    
                  init=c(u.init,v.init,D.init,ss1.init,ss2.init,ss3.init,rho.init,
                         bsst.init,sssst.init,rad.init,dep1.init,dep2.init,phi.init), 
                  u.ph=-1, v.ph=-1, D.ph=3, ss1.ph=2, ss2.ph=2, ss3.ph=2, rho.ph=3, 
                  bsst.ph=-1, sssst.ph=2, rad.ph=3, dep1.ph=-1, dep2.ph=-1,phi.ph=1,  
                  phase=c(u.ph,v.ph,D.ph,ss1.ph,ss2.ph,ss3.ph,rho.ph,
                          bsst.ph,sssst.ph,rad.ph,dep1.ph,dep2.ph,phi.ph),
                  blue.light.only=FALSE, save.dir=NULL){
  init[1] <- -init[1]
  oldDigits<-options()$digits; options(digits=22)
  vec2str<-function(x)paste(x,collapse=" ")
  confLines<-c(
    "# Ponts in angle to light approximation", 
    a2lpoints,
    "# Parameter names u v D ss1 ss2 ss3 rho bsst sssst rad dep1 dep2 phi1 ... phiN", 
    "# Initial values", 
    vec2str(init),
    "# Estimation phase", 
    vec2str(phase)
  )
  if(is.null(save.dir)){
    dirname<-paste(tempdir(), 'run', sep='')
  }else{
    dirname<-paste(save.dir, 'run', sep='')
  }
  dir.create(dirname)
  ukfdat<-paste(dirname,"ukf.dat", sep='/')
  if(is.null(prep.track$postfile)){
    writeLines(c("# Blue light only", ifelse(blue.light.only,1,0), prep.track$posthead), ukfdat)
    write.table(prep.track$data, row.names=FALSE, col.names=FALSE, file=ukfdat, append=TRUE)
    cat(prep.track$tail, file=ukfdat, append=TRUE) 
  }else{
    lines<-readLines(prep.track$postfile)
    writeLines(c("# Blue light only", ifelse(blue.light.only,1,0), lines), ukfdat)
    #file.copy(prep.track$postfile,paste(dirname,"ukf.dat", sep='/'), TRUE)    
  }
  oldwd<-getwd()
  setwd(dirname)
  writeLines(confLines,'model.cfg')
  cat("# Run program in simulation mode only 0=FALSE, 1=TRUE\n",0,"\n", file="sstsim.dat") 
  if (.Platform$OS.type == "windows") {
    error.code<-.sys(paste(.path.package("trackit"),"/admb/bin/ukf.exe", sep = ""))
  }else {
      file.copy(paste(.path.package('trackit'),"admb/bin/ukf",sep="/"),"ukf", TRUE)
      .sys("chmod u+x ukf")
      error.code<-.sys(paste("./ukf", sep = ""))
  }
  ret<-list()
  pnames<-c("u","v","D","ss1","ss2","ss3","rho","bsst","sssst","rad","dep1","dep2","qSparTilde")
  ret$init<-init; ret$init[1] <- -ret$init[1]
  ret$phase<-phase
  ret$error.code
  mpt<-read.table('mpt.out')
  jdu2date<-function(jdu)as.date(jdu-2436934.5) 
  ret$decimal.date<-mpt$V1-2436934.5
  if(prep.track$has.sst){
    sstobsstr<-(prep.track$tail[(grep("Number of sst observations",prep.track$tail)+2):length(prep.track$tail)])
    sstobsmat<-matrix(as.numeric(unlist(strsplit(sstobsstr[sstobsstr!='\n'],' '))), ncol=2, byrow=TRUE)
    sstobsmat[,1]<-sstobsmat[,1]-2436934.5
    ret$sstobs<-sstobsmat
    ret$most.prob.sst<-mpt$V14
  }
  ret$date<-jdu2date(mpt$V1)
  ret$timeAL<-mpt$V1-mpt$V1[1]
  ret$most.prob.track<-cbind(x=(360-mpt$V8)%%360,y=mpt$V9)
  ret$var.most.prob.track<-cbind(mpt$V10,mpt$V11,mpt$V12,mpt$V13)
  std<-read.table('ukf.std', header=FALSE, skip=1)      
  ret$est<-unlist(sapply(pnames, function(n)std$V3[std$V2==n]))
  if("u"%in%names(ret$est)){ret$est["u"] <- -ret$est["u"]}
  ret$sd<-unlist(sapply(pnames, function(n)std$V4[std$V2==n]))
  names(ret$est)<-gsub('qSparTilde','phi', names(ret$est))
  names(ret$sd)<-gsub('qSparTilde','phi', names(ret$sd))
  par<-as.numeric(scan('ukf.par', what="", nlines=1, quiet=TRUE)[c(6,11,16)]) 
  ret$npar<-par[1]
  ret$nlogL<-par[2]
  ret$max.grad.comp<-par[3]
  setwd(oldwd)
  if(is.null(save.dir)){
    unlink(dirname, TRUE)
  }
  class(ret)<-"trackit"
  options(digits=oldDigits)
  return(ret)
}

two.layer.depth.corr<-function(track, daybyday=FALSE, D0=50){
  if(!daybyday){
    p<-ifelse(track$depth>50,track$depth-D0,0)
    n<-ifelse(track$depth<=50,track$depth,D0)
    dl <- diff(track$light)
    dp <- diff(p)
    dn <- diff(n)
    ok <- complete.cases(cbind(dl,dp,dn))
    y<-rep(NA,2)
    y[1]<- -sum(dl[ok]*dn[ok])
    y[2]<- -sum(dl[ok]*dp[ok])
    A<-matrix(NA, 2, 2)
    A[1,1]<-sum(dn[ok]^2)
    A[1,2]<-sum(dn[ok]*dp[ok])
    A[2,1]<-A[1,2]
    A[2,2]<-sum(dp[ok]^2)
    K<-solve(A,y)
    track$light<-track$light+K[1]*n+K[2]*p
  }else{
    f<-as.factor(paste(track$year,'x',track$month,'x',track$day, sep=''))
    dummy<-tapply(1:nrow(track), INDEX=f, FUN=function(i){
      track[i,]<<-two.layer.depth.corr(track[i,],daybyday=FALSE, D0=D0)
      NULL})
  } 
  return(track)
}

scroll<-function(track, n=1000){
  par(ask=TRUE)
    for(i in 0:(nrow(track)%/%n))plot(track$light[1:n+n*i])
  par(ask=FALSE)
}

plotlon<-function(fit, exr=NULL, top=FALSE){
  lon<-fit$most.prob.track[,1]
  std<-sqrt(fit$var.most.prob.track[,1])
  low<-lon-2*std
  hig<-lon+2*std
  ran<-range(c(low,hig)) 
  if(!is.null(exr)){
    ran<-range(c(ran,exr))
  }
  x<-fit$dec
  plot(fit$dec, lon, axes=FALSE, ylim=ran, xlab="", ylab="", type="n")
  axis(2, las=1)
  mtext(side=2, line=2.5, "Longitude (E)")
  if(top){
    axis(3, at=pretty(fit$date)->tmp, labels=tmp-fit$date[1], las=1)
    mtext("Days at liberty", line=2.5)
  }else{
    axis(1, at=pretty(fit$date)->tmp, labels=as.character(as.date(tmp)), las=1)
    mtext("Date", side=1, line=2.5)
  }
  box()
  polygon(c(x,rev(x)), c(low,rev(hig)), col='thistle2', border='thistle2')
  lines(fit$dec, lon, lwd=4, col='springgreen1')
  points(fit$dec, lon, pch=20)
}

plotlat<-function(fit, exr=NULL, mid=FALSE){
  lat<-fit$most.prob.track[,2]
  std<-sqrt(fit$var.most.prob.track[,4])
  low<-lat-2*std
  hig<-lat+2*std
  ran<-range(c(low,hig)) 
  if(!is.null(exr)){
    ran<-range(c(ran,exr))
  }
  x<-fit$dec
  plot(fit$dec, lat, axes=FALSE, ylim=ran, xlab="", ylab="", type="n")
  axis(2, las=1)
  mtext(side=2, line=2.5, "Latitude")
  if(!mid){
    axis(1, at=pretty(fit$date)->tmp, labels=as.character(as.date(tmp)), las=1)
    mtext("Date", side=1, line=2.5)
  }
  box()
  polygon(c(x,rev(x)), c(low,rev(hig)), col='thistle2', border='thistle2')
  lines(fit$dec, lat, lwd=4, col='springgreen1')
  points(fit$dec, lat, pch=20)
}


plotsst<-function(fit, exr=NULL){
  if(is.null(fit$sstobs)){
    stop("No SST in this data set.")
  }else{
    sst<-fit$most.prob.sst
    sstobs<-fit$sstobs
    ran<-range(c(sst,sstobs[,2])) 
    if(!is.null(exr)){
      ran<-range(c(ran,exr))
    }
    plot(fit$dec, sst, axes=FALSE, ylim=ran, xlab="", ylab="", type="n")
    axis(2, las=1)
    mtext(side=2, line=2.5, "SST")
    axis(1, at=pretty(fit$date)->tmp, labels=as.character(as.date(tmp)), las=1)
    mtext("Date", side=1, line=2.5)
    box()
    lines(fit$dec, sst, lwd=4, col='springgreen1')
    points(fit$dec, sst, pch=20)
    points(sstobs, pch=4)
  }
}

plot.trackit<-function(x, onlylonlat=FALSE,...){
  om<-par("mar")  
  om[1]<-om[3]
  if(onlylonlat){
    sstobs<-x$sstobs
    x$sstobs<-NULL
  }
  if(is.null(x$sstobs)){
    par(mfrow=c(2,1))
  }else{
    par(mfrow=c(3,1))
  }
  par(mar=c(.1,om[2:4]))
  plotlon(x, top=TRUE)
  if(is.null(x$sstobs)){
    par(mar=c(om[1:2],.1,om[4]))
  }else{
    par(mar=c((om[1]+.1)/2,om[2],(om[3]+.1)/2,om[4]))
  }
  plotlat(x, mid=!is.null(x$sstobs))
  if(!is.null(x$sstobs)){
    par(mar=c(om[1:2],.1,om[4]))
    plotsst(x)
  }
  if(onlylonlat){
    x$sstobs<-sstobs
  }
  par(mar=om)
}


get.sst.from.server<-function(track, lonlow, lonhigh, latlow, lathigh, folder=tempdir(), 
  server='http://atlas.nmfs.hawaii.edu/cgi-bin/reynolds_extract.py'){
  testdir<-file.info(folder)$isdir
  if(is.na(testdir)){
    dir.create(folder)
  }else{
    if(!testdir)stop("The folder name supplied is in fact a filename")
  }
  fl<-dir(folder)
  if(length(fl)!=0){
    folder<-paste(folder,'sst_temp', sep='/')
    dir.create(folder)
  }

  if(is.data.frame(track))track<-list(track)  

  minDate<-min(unlist(lapply(track,function(x)mdy.date(x[1,2],x[1,3],x[1,1]))))
  maxDate<-max(unlist(lapply(track,function(x)mdy.date(x[nrow(x),2],x[nrow(x),3],x[nrow(x),1]))))
  minDate<-minDate-10 # add a few days for good measure and
  maxDate<-maxDate+10 # because data source is 8-day files

  yrRan<-c(date.mdy(minDate)$year,date.mdy(maxDate)$year)
  daysIntoYearRan<-c(minDate-mdy.date(1,1,yrRan[1]),maxDate-mdy.date(1,1,yrRan[2]))

  lonRan<-c(lonlow,lonhigh)
  latRan<-c(latlow,lathigh)

  string<-''
  string<-paste(string,'?lon1=',lonRan[1],'&lon2=',lonRan[2],sep='')
  string<-paste(string,'&lat1=',latRan[1],'&lat2=',latRan[2],sep='')
  string<-paste(string,'&year1=',yrRan[1],'&day1=',daysIntoYearRan[1],sep='')
  string<-paste(string,'&year2=',yrRan[2],'&day2=',daysIntoYearRan[2],sep='')
  link<-paste(server,string, sep='')
  dest<-paste(folder,'temp.zip', sep='/')
  download.file(link, dest, mode='wb')
  
  if((as.numeric(version$major<=2))&(as.numeric(version$minor<=9))){
    .Internal(int.unzip(paste(folder,'temp.zip',sep='/'),NULL, folder))
  }else{
    unzip(paste(folder, "temp.zip", sep = "/"), exdir=folder)
  }

  #.Internal(int.unzip(paste(folder,'temp.zip',sep='/'),NULL, folder))
  # May need some special treatment for windows here using 'zip.unpack()'
  unlink(paste(folder,'temp.zip', sep='/'))
  .sstFileVector<<-paste(folder,dir(folder), sep='/')

  for(f in .sstFileVector){
    dat<-read.table(f, head=FALSE)
    write.table(dat[complete.cases(dat),], col.names=FALSE, row.names=FALSE, quote=FALSE, file=f)
  }

  #cat(paste(rep('=',options()$width), collapse=''),'\n\n')
  # put some docs in here ...
  #cat(paste(rep('=',options()$width), collapse=''),'\n\n')

  return(folder)
}  

get.blended.sst<-function(track, lonlow, lonhigh, latlow, lathigh, folder=tempdir(), 
  server="http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=", 
  nday = "5day"){
  fl<-get.avhrr.sst(track=track, lonlow=lonlow, lonhigh=lonhigh, latlow=latlow, 
                    lathigh=lathigh, folder=folder, server=server, product='TBAssta', nday=nday)
  return(folder)
}  

get.avhrr.sst <- function (track, lonlow, lonhigh, latlow, lathigh, 
                           folder = tempdir(), 
                           server = "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=",
                           product = "TAGssta", nday = "8day", centertime="12") 
{
    ## product = "TGAssta", nday = "3day", centertime="12" (datesteps = 3,5,8)
    ## product= "TNAssta", nday = "1day", centertime="00" (datesteps = 1)
    ## product = "TN2ssta", nday="1day", centertime="00" (datesteps = 1)
    server <- paste(server, product, sep="")
    fmtDate <- function(date) {
        x <- date.mdy(date)
        paste(x$year, formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
    fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }
    testdir <- file.info(folder)$isdir
    if (is.na(testdir)) {
        dir.create(folder)
    }
    else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
    }
    fl <- dir(folder)
    if (length(fl) != 0) {
        folder <- paste(folder, "sst_temp", sep = "/")
        dir.create(folder)
    }
    if (is.data.frame(track)) 
        track <- list(track)
    minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 3], x[1, 1]))))
    maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 3], x[nrow(x), 1]))))
    minDate <- minDate - 10
    maxDate <- maxDate + 10
    datesteps <- seq(minDate, maxDate, by = ifelse(nday == "3day", 
        3, 8))
    op <- paste("&timePeriod=NDAY&centeredTime=~DATET", centertime, 
                "%3A00%3A00&maxLat=MAXLAT&minLon=MINLON&maxLon=MAXLON&minLat=MINLAT&filetype=.xyz", sep="")
    for (d in datesteps) {
        opt <- sub("NDAY", nday, op)
        opt <- sub("DATE", fmtDate(d), opt)
        opt <- sub("MAXLAT", lathigh, opt)
        opt <- sub("MINLON", lonlow, opt)
        opt <- sub("MAXLON", lonhigh, opt)
        opt <- sub("MINLAT", latlow, opt)
        link <- paste(server, opt, sep = "")
        y1 <- date.mdy(d - ifelse(nday == "3day", 1, 3))$year
        d1 <- (d - ifelse(nday == "3day", 1, 3)) - mdy.date(month = 1, day = 1, year = y1) + 
            1
        y2 <- date.mdy(d + ifelse(nday == "3day", 1, 4))$year
        d2 <- (d + ifelse(nday == "3day", 1, 4)) - mdy.date(month = 1, day = 1, year = y2) + 
            1
        filename <- paste(substring(product,2,3), y1, fmtDay(d1), "_", y2, fmtDay(d2), 
            "_sst.xyz", sep = "")
        dest <- paste(folder, filename, sep = "/")
        download.file(link, dest, mode = "wb")
        tmp <- matrix(scan(dest), ncol = 3, byrow = TRUE)[, c(2, 
            1, 3)]
        write.table(tmp[complete.cases(tmp), ], file = dest, 
            quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    .sstFileVector <<- paste(folder, dir(folder), sep = "/")
    return(folder)
}

print.trackit<-function (x, ...)
{
  cat("# Trackit version:",packageDescription('trackit')$Version,"\n")
  cat("# Date:", format(Sys.time(), "%d %b %Y (%H:%M:%S)"), "\n")
  cat("# Negative log likelihood:", round(x$nlogL,4), "\n")
  cat("# Maximum gradient component:",formatC(abs(x$max.grad.comp), dig = 4, wid = 6, format = "f"), "\n")
  if(abs(x$max.grad.comp)<1.0e-4){
    cat("# which means the convergence criteria is obtained.\n\n")
  }else{
    if(abs(x$max.grad.comp)>1.0e-2){
      cat("# which means the convergence criteria is not obtained.\n\n")
    }else{
      cat("# which does not meet the convergence criteria, \n# but is very close, so convercence so is likely \n# obtained anyway.\n\n")
    }
  }
  if (abs(x$max.grad.comp) < 1e-02) {
    cat("Parameters:\n")
    print(rbind("Estimates:" = x$es, "Std. dev.:" = x$sd))
    cat("\n# This object contains the following sub-items:\n")
    print(names(x))
    objname<- paste(deparse(substitute(x), 500), collapse = "\n")
    cat("\n# To access these items use the $ operator. For instance")
    cat("\n# to get the most probable track type: fit$most.prob.track \n")
  }
}

plotbasemap<-function(lon1, lon2, lat1, lat2, grid = FALSE, zoom = FALSE, 
                      landcolor = "darkgreen", seacolor = "lightblue", data=gmt3){
  xrange <- c(lon1,lon2)
  yrange <- c(lat1,lat2)
  aspect <- c(cos((mean(yrange) * pi)/180), 1)
  d <- c(diff(xrange), diff(yrange)) * (1 + 2 * 0.00) * aspect
         
  if (!par("new"))plot.new()
  p <- par("fin") - as.vector(matrix(c(0, 1, 1, 0, 0, 1, 1, 0), nrow = 2) %*% par("mai"))
  par(pin = p)
  p <- par("pin")
  p <- d * min(p/d)
  par(pin = p)
  d <- d * 0.00 + ((p/min(p/d) - d)/2)/aspect
  realusr<-c(xrange, yrange) + rep(c(-1, 1), 2) * rep(d, c(2, 2))
  par(usr = realusr)
  rect(lon1, lat1, lon2, lat2, col = seacolor)
  if (grid) {
    axis(1, tck = 1)
    axis(2, tck = 1)
  }

  if(xrange[1]<0){
    par(usr = realusr+c(360,360,0,0))
    polygon(data, border = landcolor, col = landcolor)
  }
  if(xrange[2]>360){
    par(usr = realusr-c(360,360,0,0))
    polygon(data, border = landcolor, col = landcolor)
  }
  par(usr = realusr)
  polygon(data, border = landcolor, col = landcolor)
  rect(lon1, lat1, lon2, lat2, lwd=1)
  axis(1)
  mtext("Longitude", side=1, line=3)
  par(las=1)
  axis(2)
  mtext("Latitude", side=2, line=3, las=0)
  if (zoom) {
    ret <- locator(2)
    if (length(ret$x) < 2) {
      zoom <- FALSE
    }else{
      lon1 <- min(ret$x)
      lon2 <- max(ret$x)
      lat1 <- min(ret$y)
      lat2 <- max(ret$y)
    }
    plotbasemap(lon1, lon2, lat1, lat2, grid, zoom, landcolor, seacolor, data)
  }
}

fitmap<-function (x, ci = FALSE, ...){
  par(mfrow=c(1,1))
  .addrange<-function (x, pct = 0.05){
    minx <- min(x, na.rm = TRUE)
    maxx <- max(x, na.rm = TRUE)
    dif <- maxx - minx
    c(minx, maxx) + c(-1, 1) * dif * pct
  }
  .CI.reg<-function (x, level = 0.95, npoints = 100, col = "blue", border = 0, density = 20, 
                     lwd = 0.1 * par("lwd"), ...){
    t.quan <- sqrt(qchisq(level, 2))
    centre <- x[5:6]
    x <- matrix(x[1:4], 2, 2)
    r <- x[1, 2]
    scale <- sqrt(diag(x))
    if (scale[1] > 0) {
        r <- r/scale[1]
    }
    if (scale[2] > 0) {
        r <- r/scale[2]
    }
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    polygon(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1],
        t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints,
        2), col = col, border = border, density = density, lwd = lwd,
        ...)
  }
  x$nobs<-nrow(x$most.prob.track)
  xlow <- x$most.prob.track[, 1] - 2 * sqrt(x$var.most.prob.track[,1])
  xhig <- x$most.prob.track[, 1] + 2 * sqrt(x$var.most.prob.track[,1])
  ylow <- x$most.prob.track[, 2] - 2 * sqrt(x$var.most.prob.track[,4])
  yhig <- x$most.prob.track[, 2] + 2 * sqrt(x$var.most.prob.track[,4])
  
  xrange <- .addrange(c(x$most.prob.track[,1], if (ci) {c(xlow, xhig)}))
  yrange <- .addrange(c(x$most.prob.track[,2], if (ci) {c(ylow, yhig)}))
  plotbasemap(xrange[1],xrange[2],yrange[1],yrange[2],...)
  if (ci) {
    apply(cbind(x$var.most.prob.track, x$most.prob.track),1, .CI.reg)
  }
  lines(x$most.prob.track, col = "blue")
  points(x$most.prob.track[c(1,x$nobs),], pch=c(6,2))
}
