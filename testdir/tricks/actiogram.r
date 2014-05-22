uments to actigram:

# dat.file = name of the data file to use, eg "D1571-local-time.csv"
# type = type of data file, used to specify which read function to use for the file, eg "D1571"
# dt = interval in seconds between measurements, eg 60 
# show.days = number of columns per day, defaults to 2 to show two complete 24 hour cycle.
#
# actigram("D1571-local-time.csv","D1571",60)
#a2 <- actigram(gws50,"dat",120,mytitle="Great White Shark TagID 50", mylabel="Depth (m)")
#a2 <- actigram(gws50,"dat",120,mytitle="Great White Shark TagID 50", mylabel="Temp (C)", zvar="temp",zlim=c(0,30))

# remember to switch to palette2
#a2 <- actigram(act50,"act",60*60,mytitle="Great White Shark TagID 50 \n Activity (> 10 m change between 2-min interval)", zlim=c(0,30),zvar="a10", mylabel="Count")
#a2 <- actigram(gws48,"dat",120,mytitle="Great White Shark TagID", mylabel="Delta Temp (C)", zvar="dt",zlim=c(-10,10))
#a2 <- actigram(gws48,"dat",120,mytitle="", mylabel="Light (unit)", zvar="daynight", zlim=c(0,10))
#a2 <- actigram(act50,"act",60*60,mytitle="Great White Shark TagID 50 \n Thermal stress in an hour i.e. sum of (average daily SST - ambient temp)", zlim=c(0,550),zvar="heatsts", mylabel="degC")

require(date)
sec.per.day<-24*60*60

actigram<-function(dat.file,type,dt,show.days=2,zvar="depth",
			doyr=c("",""),zlim=c(0,1000),nlevels=8,
			mytitle="",mylabel=NA, fname=NA, ...)
{
  dat<-get.data(dat.file,type)
  if (is.na(fname)) fname <- deparse(substitute(dat.file))
  if (dat == "NA")
    return(dat)
  z<-get.z(dat,dt,show.days,zvar)
  z<-z.plot(z,dat,dt,dat.file,doyr,zvar,zlim,nlevels,
	 mytitle,mylabel,fname=fname, ...)
  #return(list(dat=dat,z=z,dat.file=dat.file,type=type,dt=dt,show.days=show.days))
  return(z)
}

read.dat<-function(dm)
{
  #colnames(dm)<-c("year","month","day","hour","min","sec","depth","light","exttemp","dz","adz","tagid")
  #colnames(dm)<-c("year","month","day","hour","min","sec","depth","light","temp")
  return(dm)
}
read.act<-function(dm)
{
  #colnames(dm)<-c("year","month","day","hour","min","sec","a10","a20","a30","a40","tagid")
  return(dm)
}

get.data<-function(file,type)
{
  print("HERE 1")
  m = pmatch(type,table=c("dat", "act"),nomatch=0)
  if (m == 2)
    dd<-read.act(file)
  else if (m == 1)
    dd<-read.dat(file)
  else
  {
    print(paste("No read function for file type",type,". Returning NA."),quote=FALSE)
    return("NA")
  }
  print(head(dd))
  print("HERE 2")
  idate<-as.numeric(mdy.date(dd[,"month"],dd[,"day"],dd[,"year"]))
  print("HERE 3")
  Sec<-apply(dd,1,myISOdatetime)
  dd<-cbind(idate,Sec,dd) 
  print(head(dd))
  return(dd)
}

myISOdatetime<-function(v)
{
  dt<-ISOdatetime(v["year"], v["month"], v["day"], v["hour"], v["min"], v["sec"],tz="GMT")
  return(dt)
}

z.plot<-function(z,dd,dt,dat.file,doyr=c("",""),
		 zvar,zlim,nlevels,
		 mytitle,mylabel,fname, saveplot=T)
{
  print("Plotting matrix")
  file.root <- paste(fname,zvar,sep="-")
  # Palette 1
  #zRamp <-  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
  #                   "green", "yellow", "#FF7F00", "red", "#7F0000"))
  # Palette 2
  zRamp <-  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  #zRamp <-  colorRampPalette(c("grey","magenta","#00007F", "blue", "#007FFF", "cyan", 
  #                   "#7FFF7F", "yellow", "gold", "#FF7F00", "red", "#7F0000"))
  zcol<-zRamp(nlevels+2)
  levels <- pretty(zlim, nlevels+1)
  
  tod <- c(1:nrow(z))*24*dt/sec.per.day
  print(doyr)
  d1 <- as.date(doyr[1],"ymd")
  if (is.na(d1))
    d1 <- dd$idate[1]
  d2 <- as.date(doyr[2],"ymd")
  if (is.na(d2))
    d2 <- dd$idate[nrow(dd)]
  doy<-seq(d1,d2)
  print(paste(d1,d2,(d2-d1+1)))
  print(length(doy))
  print(dim(t(z)))

  zd1<-d1-dd$idate[1]+1
  zd2<-zd1+(d2-d1)
  print(paste(zd1,zd2,(zd2-zd1+1)))
  print(paste(d1,d2,(d2-d1+1)))

  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2]) * par("csi") * 2
  nf<-layout(matrix(c(1, 2), nc = 2), widths = c(1,lcm(w)))
  layout.show(nf)
  mar <- mar.orig
  mar[4] <- 1
  par(mar = mar)

  image(t(z)[zd1:zd2,] , y=tod, ylab="", x=doy, xlab="", zlim=zlim, col=zcol,axes=F)
  mtic<-which(date.mdy(doy)$day==1)
  tic1<-which(date.mdy(doy)$day==7)
  tic2<-which(date.mdy(doy)$day==14)
  tic3<-which(date.mdy(doy)$day==21)
  tic4<-which(date.mdy(doy)$day==28)
  axis(1,at=doy[mtic],labels=date.mmddyyyy(doy[mtic]),tcl=-0.6)
  axis(1,at=doy[tic1],tcl=-0.3,labels=F)
  axis(1,at=doy[tic2],tcl=-0.3,labels=F)
  axis(1,at=doy[tic3],tcl=-0.3,labels=F)
  axis(1,at=doy[tic4],tcl=-0.3,labels=F)
  mtext("Date",side=1,line=2.5)
  axis(2,at=seq(0,max(tod),by=6)) 
  axis(2,at=seq(0,max(tod),by=1),tcl=-0.3, labels=F) 
  mtext("Hour of Day (GMT)",side=2,line=3,las=0)
  rug(doy,side=3,col="yellow")
  box()
  title(main=mytitle)

  mar <- mar.orig
  mar[4] <- mar[2]
  mar[2] <- 0
  par(mar = mar)
  plot.new()  
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")
  mtext(ifelse(is.na(mylabel),toupper(zvar),mylabel), adj=1, at=2.5, line=1)
  ticks <- seq(zlim[1],zlim[2],by=(zlim[2]-zlim[1])/(nlevels+2))
  rect(0, ticks[-length(ticks)], 1, ticks[-1], col = zcol)
  axis(4, at=ticks, labels=ticks, las=1)
  box()
  
  if (saveplot){
    dpi<-96
    height<-6
    width <-9
    pheight<-height*dpi
    pwidth<-width*dpi
    file.png<-paste(file.root,"_actigram.png",sep="")
    if (as.list(capabilities())$png)
    {
    # this produces a png file but the areas behind the axis annotations are transparent
     dev.copy(device=png,file=file.png,res=dpi,bg="white",width=pwidth,height=pheight)
     print(paste("Created file",file.png))
     dev.off()
    }
    # this produces more useful png file, but takes longer
    file.eps<-paste(file.root,"_actigram.eps",sep="")
    print(paste("Creating file",file.eps))
    dev.copy2eps(file=file.eps,width=width,height=height, fonts="Times")
    cmd<-paste("pstoimg -antialias -aaliastext -quiet -density=",dpi,
              " -type png -crop a -out ",file.png," ", file.eps,sep="")
    print(paste("Executing ",cmd))
    system(cmd)
    print(paste("Created file",file.png))
    # remove the eps because it is huge
    system(paste("rm",file.eps,sep=" "))
  }
  
  # return the full image for plotting
  return(list(x=doy,y=tod,z=t(z)[zd1:zd2,]))
}

get.z<-function(dd,dt,show.days,zvar)
{
  print("Generating matrix")

  nday <- dd[nrow(dd),]$idate  - dd[1,]$idate + 1
  day.v<-seq(1,nday)

  tod.v<-seq(1,show.days*sec.per.day/dt) # dt is sampling interval in seconds
  print(length(tod.v))

  z<-matrix(nrow=length(tod.v),ncol=length(day.v))

  i1 <- dd[1,]$idate-1
  i2 <- i1+show.days
  iend <- dd[nrow(dd),]$idate
  i = 0
  while (i2 <= iend)
  { 
    i1 <- i1 + 1
    i2 <- i2 + 1
    i <- i + 1
    ii<-which(((dd$idate >= i1) & (dd$idate < i2)))
    jj<-1+(dd[ii,]$Sec-dd[ii,]$Sec[1])/dt

    print(paste(i,i1,i2,length(ii),length(jj)))
    
    dat<-dd[ii,][which(names(dd)==zvar)]

    names(dat)<- c("value")
    z[jj,i]<- dat$value
  }
  return(z)
}