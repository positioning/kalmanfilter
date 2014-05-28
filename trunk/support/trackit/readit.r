# Read WC Instrument Helper output file for archival tag
# Archive > C > Save Instrument Readings > R Read.Table Format

read.arc <- function(myfile){
  dat <- read.table(myfile, skip=3)
  dt <- format(strptime("1970-01-01", "%Y-%m-%d") + dat$Time, "%Y/%m/%d/%H/%M/%S")
  c16 <- data.frame(matrix(as.numeric(unlist(strsplit(dt, "/"))), ncol=6, byrow=T))
  nn <- names(dat)
  c7 <- which(match(nn, "Corrected.Depth.Channel")==1)
  c7 <- ifelse(length(c7)>0, c7, which(match(nn, "Depth")==1))
  c8 <- which(match(nn, "Light.Level")==1)
  c9 <- which(match(nn, "Recorder.Temperature")==1)
  mydat <- cbind(c16, dat[,c7], dat[,c8], dat[,c9])
  names(mydat) <- c('year','month','day','hour','min','sec','depth','light','temp')
  return(mydat)
}

# Read Lotek archival tag output file (.csv) from Viewer 2000

read.lotek <- function(myfile, date.format="%m/%d/%Y %H:%M:%S", 
		       surface.light=F, corr.depth=T){
  dat <- read.csv(myfile)
  dt <- format(strptime(dat$Timestamp, date.format), "%Y/%m/%d/%H/%M/%S")
  c16 <- data.frame(matrix(as.numeric(unlist(strsplit(dt, "/"))), ncol=6, byrow=T))
  nn <- names(dat)
  c7 <- which(match(nn, "Depth...dBar")==1)
  if (surface.light){
     c8 <- which(match(nn, "Light.at.Surface")==1)} else {
     c8 <- which(match(nn, "Light.at.Depth")==1)
     }
  c9 <- which(match(nn, "Ext.Temp.deg.C")==1)
  if (corr.depth){zz <- ifelse(dat[,c7]>=0, dat[,c7], 0)}
  mydat <- cbind(c16, zz, dat[,c8], dat[,c9])
  names(mydat) <- c('year','month','day','hour','min','sec','depth','light','temp')
  if (corr.depth)(mydat <- mydat[1:(which(mydat$depth==4055)[1]-1),])
  return(mydat)
}

# Read WC PSAT output, the "Locations" worksheet
# Saved from an Excel file after reformatting into a .csv

read.psat <-function(filen, len=88){
  cc<-rep("NULL",len)
  cc[14:37]<-"character"
  cc[38:61]<-"numeric"
  if(len==85){cc[62:85]<-"numeric"}
  dat<-read.csv(filen, skip=1, head=FALSE, colClasses=cc)
  names(dat)<-paste("V", 1:ncol(dat), sep='')
  date1<-t(matrix(as.numeric(unlist(strsplit(substr(dat$V1,1,10),'/'))),3))[,c(3,1,2)]
  date2<-t(matrix(as.numeric(unlist(strsplit(substr(dat$V13,1,10),'/'))),3))[,c(3,1,2)]
  geth<-function(v)as.numeric(unlist(lapply(strsplit(v, ':'),function(x)x[1])))%%24
  getm<-function(v)as.numeric(unlist(lapply(strsplit(v, ':'),function(x)x[2])))
  gets<-function(v)as.numeric(unlist(lapply(strsplit(v, ':'),function(x)x[3])))
  
  T1<-cbind(substr(dat$V1,12,19),dat[2:12])
  T2<-cbind(substr(dat$V13,12,19),dat[14:24])
  h1<-apply(T1,2,geth) 
  h2<-apply(T2,2,geth) 
  m1<-apply(T1,2,getm) 
  m2<-apply(T2,2,getm) 
  s1<-apply(T1,2,gets) 
  s2<-apply(T2,2,gets) 
  l1<-dat[,25:36]
  l2<-dat[,37:48] 
  if(len==85){
    d1<-dat[,49:51]
    d2<-dat[,52:63]
  }else{
    d1<-rep(0,12)
    d2<-rep(0,12)
  }
  dat1<-cbind(date1[rep(1:nrow(date1),each=ncol(h1)),],
              as.vector(t(h1)),
              as.vector(t(m1)),
              as.vector(t(s1)),
              as.vector(t(d1)),
              as.vector(t(l1)),0)
  dat2<-cbind(date2[rep(1:nrow(date1),each=ncol(h2)),],
              as.vector(t(h2)),
              as.vector(t(m2)),
              as.vector(t(s2)),
              as.vector(t(d2)),
              as.vector(t(l2)),0)
  
  dat<-as.data.frame(rbind(dat1,dat2))
  names(dat)<-c('year','month','day','hour','min','sec','depth','light','temp')
  o<-order(dat$year, dat$month, dat$day, dat$hour, dat$min, dat$sec)
  
  return(dat[o,])
}

