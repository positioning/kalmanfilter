# 1. Load library and data files
library(ukfsst) 
setwd('c:/temp') 
track<-read.csv('ZZZ-Tag2-11A0888-11A0888kfsst.csv', header=TRUE) 
ii <- which(track$lon > 100)
track$lon[ii] <- NA
ii <- which(track$lat > -20)
track$lat[ii] <- NA
ii <- which(track$lat < -50)
track$lat[ii] <- NA
kf <- kftrack(na.omit(track[,1:5]))
# 2. Check web source for downloading satellite SST 
sst.path<-get.sst.from.server(na.omit(track[,1:5])) 
 # Only needed for kfsst: Prepare smoothed SST-field
 # sst.file<-write.sst.field(sst.path) 
# 3. Fit data to model, type ?kfsst to see all available options
jj <- intersect(which(is.na(track$lon)==T),which(is.na(track$lat)==T))
kk = as.vector(kf$est) 
ii = c(kk[c(1:5)],0,kk[c(6:7)],1,kk[c(8:9)],300)
jj <- c(jj, 159:166)
fit<-kfsst(track[-jj,], theta.init=ii, theta.active=c(1,1,1,0,0,0,1,1,1,1,1,0))
# 4. Plot results if model convergence is obtained
library(trackit)
fitmap(fit, ci=TRUE) 
points(fit$most.prob.track, pch=20, col='black') 

# 1. Load library and data files
library(ukfsst) 
setwd('c:/temp') 
track<-read.csv('WSH-Tag3-11A0944-11A0944kfsst.csv', header=TRUE)
kf <- kftrack(track[,1:5])
#library(trackit)
#fitmap(kf, ci=T); points(kf$most.prob.track, pch=20)
# 2. Check web source for downloading satellite SST
sst.path<-get.sst.from.server(track) 
 # Only needed for kfsst: Prepare smoothed SST-field
 # sst.file<-write.sst.field(sst.path) 
# 3. Fit data to model, type ?kfsst to see all available options
fit<-kfsst(track) 
# 4. Plot results if model convergence is obtained
plot(fit, map=FALSE, ci=TRUE) 
points(fit$most.prob.track, pch=20, col='black') 

