library(trackit)
data(drifter); data(gmt3)
data(deltat); deltat$JDE = with(deltat,JDE(year,month,day))  # a fix for R 3.0+
source("http://geolocation.googlecode.com/svn/trunk/support/trackit/func_prepit.r")  # fix for path.packages issues for R.3.1+
source("http://geolocation.googlecode.com/svn/trunk/support/trackit/func_trackit.r") # fix for path.packages issues for R.3.1+
prep.track<-prepit(drifter, fix.first=c(360-161.45,22.85,2002,9,10,0,0,0), fix.last=c(360-159.87,21.95,2003,5,21,0,0,0), scan=FALSE)
fit<-trackit(prep.track)
sstfolder <- get.sst.from.server(drifter,180,210,10,40)
data(driftersst)
ptsst<-prepit(drifter, sst=driftersst, 
              fix.first=c(360-161.45,22.85,2002,9,10,0,0,0),
              fix.last=c(360-159.87,21.95,2003,5,21,0,0,0), scan=FALSE)
fitsst<-trackit(ptsst, bsst.ph=3, rad.ph=-1)  
pdf(height=6, width =6, file = 'trackit-exe-test.pdf')
plot(fit)
fitmap(fit, ci = T)
print(fit)
plot(fitsst)
fitmap(fitsst, ci = T)
print(fitsst)
dev.off()
