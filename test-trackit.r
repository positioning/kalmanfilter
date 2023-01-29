###=============================================================================================
### Trackit 
###=============================================================================================
library(trackit)
### A fix for R 3.0+
data(deltat); deltat$JDE = with(deltat,JDE(year,month,day))
data(gmt3)
### Fit to data
data(drifter)
prep.track<-prepit(drifter, fix.first=c(360-161.45,22.85,2002,9,10,0,0,0), fix.last=c(360-159.87,21.95,2003,5,21,0,0,0), scan=FALSE)
fit<-trackit(prep.track)
sstfolder <- get.sst.from.server(drifter,180,210,10,40)
data(driftersst)
ptsst<-prepit(drifter, sst=driftersst, 
              fix.first=c(360-161.45,22.85,2002,9,10,0,0,0),
              fix.last=c(360-159.87,21.95,2003,5,21,0,0,0), scan=FALSE)
fitsst<-trackit(ptsst, bsst.ph=3, rad.ph=-1)  
### Plotting to file
pdf(height=6, width =6, file = 'trackit-exe-test.pdf')
plot.trackit(fit)
fitmap(fit, ci = T)
print.trackit(fit)
plot.trackit(fitsst)
fitmap(fitsst, ci = T)
print.trackit(fitsst)
dev.off()
