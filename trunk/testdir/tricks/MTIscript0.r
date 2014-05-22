#-----------------------------------------------------------------------------#
#       To make the color ramps used in this package, use the following
#-----------------------------------------------------------------------------#
# Mimics the jet colors from matlab. Good for SST
     jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
jet.colors
     
# nice blue cascade for bathymetry
     bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))
     bath.colors
# nice earthy colors for land
     land.colors<-  colorRampPalette(c("darkgreen", "greenyellow","burlywood4"))
     
# Colors to use in the plot.by.month (and related) functions
# Also included via data(month.colors)
     month.colors=cbind(c(8:12,1:7),
       c(rgb(115/255,0,76/255),
         rgb(0,76/255,115/255),
         rgb(0,92/255,230/255),
         rgb(115/255,223/255,1),
         rgb(190/255,232/255,1),
         rgb(1,1,190/255),
         rgb(230/255,230/255,0),
         rgb(1,170/255,0),
         rgb(1,120/255,0),
         rgb(1,0,197/255),
         rgb(1,0,0),
         rgb(168/255,0,0)
       )
     )
     
# Also included via data(monames)
     monames=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec')

#=============================================================================#
# Example block #1 KF examples
#=============================================================================#
setwd('C:\\Ben\\MYPROJECTS\\MTIGeolocationWorkshop')
basedir = getwd()

# loads all libraries
library(analyzepsat)

# allows kftrack to use GMT
.gmtok=function () 
{
    .testgmt <- tempfile("testgmt")
    on.exit(unlink(.testgmt))
    return(.kfsys(paste("gmtdefaults -L 1>", .testgmt)) == 0)
}

# Load the blue shark data
data(blue.shark)

# standard kftrack fucntion
kfit = kftrack(blue.shark[,1:5])

# ignore the longitude and latitude observations and do not estimate D
kfit0 = kftrack(blue.shark[,1:5], D.active = F, sx.init=1000, sy.init=1000, sy.a=F, sx.a =F, bx.a = F, by.a = F)

# ignore the longitude and latitude observations and do not estimate D, use a different value for D
kfit01 = kftrack(blue.shark[,1:5], D.init= 1000, D.active = F, sx.init=1000, sy.init=1000, sy.a=F, sx.a =F, bx.a = F, by.a = F)

# Plot all 3 results
plot(kfit0, map =T, ci = T, points = F)
plot(kfit01, map =T, ci = T, points = F)
x11()
plot(kfit, map =F, ci = T, points = T)

# plot some of the variance 
dateplot = mdy.date(kfit$date[,2],kfit$date[,3], kfit$date[,1])

plot(sqrt(kfit0$var.most.prob.track[,4]), ylab = expression(sigma), typ = 'o', pch = 19, xlab = 'Days at Liberty', ylim =c(0,5))
lines(sqrt(kfit01$var.most.prob.track[,4]), ylab = expression(sigma), typ = 'o', pch = 19, col = 2)
lines(sqrt(kfit$var.most.prob.track[,4]), typ = 'o', pch = 19, col=4)
legend(20,3, c('no obs D=100','no obs D=1000','blue shark'), pch=19, col=c(1,2,4), lty=1)

par(mar=c(6,4,2,2))
par(cex.axis=1.2, cex.lab=1.2)
plot(dateplot, sqrt(kfit$var.most.prob.track[,4]), ylab = expression(sigma), type = 'o', pch = 19, las = 2, xlab = "")
title('Solstice variance')

# Write some output
write.csv(rbind(kfit0$estimates, kfit0$std,kfit$estimates, kfit$std), file = 'kfit0_est.csv')
write.csv(rbind(), file = 'kfit_est.csv')



#=============================================================================#
# UKF examples
#=============================================================================#
# setwd('/media/WDPASSPORT/Ben/PROJECTS/Classwork/2010/TaggingCourse/lecture/')
# Get some SST

sharksst ='C:/Ben/MYPROJECTS/MTIGeolocationWorkshop/SST/bsh_pac'

get.reynolds(blue.shark, folder = sharksst)  #

sharksst ='C:/Ben/MYPROJECTS/MTIGeolocationWorkshop/SST/bsh_pac/sst_files/'

# make the file vector for ukfsst
 # .sstFileVector = list.files(paste(getwd(),'/SST/',sep=""), full.names=T, pattern='.xyz')

# run it!
ukfit1 = kfsst(blue.shark, localsstfolder = sharksst)
ukfit2 = kfsst(blue.shark, bx.a=F, by.a=F, bsst.a=F)

# Plot the results
plot(ukfit1, map =T, ci = T, points = T)
plot(ukfit2, map =T, ci = T, points = T)


# output the results
write.csv(rbind(ukfit$estimates, ukfit$std), file = 'ukfit_est.csv')

# Plot three examples together
par(mar=c(6,4,2,2))
par(cex.axis=1.2, cex.lab=1.2)
plot(dateplot, sqrt(kfit$var.most.prob.track[,4]), ylab = expression(sigma), typ = 'o', pch = 19, las = 2, xlab = "")
lines(dateplot,sqrt(ukfit2$var.most.prob.track[,4]), typ = 'o', pch = 19, col=2)
legend('topright', c('kftrack','ukfsst'), pch=19, col=c(1,2), lty=1)

#=============================================================================#
# Compare variance models
#=============================================================================#

# run ukfsst again with a different variance structure
uk1 = kfsst(blue.shark, D.a = F, bx.a = F, by.a = F, var.struct = "uniform")
# uk2 = kfsst(blue.shark, var.struct = "daily")

# Plot the results

.plot3(uk1, map = T, ci = T)
lines(uk1$most.prob.track, pch = 19, typ = 'o', col = 4)
lines(ukfit2$most.prob.track, pch = 19, typ = 'o', col = 2)

