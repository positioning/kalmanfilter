===========================================================#
# MTI script 1. Working with a fish in the Atlantic ocean
#===============================================================#

# set working directory
setwd('C:/Ben/MYPROJECTS/MTIGeolocationWorkshop')
basedir = getwd()

# read in libraries
library(analyzepsat)

# load some coastlines
data(ATL)

# use a deployment text file
taglocfile = 'deployments.txt'
xlsfile = paste(basedir,'/WorkshopDataSet/WorkshopReport.xls',sep="")

# read in data from fish report
psat = MWTextract('12345',  2007, xlsfile = xlsfile, taglocfile = taglocfile, delta = T, minmax = T, odbc = T) 

# prepare data for fitting
track = prepf(psat)
track[,6] = .fill.vals(track[,6])

# estimate track from light only
kfit = kftrack(track[,1:5])

# download SST
sstfolder = paste(basedir,'/SST',sep="")
get.reynolds(track, folder = sstfolder)
sstfolder = 'C:/Ben/MYPROJECTS/MTIGeolocationWorkshop/SST/sst_files'

track[,4] = track[,4]+360
ukfit = kfsst(track[,1:6], localsstfolder = sstfolder)

# get bathymetry
bath = get.bath.data(-100,0,0,55,res = 1, seaonly = F)

# adjustments. Cut off some unlikely data
track = prepf(psat, xmin = -80, xmax = -66, ymax = 50)
track[,6] = .fill.vals(track[,6])

# make sure longitudes match
track[,4] = track[,4]+360
ukfit = kfsst(track[,1:6], localsstfolder = sstfolder) 

# this works..
ukfit = kfsst(track[,1:6], bx.a=F, by.a=F, bsst.a=F, sy.init = 1000, sy.a=F, localsstfolder = sstfolder)
ukfit$data.name = 'blue2'

# make object for bathymetric correction
fmat = prepb(ukfit, prepf(psat, xmin = -80, xmax = -66, ymax = 50))
fmat[,8] = fmat[,8] -360

# do the bathymetric correction
btrack = make.btrack(fmat, bath)

# plots
x11()
par(mfrow=c(1,2))
plot.btrack(fmat)
plot.btrack(btrack, bathy = bath)

# fancy stuff
data(myramps)
?analyzepsat # copy and paste color ramps here
mymap = plotmap(260, 360, 0, 60, save = T, res = 3)
dev.off()

par(mar=c(7,4,4,7))
plot.btrack(btrack)
image(bath$lon+360, bath$lat, t(bath$data), col = bath.colors2(100), add=T, zlim = c(-9000,0))
contour(bath$lon+360, bath$lat, t(bath$data), col = 'white', lty = 1:2, add=T, levels = c(-200,-100))
plot.btrack(btrack, add=T)
polygon(mymap, col = 'grey90')
image(bath$lon+360, bath$lat, t(bath$data), col = grey.colors(100), add=T, zlim = c(1,5000))
.add.month.scale()#smallplot=c(.85,.87, .2,.8)
.add.bathy.scale (colscale = 2)
box(lwd=2)

# export for tagbase
btrack$tagID = 12345
write.csv(btrack, 'tag12345-ukfsst-bath.csv')

# shapefiles
track2shp(btrack, 'tag12345-track')
CI2shp(btrack, 'tag12345-CI')

# write a kml file
ktrack = data.frame(btrack[,1:3], hour=0, min=0, sec=0, btrack[,8:9])
tracks.kml(c('ktrack'))

save.image('atl-bsh.RData')
