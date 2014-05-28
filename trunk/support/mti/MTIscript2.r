#===============================================================#
# MTI script 2:other things you can do with analyzepsat
#===============================================================#
# set working directory
setwd('C:/Ben/MYPROJECTS/MTIGeolocationWorkshop')
basedir = getwd()

# read in libraries
library(analyzepsat)

# load
load('atl-bsh.RData')

# Plot temprature and depth

par(mfrow=c(2,1))
plot.TZ(psat,1, dcol = 4, axes = T, trend=T)
plot.TZ(psat,2, trend = T)

par(mar=c(6,4,4,2))
plotTZprofile(psat, legend = F)
plotTZprofile(psat, legend = T)

# make an atlas of everything!
source('psat2atlas.r')

psat2atlas(psat, btrack, bathy = bath)

#===============================================================#
# get a utilization distribution
kd = track2KD(btrack, range.x = c(-80, -65), range.y = c(15, 45))
ud = kern2UD(kd)
image.plot(ud, col = rev(jet.colors(100)))

attributes(ud)$xll = attributes(ud)$xll +360

plot.btrack(btrack)
image(ud, add=T, col = rev(jet.colors(100)), zlim = c(0,.95))
plot.btrack(btrack, add=T, bymonth = F)

image.plot(zlim = c(5,100), legend.shrink=.5, smallplot = c(.85,.87,.25,.75), add=T, col = rev(jet.colors(100)), legend.only=T, legend.args=list(text = "%", side = 3, line = .5, las=2), horizontal =F)

# export UD for GIS
export.asc(ud, 'sharkUD.asc')

# get area of each UD level
UD.area(list(tag12345 = ud), levels = seq(0.2, 0.95, by = 0.05))

#===============================================================#
# vertical habitat envelopes
mtz = merge.tz(list('12345' = btrack), list('12345' = psat), tagID = NULL)
mhv = merge.hv(btrack, mtz)
vhe = make.env(mhv, fixz = F, mbz = c(-500), mbt = c(30), log = T, mmar = c(4, 6, 2, 6), mcex = 1.2, plot = T)

# subset for a few months of interest
ss = mhv$Month >7 & mhv$Month < 11

vhe.summer = make.env(mhv[ss,], fixz = F, mbz = c(-500), mbt = c(30), log = T, mmar = c(4, 6, 2, 6), mcex = 1.2, plot = T)

vhe.winter = make.env(mhv[ss==F,], fixz = F, mbz = c(-500), mbt = c(30), log = T, mmar = c(4, 6, 2, 6), mcex = 1.2, plot = T)

# output a pdf of individual months
pdf(height = 8, width = 15,  file = 'shark h-v hab.pdf')
par(mfrow = c(1,2))

for(i in unique(mhv$Month)){
# x11(width = 10, height = 5)
 ss = mhv$Month == i
 make.env(mhv[ss,], fixz = F, mbz = c(-500), mbt = c(30), log = T, mmar = c(4, 6, 2, 6), mcex = 1.2, plot = T)
 title(month.name[i])
 ss2 = btrack$Month==i
 plot.btrack(btrack[ss2,], bathy=bath)
}
dev.off()
