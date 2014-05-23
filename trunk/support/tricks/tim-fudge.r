library(analyzepsat)
setwd('c:/ukfsst/dan')
data(etopo2)
data(gshhs.NATL)
data(gmt3, package="trackit")
source("ben-frontmatter.r")
source("maxdepth.r")

plot.track <- function (fit, map, cex = 1.2, ci = F, bathy = NULL, add = F, 
    bathlevels = c(-100, -200), alpha = 0.15, bymonth = T, pch = 21, 
    bg = "yellow", points.only=F) 
{
    btrack <- data.frame(data.frame(date.mdy(fit$date))[,c(3,1,2)], fit$var.most.prob.track, 
          fit$most.prob.track)
    names(btrack) <- c("Year", "Month", "Day", "V11", "V12", "V21", "V22", "Lon_E", "Lat_N")
    len = length(btrack[, 1])
    xlims = c(min(btrack[, 8]) - 2, max(btrack[, 8] + 2)) 
    ylims = c(min(btrack[, 9]) - 2, max(btrack[, 9] + 2))
    if (add == F) 
        plot(map2$SP, col = "khaki", pbg = "azure2", xlim = xlims, 
            ylim = ylims, xaxs = "i", yaxs = "i", axes = TRUE)
    if (!is.null(bathy)) {
        bathy$lon = bathy$lon + 360
        contour(bathy$lon, bathy$lat, t(bathy$data), levels = bathlevels, 
            drawlabels = F, add = T, col = bath.colors(length(bathlevels)))
    }
    S = SpatialPointsDataFrame(btrack[, 8:9], btrack)
    if (btrack[1, 8] < 0) 
        S@coords[, 1] = S@data[, 8] = btrack[, 8] = S@coords[, 
            1] + 360
    proj4string(S) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
    if (ci) {
        sapply(1:len, function(i) makeCI(as.numeric(btrack[i, 
            4:9]), col = rgb(0.7, 0.7, 0.7, alpha = alpha), border = 0))
        plot(map2$SP, col = "khaki", pbg = "azure2", xaxs = "i", 
            yaxs = "i", axes = F, add = T)
    }
    if (points.only) {points(S, pch = pch, bg = bg)} else {
        points(S, pch = pch, bg = bg, typ = "o")
    }
    if (bymonth) 
        plot.by.month(S@data, cex = cex, pch = pch)
    # Start
    lines(btrack[1, 8], btrack[1, 9], typ = "p", pch = 25, col = 1, 
        bg = "red", cex = 1.8)
    # End
    lines(btrack[len, 8], btrack[len, 9], typ = "p", pch = 24, 
        col = 1, bg = "green", cex = 1.8)
    box(lwd = 2)
}

#==============================================================================#
# Bathymetric correction
#==============================================================================#
bcorr <- function(fit, dat, plot=T){
   fmat = make.fmat(fit, dat)
   fmat[,8] = fmat[,8] -360
   #fmat$max_depth = fmat$max_depth*-1
   btrack = make.btrack(fmat, bathy)
   if (plot) {
     par(mfrow=c(1,2))
     plot.btrack(fmat, bathy=bathy, ci =T);polygon(gmt3);title('KF fit')
     plot.btrack(btrack, bathy=bathy) 
     image(bathy[[1]]+360, bathy[[2]], t(bathy[[3]]), col = bath.colors(100), add=T, zlim = c(-10000,0))
     plot.btrack(btrack, ci=T, add=T)
     polygon(gmt3)
     title('Depth corrected')
   }
   return(btrack)
}
bcorr.trackit <- function(fit, track, plot=T){
   depth <- daily.depth(track)
   depth <- rbind(depth,depth)
   depth <- depth[order(depth$jday),]
   fmat <- data.frame(data.frame(date.mdy(fit$date))[,c(3,1,2)], fit$var.most.prob.track, 
          fit$most.prob.track, depth[,2], 0)
   names(fmat) <- c("Year", "Month", "Day", "V11", "V12", "V21", 
        "V22", "Lon_E", "Lat_N", "max_depth", "SST")
   fmat[,8] <- fmat[,8] -360
   fmat$max_depth <- fmat$max_depth*-1
   btrack <- make.btrack(fmat, bathy)
   if (plot) {
     par(mfrow=c(1,2))
     plot.btrack(fmat, bathy=bathy, ci =T);polygon(gmt3);title('KF fit')
     plot.btrack(btrack, bathy=bathy) 
     image(bathy[[1]]+360, bathy[[2]], t(bathy[[3]]), col = bath.colors(100), add=T, zlim = c(-10000,0))
     plot.btrack(btrack, ci=T, add=T)
     polygon(gmt3)
     title('Depth corrected')
   }
   return(btrack)
}
iplot.trackit <- function(fit, track, falsecolor=T, txt="", ...){
     plot.track(fit, bathy=bathy, ...)
     if (falsecolor) {
        image(bathy[[1]]+360, bathy[[2]], t(bathy[[3]]), col = bath.colors(100), add=T, zlim = c(-10000,0))
        plot.track(fit, add=T, ...)
     }
     polygon(gmt3)
     title(txt)
}

#==============================================================================#
# TrackIt example:
 load("A1504fitlight.Rdata")
 fit <- fitlight
 # Take a look 
 library(trackit); fitmap(fit); points(fit$most.prob.track, pch=20); 
 len <- length(fit$date)
 by <- 4
 text(fit$most.prob.track[seq(1,len, by),1]-0.1, fit$most.prob.track[seq(1,len, by),2],  
      label=seq(1,len, by), offset=2, cex=0.8)
 ### Cheat a little bit by moving some points
 idx <- c(53:65)
 fit$most.prob.track[idx,2] <- fit$most.prob.track[idx,2] - 2
 idx <- c(66:68)
 fit$most.prob.track[idx,2] <- fit$most.prob.track[idx,2] - 1
 fit$most.prob.track[idx,1] <- fit$most.prob.track[idx,1] - 1
 # Take another look 
 # library(trackit); fitmap(fit); points(fit$most.prob.track, pch=20); 
 month.colors <- make.month.colors(c(1:12))
 #iplot.trackit(fitlight, track)
 x11(); colorwheel()
 x11(); newtrack <- bcorr.trackit(fit, track)
#==============================================================================#

#==============================================================================#
# UKF Example:
# load("STM047.Rdata")
# dat <- read.csv("STM047.csv")
# newtrack <- bcorr(fit, dat)
#==============================================================================#