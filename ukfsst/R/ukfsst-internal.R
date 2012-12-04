.have.date<-function()"date"%in%.packages(TRUE)

`.First.lib` <-
function (lib, pkg) 
{
    library(kftrack)
    library(date)
    cat("\n\n")
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
    cat("Welcome to UKFSST - A package for improving tracking of tagged\n")
    cat("sea creatures by including Sea Surface Temperature (SST)\n")
    cat("in the Unscented Kalman Filter (UKF) based reconstruction model. \n\n")
    cat("What you typically want to do is the following sequence:\n\n")
    cat("1) Read in a track(\n")
    cat("2) Obtain a corresponding SST-field\n")
    cat("3) Reconstruct the track\n")
    cat("4) Investigate the fitted track by plots and summaries\n\n")
    cat("A slightly more detailed description of these four steps\n")
    cat("including what functions may be useful and how to call them is\n")
    cat("available via the function 'road.map'. If needed simply type: \n\n")
    cat("   road.map()\n\n")
    cat("and the text will be displayed.\n\n")
    cat("Check also the complete example in the 'blue.shark' dataset\n")
    cat("documentation by typing:\n\n")
    cat("  example(blue.shark)\n\n")
    cat("Thank you for your efforts in downloading and trying this\n")
    cat("software. If you encounter any bugs or have any questions -\n")
    cat("please don't hesitate to contact me:\n\n")
    cat("Anders Nielsen <anders.nielsen@hawaii.edu>\n\n")
    if (.Platform$OS.type == "windows") {
        cat(paste(rep("=", options()$width), collapse = ""), 
            "\n\n")
        cat("WINDOWS USERS: Please press (Ctrl+W) or use the menu \n")
        cat("(Misc->Buffered output) to switch the delayed output off.\n\n")
    }
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
}
`.generate.dat.file.kfsst` <-
function (data, file = "ukfsst.dat", fix.first = TRUE, fix.last = TRUE, 
    theta.active = c(u.active, v.active, D.active, bx.active, 
        by.active, bsst.active, sx.active, sy.active, ssst.active, 
        a0.active, b0.active, r.active), theta.init = c(u.init, 
        v.init, D.init, bx.init, by.init, bsst.init, sx.init, 
        sy.init, ssst.init, a0.init, b0.init, r.init), u.active = TRUE, 
    v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active = TRUE, 
    bsst.active = TRUE, sx.active = TRUE, sy.active = TRUE, ssst.active = TRUE, 
    a0.active = TRUE, b0.active = TRUE, r.active = FALSE, u.init = 0, 
    v.init = 0, D.init = 100, bx.init = 0, by.init = 0, bsst.init = 0, 
    sx.init = 0.5, sy.init = 1.5, ssst.init = 0.1, a0.init = 0.001, 
    b0.init = 0, r.init = 200, var.struct = "solstice", from.ystr = c(3, 
        6), from.dstr = c(7, 9), to.ystr = c(11, 14), to.dstr = c(15, 
        17)) 
{
    "%+%" <- function(s1, s2) paste(s1, s2, sep = "")
    tostr <- function(x) paste(as.numeric(x), collapse = "\t")
    var.flags <- switch(var.struct, uniform = 0, solstice = 1, 
        warning("No matching variance structure found"))
    header <- "#Auto generated data file from R-KFsst \n#" %+% 
        date() %+% "\n#\n" %+% "# Number of data points\n  " %+% 
        tostr(nrow(data)) %+% "\n#\n" %+% "# 1 if first point is true release position; 0 otherwise\n  " %+% 
        tostr(fix.first) %+% "\n#\n" %+% "# 1 if last point is true recapture position; 0 otherwise\n  " %+% 
        tostr(fix.last) %+% "\n#\n" %+% "# active parameters \n# u\tv\tD\tbx\tby\tbsst\tsx\tsy\tssst\ta0\tb0\n  " %+% 
        tostr(theta.active) %+% "\n#\n" %+% "# initial values \n# u\tv\tD\tbx\tby\tbsst\tsx\tsy\tssst\ta0\tb0\n  " %+% 
        tostr(theta.init) %+% "\n#\n" %+% "# latitude errors \n# Variance structure 0=uniform 1=solstice\n  " %+% 
        tostr(var.flags) %+% "\n#\n" %+% "# Positions \n# year\tmonth\tday\n  "
    cat(header, file = file)
    datrest <- data[, -c(1:3)]
    what <- matrix(rep(1:ncol(datrest), each = nrow(datrest)), 
        ncol = ncol(datrest))
    fixed <- matrix(0, nrow = nrow(datrest), ncol = ncol(datrest))
    if (fix.first) 
        fixed[1, 1:2] <- 1
    if (fix.last) 
        fixed[nrow(datrest), 1:2] <- 1
    number <- matrix(rep(1:nrow(datrest), ncol(datrest)), ncol = ncol(datrest))
    datawrite <- cbind(as.numeric(t(datrest)), as.numeric(t(what)), 
        as.numeric(t(fixed)), as.numeric(t(number)))
    datawrite <- datawrite[!is.na(datawrite[, 1]), ]
    write.table(data[, 3:1], file = file, row.names = FALSE, 
        col.names = FALSE, sep = "\t", eol = "\n  ", append = TRUE)
    cat("\n#nobs\n", nrow(datawrite), "\n  ", file = file, append = TRUE)
    write.table(datawrite, file = file, row.names = FALSE, col.names = FALSE, 
        sep = "\t\t", eol = "\n  ", append = TRUE)
    cat("\n#Number of sst data files\n", length(.sstFileVector), 
        "\n", file = file, append = TRUE)
    cat("#List of file names\n", file = file, append = TRUE)
    for (i in 1:length(.sstFileVector)) cat(.sstFileVector[i], 
        "\n", file = file, append = TRUE)
    bnvec <- basename(.sstFileVector)
    y1 <- as.numeric(substr(bnvec, from.ystr[1], from.ystr[2]))
    y2 <- as.numeric(substr(bnvec, to.ystr[1], to.ystr[2]))
    d1 <- as.numeric(substr(bnvec, from.dstr[1], from.dstr[2]))
    d2 <- as.numeric(substr(bnvec, to.dstr[1], to.dstr[2]))
    date1 <- (mdy.date(year = y1, month = 1, day = 1) + d1 - 
        1)
    date2 <- (mdy.date(year = y2, month = 1, day = 1) + d2 - 
        1)
    middate <- date.mdy(0.5 * (date1 + date2))
    datemat <- cbind(middate$year, middate$month, middate$day)
    cat("#Corresponding mid-dates (rounded upwards)\n", file = file, 
        append = TRUE)
    write.table(datemat, row.names = FALSE, col.names = FALSE, 
        quote = FALSE, file = file, append = TRUE)
    cat("\n", file = file, append = TRUE)
    class(header) <- "kfssthead"
    return(header)
}
`.myflush` <-
function () 
if (.Platform$OS.type == "windows") {
    flush.console()
} else {
    par(ask = FALSE)
}


.plot1<-function(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE,...){
  mypch<-c(ifelse(x$fix.first,6,4), rep(4,x$nobs-2), ifelse(x$fix.last,2,4))
  xlow<-x$most.prob.track[,"x"]-2*sqrt(x$var.most.prob.track[,1])
  xhig<-x$most.prob.track[,"x"]+2*sqrt(x$var.most.prob.track[,1])
  xrange<-.addrange(c(if(points){x$nominal.track[,"x"]}, if(pred){x$pred.track[,"x"]}, if(most){x$most.prob.track[,"x"]}, if(ci){c(xlow,xhig)}))
  plot(x$days.at.liberty, x$nominal.track[,"x"], ylab="Longitude", pch=mypch, 
       ylim=xrange, xlab="Days at liberty", type=ifelse(points,"b","n"), col=gray(.9))
  if(points){points(x$days.at.liberty, x$nominal.track[,"x"], pch=mypch)}
  if(pred){lines(x$days.at.liberty, x$pred.track[,"x"], col="green")}
  if(most){lines(x$days.at.liberty, x$most.prob.track[,"x"], col="blue")}
  if(ci){
    lines(x$days.at.liberty, xlow, col="blue", lty="dashed")
    lines(x$days.at.liberty, xhig, col="blue", lty="dashed")
  }
}

.plot2<-function(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE,...){
  mypch<-c(ifelse(x$fix.first,6,4), rep(4,x$nobs-2), ifelse(x$fix.last,2,4))
  ylow<-x$most.prob.track[,"y"]-2*sqrt(x$var.most.prob.track[,4])
  yhig<-x$most.prob.track[,"y"]+2*sqrt(x$var.most.prob.track[,4])
  yrange<-.addrange(c(if(points){x$nominal.track[,"y"]}, if(pred){x$pred.track[,"y"]}, if(most){x$most.prob.track[,"y"]}, if(ci){c(ylow,yhig)}))
  plot(x$days.at.liberty, x$nominal.track[,"y"], ylab="Latitude", pch=mypch, ylim=yrange, 
       las=3, xlab="", axes=FALSE, type=ifelse(points,"b","n"), col=gray(.9))
  if(points){points(x$days.at.liberty, x$nominal.track[,"y"], pch=mypch)}
  if(pred){lines(x$days.at.liberty, x$pred.track[,"y"], col="green")}
  if(most){lines(x$days.at.liberty, x$most.prob.track[,"y"], col="blue")}
  if(ci){
    lines(x$days.at.liberty, ylow, col="blue", lty="dashed")
    lines(x$days.at.liberty, yhig, col="blue", lty="dashed")
  }
}

.plot3<-function(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE, map=FALSE, res=3,...){
  gmt <- function(x1,x2,y1,y2,resolution=3) {
    ##############################################
    ## get mapping polygons from pscoast
    ##############################################
    read.ps.line<-function(txt){
      ##############################################
      ## Extract the stuff needed from a line 
      ##############################################  
      txt.split<-strsplit(txt, split=" ")[[1]]
      ret<-c(NA,NA)
      if(length(txt.split)==3){
        if(txt.split[3]%in%c("M","moveto","D")){ret<-as.numeric(txt.split[1:2])}
      }
      return(ret)
    }
    if(resolution<1 || resolution>5) stop("resolution from 1 (full) to 5 (crude)")
    res <- c("f","h","i","l","c")[resolution]
    filen <-tempfile("gmtmap")
    on.exit(unlink(filen))
    cmd <<- paste("pscoast -R",x1,"/",x2,"/",y1,"/",y2," -Jx2id -P -G0 -D",res," -X0 -Y0 >",filen,sep="")
    .kfsys(cmd)
    txt<-readLines(filen)
    mat<-matrix(unlist(lapply(txt, read.ps.line)),ncol=2, byrow=TRUE)
    for(i in 2:nrow(mat)){if(!is.na(mat[i,1])&!is.na(mat[i-1,1])){mat[i,]<-mat[i,]+mat[i-1,]}}  
    mat[,1]<-mat[,1]/600+x1
    mat[,2]<-mat[,2]/600+y1		
    return(mat)
  }
  
  mypch<-c(ifelse(x$fix.first,6,4), rep(4,x$nobs-2), ifelse(x$fix.last,2,4))
  xlow<-x$most.prob.track[,"x"]-2*sqrt(x$var.most.prob.track[,1])
  xhig<-x$most.prob.track[,"x"]+2*sqrt(x$var.most.prob.track[,1])
  ylow<-x$most.prob.track[,"y"]-2*sqrt(x$var.most.prob.track[,4])
  yhig<-x$most.prob.track[,"y"]+2*sqrt(x$var.most.prob.track[,4])
  xrange<-.addrange(c(if(points){x$nominal.track[,"x"]}, if(pred){x$pred.track[,"x"]}, if(most){x$most.prob.track[,"x"]}, if(ci){c(xlow,xhig)}))
  yrange<-.addrange(c(if(points){x$nominal.track[,"y"]}, if(pred){x$pred.track[,"y"]}, if(most){x$most.prob.track[,"y"]}, if(ci){c(ylow,yhig)}))
  plot(NA,NA, xlab="Longitude", ylab="Latitude", xlim=xrange, ylim=yrange, type="n")
  if(map){
    if(.gmtok()){
      xxyy<-par("usr"); x1<-xxyy[1]; x2<-xxyy[2]; y1<-xxyy[3]; y2<-xxyy[4];
      junk <- gmt(x1,x2,y1,y2,resolution=res)
      rect(x1,y1,x2,y2, col="lightblue")
      polygon(junk,border="darkgreen",col="darkgreen")
    }else{stop("GMT is not installed")}
  }
  if(points){
    par(new=TRUE)
    plot(x$nominal.track, pch=mypch, xlab="Longitude", ylab="Latitude", xlim=xrange, 
       ylim=yrange, type="b", col=gray(.9))
    points(x$nominal.track , pch=mypch)
  }
  if(ci){apply(cbind(x$var.most.prob.track, x$most.prob.track),1,.CI.reg)}
  if(pred){lines(x$pred.track, col="green")}
  if(most){lines(x$most.prob.track, col="blue")}
}


.CI.reg<-function (x, level = 0.95, npoints = 100, col="blue", border=0, density=20, lwd=0.1*par("lwd"), ...){ 
    #This function is partly stolen from the 'ellipse' package.
    t.quan<-sqrt(qchisq(level, 2))
    centre<-x[5:6]
    x<-matrix(x[1:4], 2,2)
    r <- x[1,2]
    scale <- sqrt(diag(x))
    if (scale[1] > 0){r <- r/scale[1]}
    if (scale[2] > 0){r <- r/scale[2]}
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    polygon(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1], t.quan * scale[2] *
            cos(a - d/2) + centre[2]), npoints, 2), 
            col=col,border=border, density=density, lwd=lwd,...)
  }

.addrange<-function(x, pct=.05){
  minx<-min(x, na.rm=TRUE); maxx<-max(x, na.rm=TRUE); dif<-maxx-minx
  c(minx,maxx)+c(-1,1)*dif*pct
}



`.plotsst` <-
function (x, ci = FALSE, points = TRUE, pred = TRUE, most = TRUE, 
    ...) 
{
    mypch <- c(ifelse(x$fix.first, 6, 4), rep(4, x$nobs - 2), 
        ifelse(x$fix.last, 2, 4))
    yrange <- .addrange(c(if (points) {
        x$SST[, "o"]
    }, if (pred) {
        x$SST[-1, "p"]
    }, if (most) {
        x$SST[, "smooth"]
    }))
    plot(x$days.at.liberty, x$SST[, "o"], ylab = "SST", pch = mypch, 
        ylim = yrange, las = 3, xlab = "", axes = FALSE, type = ifelse(points, 
            "b", "n"), col = gray(0.9))
    if (points) {
        points(x$days.at.liberty, x$SST[, "o"], pch = mypch)
    }
    if (pred) {
        lines(x$days.at.liberty[-1], x$SST[-1, "p"], col = "green")
    }
    if (most) {
        lines(x$days.at.liberty, x$SST[, "smooth"], col = "blue")
    }
}
`.read.output.kfsst` <-
function (data, fix.first = TRUE, fix.last = TRUE, theta.active = c(u.active, 
    v.active, D.active, bx.active, by.active, sx.active, sy.active, 
    a0.active, b0.active, r.active), theta.init = c(u.init, v.init, 
    D.init, bx.init, by.init, sx.init, sy.init, a0.init, b0.init, 
    r.init), u.active = TRUE, v.active = TRUE, D.active = TRUE, 
    bx.active = TRUE, by.active = TRUE, sx.active = TRUE, sy.active = TRUE, 
    a0.active = TRUE, b0.active = TRUE, r.active = FALSE, u.init = 0, 
    v.init = 0, D.init = 100, bx.init = 0, by.init = 0, sx.init = 0.5, 
    sy.init = 1.5, a0.init = 0.001, b0.init = 0, r.init = 200, 
    var.struct = "solstice", from.ystr = c(3, 6), from.dstr = c(7, 
        9), to.ystr = c(11, 14), to.dstr = c(15, 17)) 
{
    getpar <- function(what, file) {
        txt <- readLines(file)
        return(as.numeric(strsplit(txt[grep(what, txt) + 1], 
            split = " ")[[1]]))
    }
    getrep <- function(what, file) {
        txt <- readLines(file)
        return(as.numeric(strsplit(txt[grep(what, txt)], split = " ")[[1]][3]))
    }
    theta.names <- c("u", "v", "D", "bx", "by", "bsst", "sx", 
        "sy", "ssst", "radius")
    if (var.struct == "solstice") {
        theta.names <- c(theta.names, "a0", "b0")
    }
    if (file.access("ukfsst.par") != 0) {
        warning("File \"ukfsst.par\" not found (possibly because there was no solution to minimization problem)", 
            call. = FALSE)
        npar <- NA
        nlogL <- NA
        max.grad.comp <- NA
        estimates <- NA
    }
    else {
        tmp <- as.numeric(scan("ukfsst.par", what = "character", 
            16, quiet = TRUE)[c(6, 11, 16)])
        npar <- tmp[1]
        nlogL <- tmp[2]
        max.grad.comp <- tmp[3]
        estimates <- sapply(c("uu", "vv", "D", "bx", "by", "bsst", 
            "vx", "vy", "vsst", "radius"), getpar, file = "ukfsst.par")
        if (var.struct == "solstice") {
            estimates <- c(estimates, sapply(c("a0", "b0"), getpar, 
                file = "ukfsst.par"))
        }
        names(estimates) <- theta.names
    }
    if (file.access("ukfsst.std") != 0) {
        warning("File \"ukfsst.std\" not found (possibly the hessian was not estimated)", 
            call. = FALSE)
        std.dev <- NA
    }
    else {
        dat <- read.table("ukfsst.std", skip = 1)
        tmp <- dat[dat[, 2] %in% c("sduu", "sdvv", "sdD", "sdbx", 
            "sdby", "sdbsst", "sdvx", "sdvy", "sdvsst", "sdr"), 
            3:4]
        if (var.struct == "solstice") {
            if (theta.active[10]) {
                tmp <- rbind(tmp, dat[dat[, 2] == "a0", 3:4])
            }
            else {
                tmp <- rbind(tmp, c(0, 0))
            }
            if (theta.active[11]) {
                tmp <- rbind(tmp, dat[dat[, 2] == "b0", 3:4])
            }
            else {
                tmp <- rbind(tmp, c(0, 0))
            }
        }
        std.dev <- tmp[, 2]
        names(std.dev) <- paste("sd.", theta.names, sep = "")
    }
    if (file.access("mpt.out") != 0) {
        warning("File not found")
    }
    else {
        mpt <- read.table("mpt.out", header = FALSE)
        most.prob.track <- mpt[, 8:9]
        colnames(most.prob.track) <- c("x", "y")
        mptsst <- mpt[, 10]
        var.most.prob.track <- mpt[, 11:14]
    }
    if (file.access("pred.out") != 0) {
        warning("File not found")
    }
    else {
        pred <- read.table("pred.out", header = FALSE)
        pred.track <- pred[, 7:8]
        colnames(pred.track) <- c("x", "y")
        predsst <- pred[, 9]
    }
    if (file.access("time.out") != 0) {
        warning("File not found")
    }
    else {
        time <- read.table("time.out", header = FALSE)
        days.at.liberty <- time[, 4]
        date <- time[, 1:3]
        colnames(date) <- c("year", "month", "day")
    }
    nominal.track <- data[, 4:5]
    colnames(nominal.track) <- c("x", "y")
    nomsst <- data[, 6]
    SST <- cbind(o = nomsst, p = predsst, smooth = mptsst)
    mptfilename <- "mpt.out"
    return(list(npar = npar, nlogL = nlogL, max.grad.comp = max.grad.comp, 
        estimates = estimates, std.dev = std.dev, nominal.track = nominal.track, 
        pred.track = pred.track, most.prob.track = most.prob.track, 
        var.most.prob.track = var.most.prob.track, days.at.liberty = days.at.liberty, 
        date = date, SST = SST))
}
`.sys` <-
function (cmd) 
if (.Platform$OS.type == "windows") {
    shell(cmd, invisible = TRUE)
} else {
    system(cmd)
}
