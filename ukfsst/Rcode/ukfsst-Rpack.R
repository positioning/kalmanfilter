.First.lib<-function(lib, pkg){
library(kftrack)
library(date)
cat('\n\n')
cat(paste(rep('=',options()$width), collapse=''),'\n\n')
cat('Welcome to UKFSST - A package for improving tracking of tagged\n')
cat('sea creatures by including Sea Surface Temperature (SST)\n') 
cat('in the Unscented Kalman Filter (UKF) based reconstruction model. \n\n')

cat('What you typically want to do is the following sequence:\n\n')
cat('1) Read in a track(\n') 
cat('2) Obtain a corresponding SST-field\n') 
cat('3) Reconstruct the track\n')
cat('4) Investigate the fitted track by plots and summaries\n\n')

cat('A slightly more detailed description of these four steps\n') 
cat('including what functions may be useful and how to call them is\n')
cat('available via the function \'road.map\'. If needed simply type: \n\n')
cat('   road.map()\n\n')
cat('and the text will be displayed.\n\n')    

cat('Check also the complete example in the \'blue.shark\' dataset\n')
cat('documentation by typing:\n\n')
cat('  example(blue.shark)\n\n')

cat('Thank you for your efforts in downloading and trying this\n')
cat('software. If you encounter any bugs or have any questions -\n') 
cat('please don\'t hesitate to contact me:\n\n')
cat('Anders Nielsen <anders.nielsen@hawaii.edu>\n\n')

if (.Platform$OS.type == "windows") {
  cat(paste(rep('=',options()$width), collapse=''),'\n\n')
  cat('WINDOWS USERS: Please press (Ctrl+W) or use the menu \n')
  cat('(Misc->Buffered output) to switch the delayed output off.\n\n')
}

cat(paste(rep('=',options()$width), collapse=''),'\n\n')

}

.sys<-function (cmd)
if (.Platform$OS.type == "windows") {
    shell(cmd, invisible=TRUE)
} else {
    system(cmd)
}

.myflush<-function ()
if (.Platform$OS.type == "windows") {
    flush.console()
} else {
  par(ask=FALSE)    
}

road.map<-function(){
#==================================================================#
cat(paste(rep('=',options()$width), collapse=''),'\n\n')

cat('What you typically want to do is the following sequence:\n\n')
cat('1) Read in a track:\n\n') 
cat('   Done via basic R-functions like \'read.table\' or \'read.csv\'\n')
cat('   For instance if your track file is organized like:\n\n') 
cat('   day month year  obsLon obsLat obsSst\n')
cat('    10     9 2002  198.04  23.09   30.6\n')
cat('    11     9 2002  198.11  50.50   30.7\n')
cat('    12     9 2002  197.65  45.50   30.6\n')
cat('                < and so on >\n\n')          
cat('   the command:\n\n')
cat('      track1 <- read.table(<filename>, head=TRUE)\n\n')
cat('   should read the data into a data frame named \'track1\'.\n')
cat('   Notice that if several tracks are in the same area and\n')
cat('   time period it is possible to read all of them before step 2),\n')
cat('   and then construct one SST-field covering all the tracks \n\n')
dummy<-readline('2) Obtain a corresponding SST-field: (press <return>)\n\n') 
cat('   A simple server has been set up to extract SST-fields that\n')
cat('   covers a track in simple way from within R\n')
cat('   To use this source type a command similar to:\n\n')
cat('      sst.path <- get.sst.from.server(track1)\n\n')
cat('   The path returned from the function is where all the SST\n') 
cat('   files are saved (useful in step 3)).\n') 
dummy<-readline('3) Reconstruct the track: (press <return>)\n\n')
cat('   To old users of the KFtrack package running this step\n')
cat('   should be very familiar. To apply the default full model to\n')
cat('   a track simply type: \n\n')
cat('      fit <- kfsst(track1)\n\n')
cat('   It is possible and in some cases necessary to customize the\n')
cat('   full model, for instance by choosing a simpler variance\n')
cat('   structure, or leaving out a bias term. For details on these\n')
cat('   and other options, please see the documentation (?kfsst).\n\n')
dummy<-readline('4) Investigate the fitted track: (press <return>)\n\n')

cat('   A basic plotting command is associated with the object\n')
cat('   returned from the function \'kfsst\', hence to see a\n')
cat('   plotted representation of the fit type: \n\n')
cat('      plot(fit)\n\n')
cat('   Often the raw geo locations are so poor that you may wish\n')
cat('   to leave them out of the plot to better zoom in on the\n')
cat('   reconstructed track. Also you may wish to get a visual\n')
cat('   representation of the uncertainties. Invoking the plot\n')
cat('   command with the following options will achieve that:\n\n')       
cat('      plot(fit, points=FALSE, ci=TRUE)\n\n')
cat('   It is also possible to have the track displayed on a map,\n') 
cat('   but it requires the free software GMT to be installed also.\n')
cat('   Read all about it in the documentation (?plot.kfsst).\n\n')

cat('   Besides the supplied plotting method it is possible to use\n')
cat('   the rich facilities in R to explore the track and the\n')
cat('   estimated model parameters. Everything needed from the\n')
cat('   estimated track is available via the returned object \'fit\'.\n')
cat('   Try typing the name of the fitted object:\n\n')
cat('      fit\n\n')
cat('   A list of the sub-objects in \'fit\' should now be displayed\n')
cat('   Along with a summary of the fit. To access a sub-object,\n')
cat('   for instance the negative log likelihood value type: \n\n')
cat('      fit$nlogL\n\n')
cat('   Notice the use of \'$\' as sub-object operator.\n\n')
       
#==================================================================#

cat(paste(rep('=',options()$width), collapse=''),'\n\n')
}

get.sst.from.server<-function(track, folder=tempdir(), 
  server='http://atlas.nmfs.hawaii.edu/cgi-bin/reynolds_extract.py')
{
  fl<-dir(folder)
  if(length(fl)!=0){
    folder<-paste(folder,'sst_temp', sep='/')
    dir.create(folder)
  }

  if(is.data.frame(track))track<-list(track)  

  minDate<-min(unlist(lapply(track,function(x)mdy.date(x[1,2],x[1,1],x[1,3]))))
  maxDate<-max(unlist(lapply(track,function(x)mdy.date(x[nrow(x),2],x[nrow(x),1],x[nrow(x),3]))))
  minDate<-minDate-10 # add a few days for good measure and
  maxDate<-maxDate+10 # because data source is 8-day files

  yrRan<-c(date.mdy(minDate)$year,date.mdy(maxDate)$year)
  daysIntoYearRan<-c(minDate-mdy.date(1,1,yrRan[1]),maxDate-mdy.date(1,1,yrRan[2]))

  minLon<-min(unlist(lapply(track, function(x)min(x[,4]))))-1
  maxLon<-max(unlist(lapply(track, function(x)max(x[,4]))))+1
  lonRan<-c(minLon,maxLon)

  minLat<-min(unlist(lapply(track, function(x)min(x[,5]))))-5
  maxLat<-max(unlist(lapply(track, function(x)max(x[,5]))))+5
  latRan<-c(minLat,maxLat)

  string<-''
  string<-paste(string,'?lon1=',lonRan[1],'&lon2=',lonRan[2],sep='')
  string<-paste(string,'&lat1=',latRan[1],'&lat2=',latRan[2],sep='')
  string<-paste(string,'&year1=',yrRan[1],'&day1=',daysIntoYearRan[1],sep='')
  string<-paste(string,'&year2=',yrRan[2],'&day2=',daysIntoYearRan[2],sep='')
  link<-paste(server,string, sep='')
  dest<-paste(folder,'temp.zip', sep='/')
  download.file(link, dest, mode='wb')
  if((version$major<=2)&(version$minor<=9)){
    .Internal(int.unzip(paste(folder,'temp.zip',sep='/'),NULL, folder))
  }else{
    unzip(paste(folder, "temp.zip", sep = "/"), exdir=folder)
  }
  #
  # May need some special treatment for windows here using 'zip.unpack()'
  unlink(paste(folder,'temp.zip', sep='/'))
  .sstFileVector<<-paste(folder,dir(folder), sep='/')

  for(f in .sstFileVector){
    dat<-read.table(f, head=FALSE)
    write.table(dat[complete.cases(dat),], col.names=FALSE, row.names=FALSE, quote=FALSE, file=f)
  }

  cat(paste(rep('=',options()$width), collapse=''),'\n\n')
  cat('Downloaded', length(dir(folder)), 'files to:\n\n  ', folder)
  cat('\n\nNow you most likely want to run a command like:\n\n   ')
  cat('fit <- kfsst(track)','\n\n')
  cat('to fit the full model (except for estimation of the radius).\n')
  cat('It is possible and in some cases necessary to customize the\n')
  cat('full model, for instance by choosing a simpler variance\n')
  cat('structure, estimating radius, or leaving out a bias term.\n')
  cat('For details on these and other options, please see the\n')
  cat('documentation (?kfsst).\n\n')
  cat(paste(rep('=',options()$width), collapse=''),'\n\n')

  return(folder)
}  

get.blended.sst<-function(track, folder=tempdir(), 
  server="http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=", 
  nday = "5day"){
  fl<-get.avhrr.sst(track=track, folder=folder, server=server, product="TBAssta", nday=nday)
  return(fl)
}  

get.avhrr.sst <- function (track, folder = tempdir(), 
                           server = "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=",
                           product = "TAGssta", nday = "8day", centertime="12") 
{
    ## product = "TGAssta", nday = "3day", centertime="12" (datesteps = 3,5,8)
    ## product= "TNAssta", nday = "1day", centertime="00" (datesteps = 1)
    ## product = "TN2ssta", nday="1day", centertime="00" (datesteps = 1)
    server <- paste(server, product, sep="")
    fmtDate <- function(date) {
        x <- date.mdy(date)
        paste(x$year, formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
    fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }
    fl <- dir(folder)
    if (length(fl) != 0) {
        folder <- paste(folder, "sst_temp", sep = "/")
        dir.create(folder)
    }
    if (is.data.frame(track)) 
        track <- list(track)
    minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1,2], x[1, 1], x[1, 3]))))
    maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x),2], x[nrow(x), 1], x[nrow(x), 3]))))
    minDate <- minDate - 10
    maxDate <- maxDate + 10
    datesteps <- seq(minDate, maxDate, by = ifelse(nday == "3day", 3, 8))
    minLon <- min(unlist(lapply(track, function(x) min(x[, 4])))) - 2
    maxLon <- max(unlist(lapply(track, function(x) max(x[, 4])))) + 2
    minLat <- min(unlist(lapply(track, function(x) min(x[, 5])))) - 2
    maxLat <- max(unlist(lapply(track, function(x) max(x[, 5])))) + 2
    op <- paste("&timePeriod=NDAY&centeredTime=~DATET", centertime, 
                "%3A00%3A00&maxLat=MAXLAT&minLon=MINLON&maxLon=MAXLON&minLat=MINLAT&filetype=.xyz", sep="")
    for (d in datesteps) {
        opt <- sub("NDAY", nday, op)
        opt <- sub("DATE", fmtDate(d), opt)
        opt <- sub("MAXLAT", maxLat, opt)
        opt <- sub("MINLON", minLon, opt)
        opt <- sub("MAXLON", maxLon, opt)
        opt <- sub("MINLAT", minLat, opt)
        link <- paste(server, opt, sep = "")
        y1 <- date.mdy(d - ifelse(nday == "3day", 1, 3))$year
        d1 <- (d - ifelse(nday == "3day", 1, 3)) - mdy.date(month = 1, day = 1, year = y1) + 1
        y2 <- date.mdy(d + ifelse(nday == "3day", 1, 4))$year
        d2 <- (d + ifelse(nday == "3day", 1, 4)) - mdy.date(month = 1, day = 1, year = y2) + 1
        filename <- paste(substring(product,2,3), y1, fmtDay(d1), "_", y2, fmtDay(d2), "_sst.xyz", sep = "")
        dest <- paste(folder, filename, sep = "/")
        download.file(link, dest, mode = "wb")
        tmp <- matrix(scan(dest), ncol = 3, byrow = TRUE)[, c(2, 1, 3)]
        write.table(tmp[complete.cases(tmp), ], file = dest, 
            quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    .sstFileVector <<- paste(folder, dir(folder), sep = "/")
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
    cat("Downloaded", length(dir(folder)), "files to:\n\n  ", folder)
    cat("\n\nNow you most likely want to run a command like:\n\n   ")
    cat("fit <- kfsst(track)", "\n\n")
    cat("to fit the full model (except for estimation of the radius).\n")
    cat("It is possible and in some cases necessary to customize the\n")
    cat("full model, for instance by choosing a simpler variance\n")
    cat("structure, estimating radius, or leaving out a bias term.\n")
    cat("For details on these and other options, please see the\n")
    cat("documentation (?kfsst).\n\n")
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
    return(folder)
}


kfsst<-function (data, fix.first = TRUE, fix.last = TRUE, 
    theta.active = c(u.active, v.active, D.active, bx.active, by.active, 
    bsst.active, sx.active, sy.active, ssst.active, a0.active, b0.active, r.active), 
    theta.init = c(u.init, v.init, D.init, bx.init, by.init, bsst.init, 
    sx.init, sy.init, ssst.init, a0.init, b0.init, r.init), u.active = TRUE, 
    v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active=TRUE, 
    bsst.active=TRUE, sx.active = TRUE, sy.active = TRUE, ssst.active=TRUE, 
    a0.active = TRUE, b0.active = TRUE, r.active=FALSE, u.init = 0, v.init = 0, 
    D.init = 100, bx.init = 0, by.init = 0, bsst.init=0, sx.init = 0.1, sy.init = 1.0, 
    ssst.init=.1, a0.init = 0.001, b0.init = 0, r.init=200, var.struct = "solstice", 
    save.dir = NULL, admb.string = "", from.ystr=c(3,6), from.dstr=c(7,9), 
    to.ystr=c(11,14), to.dstr=c(15,17), localsstfolder=NULL)
{
    if(!is.null(localsstfolder)){
      .sstFileVector<<-paste(localsstfolder,dir(localsstfolder), sep='/')
    }
    #cat(sst.file, '\n')
    olddir <- getwd()
    dirname <- ifelse(is.null(save.dir), "_kfsst_temp_", save.dir)
    dir.create(dirname)
    #file.copy(sst.file, paste(dirname,"sst.dat", sep='/'), TRUE)
    setwd(dirname)
    if(!is.null(save.dir)){
      unlink(c('mpt.out', 'pred.out', 'time.out', 'ukfsst.par', 'ukfsst.rep', 'ukfsst.std'))
    }
    header <- .generate.dat.file.kfsst(data, "ukfsst.dat", fix.first,
        fix.last, theta.active, theta.init, var.struct = var.struct)
    if (.Platform$OS.type == "windows") {
      error.code<-.sys(paste(.path.package("ukfsst"),"/admb/ukfsst.exe", " ", admb.string, sep = ""))
    }else {
      file.copy(paste(.path.package('ukfsst'),"admb/ukfsst",sep="/"),"ukfsst", TRUE)
      .sys("chmod u+x ukfsst")
      error.code<-.sys(paste("./ukfsst", " ", admb.string, sep = ""))
    }
    kf.o <- .read.output.kfsst(data, fix.first, fix.last, theta.active,
        theta.init, var.struct = var.struct)
    kf.o$nobs <- nrow(data)
    kf.o$header <- header
    kf.o$fix.first <- fix.first
    kf.o$fix.last <- fix.last
    kf.o$theta.active <- theta.active
    kf.o$theta.init <- theta.init
    kf.o$var.struct <- var.struct
    kf.o$call <- match.call()
    kf.o$data.name <- deparse(substitute(data))
    kf.o$error.code<-error.code
    setwd(olddir)
    if (is.null(save.dir)) {
        unlink(dirname, recursive = TRUE)
    }
    class(kf.o) <- "kfsst"
    return(kf.o)
}

.generate.dat.file.kfsst<-function (data, file = "ukfsst.dat", fix.first = TRUE, fix.last = TRUE,
    theta.active = c(u.active, v.active, D.active, bx.active, by.active, bsst.active, sx.active,
    sy.active, ssst.active, a0.active, b0.active, r.active), theta.init = c(u.init, v.init, D.init,
    bx.init, by.init, bsst.init, sx.init, sy.init, ssst.init, a0.init, b0.init, r.init), 
    u.active = TRUE, v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active=TRUE, 
    bsst.active=TRUE, sx.active = TRUE, sy.active = TRUE, ssst.active=TRUE, a0.active = TRUE, 
    b0.active = TRUE, r.active=FALSE, u.init = 0, v.init = 0, D.init = 100, bx.init = 0, 
    by.init = 0, bsst.init=0, sx.init = 0.5, sy.init = 1.5, ssst.init=.1, a0.init = 0.001, 
    b0.init = 0, r.init=200, var.struct = "solstice", from.ystr=c(3,6), from.dstr=c(7,9), 
    to.ystr=c(11,14), to.dstr=c(15,17))
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

    datrest<-data[,-c(1:3)]
    what<-matrix(rep(1:ncol(datrest), each=nrow(datrest)), ncol=ncol(datrest))
    fixed<-matrix(0, nrow=nrow(datrest), ncol=ncol(datrest))
    if(fix.first)fixed[1,1:2]<-1 
    if(fix.last)fixed[nrow(datrest),1:2]<-1 
    number<-matrix(rep(1:nrow(datrest), ncol(datrest)), ncol=ncol(datrest))
    datawrite<-cbind(as.numeric(t(datrest)),as.numeric(t(what)),as.numeric(t(fixed)),as.numeric(t(number)))
    datawrite<-datawrite[!is.na(datawrite[,1]),]
    write.table(data[,3:1], file = file, row.names = FALSE, col.names = FALSE, sep = "\t", eol = "\n  ", append = TRUE)
    cat("\n#nobs\n",nrow(datawrite),'\n  ', file = file, append = TRUE)
    write.table(datawrite, file = file, row.names = FALSE, col.names = FALSE, sep = "\t\t", eol = "\n  ", append = TRUE)
    
    cat("\n#Number of sst data files\n",length(.sstFileVector),"\n", file = file, append = TRUE)
    cat("#List of file names\n", file = file, append = TRUE)
    for(i in 1:length(.sstFileVector))cat(.sstFileVector[i],'\n',  file = file, append = TRUE)
    bnvec<-basename(.sstFileVector)
    y1<-as.numeric(substr(bnvec,from.ystr[1], from.ystr[2]))
    y2<-as.numeric(substr(bnvec,to.ystr[1], to.ystr[2]))
    d1<-as.numeric(substr(bnvec,from.dstr[1], from.dstr[2]))
    d2<-as.numeric(substr(bnvec,to.dstr[1], to.dstr[2]))
    date1<-(mdy.date(year=y1, month=1, day=1)+d1-1)
    date2<-(mdy.date(year=y2, month=1, day=1)+d2-1)
    middate<-date.mdy(0.5*(date1+date2))
    datemat<-cbind(middate$year,middate$month,middate$day)
    cat("#Corresponding mid-dates (rounded upwards)\n", file = file, append = TRUE)
    write.table(datemat, row.names=FALSE, col.names=FALSE, quote=FALSE, file = file, append = TRUE)
    cat("\n", file = file, append = TRUE)
    class(header) <- "kfssthead"
    return(header)
}

.read.output.kfsst<-function (data, fix.first = TRUE, fix.last = TRUE, theta.active = c(u.active,
    v.active, D.active, bx.active, by.active, sx.active, sy.active, a0.active, b0.active, r.active), 
    theta.init = c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, b0.init, 
    r.init), u.active = TRUE, v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active = TRUE,
    sx.active = TRUE, sy.active = TRUE, a0.active = TRUE, b0.active = TRUE, r.active=FALSE,
    u.init = 0, v.init = 0, D.init = 100, bx.init = 0, by.init = 0, sx.init = 0.5, sy.init = 1.5, 
    a0.init = 0.001, b0.init = 0, r.init=200, var.struct = "solstice", from.ystr=c(3,6), 
    from.dstr=c(7,9), to.ystr=c(11,14), to.dstr=c(15,17))
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
    theta.names <- c("u", "v", "D", "bx", "by", "bsst", "sx", "sy", "ssst", "radius")
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
        estimates <- sapply(c("uu", "vv", "D", "bx", "by", "bsst", "vx",
            "vy", "vsst", "radius"), getpar, file = "ukfsst.par")
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
            "sdby", "sdbsst", "sdvx", "sdvy", "sdvsst", "sdr"), 3:4]
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
      mpt<-read.table('mpt.out', header=FALSE)
      most.prob.track<-mpt[,8:9]
      colnames(most.prob.track)<-c("x","y")
      mptsst<-mpt[,10]
      var.most.prob.track<-mpt[,11:14]
    }
    if (file.access("pred.out") != 0) {
        warning("File not found")
    }
    else {
      pred<-read.table('pred.out', header=FALSE)
      pred.track<-pred[,7:8]
      colnames(pred.track)<-c("x","y")
      predsst<-pred[,9]
    }
    if (file.access("time.out") != 0) {
        warning("File not found")
    }
    else {
      time<-read.table('time.out', header=FALSE)
      days.at.liberty<-time[,4]
      date<-time[,1:3]
      colnames(date)<-c("year", "month", "day")
    }
    nominal.track<-data[,4:5]
    colnames(nominal.track)<-c("x","y")
    nomsst<-data[,6]
    SST<- cbind(o=nomsst, p=predsst, smooth=mptsst)
    mptfilename <- "mpt.out"
    return(list(npar = npar, nlogL = nlogL, max.grad.comp = max.grad.comp,
        estimates = estimates, std.dev = std.dev, nominal.track = nominal.track,
        pred.track = pred.track, most.prob.track = most.prob.track,
        var.most.prob.track = var.most.prob.track, days.at.liberty = days.at.liberty,
        date = date, SST=SST))
}

print.kfsst<-function (x, ...)
{
    headvec <- strsplit(x$header, split = "\n")[[1]]
    "%+%" <- function(s1, s2) paste(s1, s2, sep = "")
    out <- "\n\n#R-KFtrack fit\n" %+% headvec[2] %+% "\n" %+%
        "#Number of observations: " %+% x$nobs %+% "\n" %+% "#Negative log likelihood: " %+%
        x$nlog %+% "\n" %+% "#The convergence criteria was " %+%
        ifelse(abs(x$max.grad.comp) > 1e-04, "NOT ", "") %+%
        "met\n\n"
    cat(out)
    if (abs(x$max.grad.comp) < 1e-04) {
        cat("Parameters:\n")
        print(rbind("Estimates:" = x$estimates, "Std. dev.:" = x$std.dev))
        cat("\nThis object contains the following sub-items:\n")
        print(names(x))
    }
}

.plotsst<-function (x, ci = FALSE, points = TRUE, pred = TRUE, most = TRUE,...)
{
  mypch <- c(ifelse(x$fix.first, 6, 4), rep(4, x$nobs - 2), ifelse(x$fix.last,
      2, 4))

  yrange <- .addrange(c(if (points) {
       x$SST[,"o"]
  }, if (pred) {
      x$SST[-1,"p"]
  }, if (most) {
      x$SST[,"smooth"]
  }))
  plot(x$days.at.liberty, x$SST[,"o"], ylab = "SST",
      pch = mypch, ylim = yrange, las = 3, xlab = "", axes = FALSE,
      type = ifelse(points, "b", "n"), col = gray(0.9))
  if (points) {
      points(x$days.at.liberty, x$SST[,"o"], pch = mypch)
  }
  if (pred) {
      lines(x$days.at.liberty[-1], x$SST[-1,"p"], col = "green")
  }
  if (most) {
      lines(x$days.at.liberty, x$SST[,"smooth"], col = "blue")
  }
}

plot.kfsst<-function (x, ci = FALSE, points = TRUE, pred = TRUE, most = TRUE, gmt=FALSE,...)
{
    par(mar = c(4, 4, 5, 1) + 0.1)
    layout(matrix(c(1, 4, 2, 4, 3, 4), ncol=2, nrow=3, byrow = TRUE))
    .plot1(x = x, ci = ci, points = points, pred = pred, most = most,...)
    xp <- par("xaxp")
    xcut <- seq(xp[1], xp[2], length = xp[3] + 1)
    par(mar = c(6, 4, 3, 1) + 0.1)
    .plot2(x = x, ci = ci, points = points, pred = pred, most = most,...)
    axis(1); axis(2); box();
    .plotsst(x = x, ci = ci, points = points, pred = pred, most = most,...)
    if (.have.date()) {
        firstdate <- mdy.date(x$date[1, "month"], x$date[1, "day"],
            x$date[1, "year"])
        axis(2)
        axis(1, at = xcut, labels = paste(firstdate + xcut),
            las = 2)
        box()
        mtext("Date", side = 1, line = 5, cex = par("cex"))
    }
    else {
        axis(1)
        axis(2)
        box()
        mtext("Days at liberty", side = 1, line = 3, cex = par("cex"))
    }
    par(mar = c(4, 4, 5, 1) + 0.1)
    .plot3(x = x, ci = ci, points = points, pred = pred, most = most,...)
    title(paste("Estimated track of", x$data.name), outer = TRUE,
        line = -1.5)
    if(gmt){
      .gmt.plot(x, map.width = 5, ci = ci, points = points, pred = pred, most = most, ...)
    }
}


"plotmap" <- function(x1, x2, y1, y2, resolution=3,
               grid=FALSE, add=FALSE, save=FALSE,
               landcolor="darkgreen", seacolor="lightblue", zoom=FALSE) {
###============================================================
###============================================================
  gmt <- function(x1,x2,y1,y2,resolution=3) {
    read.ps.line<-function(txt){
      txt.split<-strsplit(txt, split=" ")[[1]]
      ret<-c(NA,NA)
      if(length(txt.split)==3){
        if(txt.split[3]%in%c("M","moveto","D")){
          ret<-as.numeric(txt.split[1:2])
        }
      }
      return(ret)
    }
    if(resolution<1 || resolution>5)
        stop("resolution from 1 (full) to 5(crude)")
    res<-c("f","h","i","l","c")[resolution]
    filen <- tempfile("gmtmap")
    on.exit(unlink(c(filen,".gmtcommands4")))
    cmd<-paste("pscoast -R",x1,"/",x2,"/",y1,"/",y2,
               " -Jx2id -P -G -D",res,
               " -X0 -Y0 >",filen,sep="")
    .sys(cmd)
    txt<-readLines(filen)
    mat<-matrix(unlist(lapply(txt, read.ps.line)),
                ncol=2, byrow=TRUE)
    for(i in 2:nrow(mat)){
      if(!is.na(mat[i,1])&!is.na(mat[i-1,1]))
        mat[i,]<-mat[i,]+mat[i-1,]
    }
    maxx<-max(mat[,1], na.rm=TRUE)
    maxy<-max(mat[,2], na.rm=TRUE)

    mat[,1]<-mat[,1]/600+x1
    mat[,2]<-mat[,2]/600+y1
    return(mat)
  }
  junk <- gmt(x1,x2,y1,y2,resolution=resolution)
  if(!add) {
    plot(c(x1,x2),c(y1,y2),type='n',ylab="",xlab="",
       xaxs="i", yaxs="i")
    rect(x1,y1,x2,y2, col=seacolor)
    if(grid) {
      axis(1,tck=1)
      axis(2,tck=1)
    }
  }
  polygon(junk,border=landcolor,col=landcolor)
  if(zoom){
    ret<-locator(2)
    if(length(ret$x)<2){
      zoom<-FALSE
    }else{
      x1<-min(ret$x);x2<-max(ret$x);y1<-min(ret$y);y2<-max(ret$y);
    }
    plotmap(x1,x2,y1,y2,resolution,grid,add,save,landcolor,seacolor,zoom)    
  }
  if(save) {
    dimnames(junk)[[2]] <- c("longitude","latitude")
    return(junk)
  }
}

blue.shark<-structure(list(day = c(11, 16, 18, 22, 24, 26, 28, 30, 2, 4,
6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 2, 4, 6, 8,
10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 2, 4, 6, 8, 10, 12,
22), month = c(4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
6, 7, 7, 7, 7, 7, 7, 7), year = c(2001, 2001, 2001, 2001, 2001,
2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
2001, 2001, 2001, 2001, 2001, 2001, 2001), Long = c(201.722,
201.19, 202.95, 199.11, 200.64, 197.81, 203.1, 203.29, 194.73,
198.68, 206.89, 204.11, 202.84, 203.83, 207.46, 200.35, 212.49,
206.4, 201.68, 202.86, 206.91, 203.6, 208.18, 205.17, 205.76,
208.48, 209.96, 206.31, 206.8, 209.28, 211.4, 210.14, 210.38,
213.5, 212.25, 214.99, 212.73, 224.72, 213.33, 224.06, 215.53,
218.13, 213.22, 211.8, 216.337), Lat = c(18.875, 24.15, 12.89,
28.79, 22.6, 19.2, 26.9, 28.52, 7.59, 22.95, 18.22, 32.63, 23.91,
20.93, 31.11, 13.15, 23.52, 35.27, 23.84, 29.6, 21.77, 35.07,
36.02, 37.85, 35.31, 39.13, 34.77, 34.55, 37.01, 37.3, 39.95,
35.44, 34.29, 37.89, 35.69, 37.96, 35.21, 27.37, 34.68, 21.03,
27.88, 28.11, 34.97, 29.07, 29.404), sst = c(24.73, 24.37, 24.73,
24.37, 23.83, 23.39, 22.77, 22.95, 22.59, 23.12, 22.77, 21.54,
21.02, 21.19, 21.02, 21.36, 20.32, 19.8, 18.77, 18.6, 18.26,
17.58, 17.07, 16.74, 16.57, 15.98, 16.24, 14.91, 15.98, 17.24,
17.41, 16.57, 17.08, 18.09, 16.74, 15.07, 17.58, 16.57, 18.26,
17.41, 20.84, 20.23, 21.27, 20.84, 22.42)), .Names = c("day",
"month", "year", "Long", "Lat", "sst"), class = "data.frame", row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
"25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
"36", "37", "38", "39", "40", "41", "42", "43", "44", "45"))
