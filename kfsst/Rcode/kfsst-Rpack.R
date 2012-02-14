.First.lib<-function(lib, pkg){
library(kftrack)
library(date)
library(locfit, warn=FALSE) 
cat('\n\n')
cat(paste(rep('=',options()$width), collapse=''),'\n\n')
cat('Welcome to KFSST - A package for improving tracking of tagged\n')
cat('sea creatures by including Sea Surface Temperature (SST)\n') 
cat('in the Kalman Filter (KF) based reconstruction model. \n\n')

cat('What you typically want to do is the following sequence:\n\n')
cat('1) Read in one or more track(s)\n') 
cat('2) Obtain a corresponding SST-field\n') 
cat('3) Smooth the SST-field and save it in suitable format\n') 
cat('4) Reconstruct the track(s)\n')
cat('5) Investigate the fitted track by plots and summaries\n\n')

cat('A slightly more detailed description of these five steps\n') 
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
    
}


road.map<-function(){
#==================================================================#
cat(paste(rep('=',options()$width), collapse=''),'\n\n')

cat('What you typically want to do is the following sequence:\n\n')
cat('1) Read in one or more track(s):\n\n') 
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
cat('   Notice that if several tracks are in the same region and\n')
cat('   time period it is better to read all of them before step 2),\n')
cat('   because then one SST-field covering all tracks can be \n')
cat('   constructed.\n\n')
dummy<-readline('2) Obtain a corresponding SST-field: (press <return>)\n\n') 
cat('   A simple server has been set up to extract SST-fields that\n')
cat('   covers a track (or a set of tracks) in simple way from\n')
cat('   within R. To use this source type a command similar to:\n\n')
cat('      sst.path <- get.sst.from.server(track1)\n\n')
cat('   Notice \'track1\' can be replaced by a list of tracks like:\n\n') 
cat('      sst.path <- get.sst.from.server(list(track1, track2))\n\n')
cat('   to obtain an SST-field covering a set of tracks.\n\n')
cat('   The path returned from the function is where all the SST\n') 
cat('   files are saved (useful in step 3)). To use a different\n') 
cat('   SST-source please see documentation for the function\n') 
cat('   \'write.sst.field\'.\n\n')
dummy<-readline('3) Smooth the SST-field and save it in suitable format: (press <return>)\n\n') 
cat('   A function is supplied to automate the process of reading\n') 
cat('   smoothing, and saving the SST-data in the right format. Its\n') 
cat('   basic use is:\n\n')
cat('      sst.file <- write.sst.field(sst.path)\n\n')
cat('   but it comes with a lot of options to adjust degree of \n')
cat('   smoothing and resolution of field representation. Please \n')
cat('   consult the documentation to see all the details.\n\n')
cat('   The filename returned (including absolute path) is where\n')
cat('   the smooth representation of the SST-field is saved.\n\n')
dummy<-readline('4) Reconstruct the track(s): (press <return>)\n\n')
cat('   To old users of the KFtrack package running this step\n')
cat('   should be very familiar. To apply the default full model to\n')
cat('   a track simply type: \n\n')
cat('      fit <- kfsst(track1, sst.file)\n\n')
cat('   It is possible and in some cases necessary to customize the\n')
cat('   full model, for instance by choosing a simpler variance\n')
cat('   structure, or leaving out a bias term. For details on these\n')
cat('   and other options, please see the documentation (?kfsst).\n\n')
dummy<-readline('5) Investigate the fitted track(s): (press <return>)\n\n')

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

  minLat<-min(unlist(lapply(track, function(x)min(x[,5]))))-1
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
  ## fixed for R 2.11 MDSumner 2010-05-17
  unzip(paste(folder, "temp.zip", sep = "/"),NULL, exdir = folder)
  unlink(paste(folder, "temp.zip", sep = "/"))
  ##.Internal(int.unzip(paste(folder,'temp.zip',sep='/'),NULL, folder))

  cat(paste(rep('=',options()$width), collapse=''),'\n\n')
  cat('Downloaded', length(dir(folder)), 'files to:\n\n  ', folder)
  cat('\n\nNow you most likely want to run a command like:\n\n   ')
  cat(paste('sst.file <- write.sst.field(datadir=\'',folder,'\', filename=\'sst.dat\')',sep=''),'\n\n')
  cat('to generate the smooth representation of the SST-field and its\n')
  cat('derivatives and save it in the file called \'sst.dat\'\n\n') 
  cat('Please consult the documentation (by typing \'?write.sst.field\') \n')
  cat('for a description of the different options including degree of\n')
  cat('smoothing and resolution of representation.\n\n')     
  cat(paste(rep('=',options()$width), collapse=''),'\n\n')

  return(folder)
}  

get.blended.sst<-function (track, folder = tempdir(), 
  server = 'http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=TBAssta', 
  nday='5day')
{
    fmtDate<-function(date){
      x<-date.mdy(date) 
      paste(x$year, formatC(x$month, digits = 1, flag='0', format='d'),
                    formatC(x$day, digits = 1, flag='0', format='d'), sep='-')
    }
    fmtDay<-function(day){
      formatC(day, digits = 2, flag='0', format='d')    
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
    datesteps<-seq(minDate, maxDate, by=ifelse(nday=='5day',5,8))
    minLon <- min(unlist(lapply(track, function(x) min(x[, 4])))) - 1
    maxLon <- max(unlist(lapply(track, function(x) max(x[, 4])))) + 1
    minLat <- min(unlist(lapply(track, function(x) min(x[, 5])))) - 1
    maxLat <- max(unlist(lapply(track, function(x) max(x[, 5])))) + 5
    op<-'&timePeriod=NDAY&centeredTime=~DATET12%3A00%3A00&maxLat=MAXLAT&minLon=MINLON&maxLon=MAXLON&minLat=MINLAT&filetype=.xyz' 

    for(d in datesteps){
      opt<-sub('NDAY', nday, op)
      opt<-sub('DATE', fmtDate(d), opt)
      opt<-sub('MAXLAT', maxLat, opt)
      opt<-sub('MINLON', minLon, opt)
      opt<-sub('MAXLON', maxLon, opt)
      opt<-sub('MINLAT', minLat, opt)
      link <- paste(server, opt, sep = "")
      
      y1<-date.mdy(d-ifelse(nday=='5day',2,3))$year
      d1<-(d-2)-mdy.date(month=1, day=1, year=y1)+1
      y2<-date.mdy(d+ifelse(nday=='5day',2,8))$year
      d2<-(d+2)-mdy.date(month=1, day=1, year=y2)+1
      filename<-paste('BA',y1,fmtDay(d1),'_',y2,fmtDay(d2),'_sst.xyz', sep='')
      dest <- paste(folder, filename, sep = "/")
      download.file(link, dest, mode = "wb")
      tmp<-matrix(scan(dest), ncol=3, byrow=TRUE)[,c(2,1,3)]
      write.table(tmp, file=dest, quote=FALSE, row.names=FALSE, col.names=FALSE) 
    }
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
    cat("Downloaded", length(dir(folder)), "files to:\n\n  ",
        folder)
    cat("\n\nNow you most likely want to run a command like:\n\n   ")
    cat(paste("sst.file <- write.sst.field(datadir='", folder,
        "', filename='sst.dat')", sep = ""), "\n\n")
    cat("to generate the smooth representation of the SST-field and its\n")
    cat("derivatives and save it in the file called 'sst.dat'\n\n")
    cat("Please consult the documentation (by typing '?write.sst.field') \n")
    cat("for a description of the different options including degree of\n")
    cat("smoothing and resolution of representation.\n\n")
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
    return(folder)
}

write.sst.field<-function(datadir, nlon=100, nlat=150, filename="sst.dat", alpha=0.05, 
                          from.ystr=c(3,6), from.dstr=c(7,9), to.ystr=c(11,14), 
                          to.dstr=c(15,17), peak=FALSE)
{
  mydig<-8 # found that locfit win differs from locfit linux - this should fix it
  dirlist<-dir(datadir)
  
  greg.zero.date<-mdy.date(day=15, month=10, year=1582)
  
  from<-mdy.date(day=1, month=1, year=as.numeric(substr(dirlist,  from.ystr[1], from.ystr[2])))
  from<-from+as.numeric(substr(dirlist, from.dstr[1], from.dstr[2]))-1-greg.zero.date
  
  to<-mdy.date(day=1, month=1, year=as.numeric(substr(dirlist, to.ystr[1], to.ystr[2])))
  to<-to+as.numeric(substr(dirlist, to.dstr[1], to.dstr[2]))-1-greg.zero.date

  grid.greg<-0.5*(from+to)
  ngreg<-length(grid.greg) 
               
  dat<-read.table(paste(datadir,dirlist[1], sep="/"), header=FALSE)
  x<-dat[,2]; y<-dat[,1]; z<-dat[,3];
  grid.x<-seq(min(x), max(x), length=nlon)
  grid.y<-seq(min(y), max(y), length=nlat)
  grid<-expand.grid(grid.x,grid.y)

  cat("", file=filename)
  cat(ngreg, "\n", nlon, "\n", nlat, "\n", file=filename, append=TRUE)
  cat(grid.greg, "\n", file=filename, append=TRUE)
  cat(grid.x, "\n", file=filename, append=TRUE)
  cat(grid.y, "\n", file=filename, append=TRUE)

  cat('\nStep 1/3:')
  for(file in dirlist){
    dat<-read.table(paste(datadir,file, sep="/"), header=FALSE)
    x<-dat[,2]; y<-dat[,1]; z<-dat[,3];
    sst<-locfit(z~x:y, alpha=alpha)
    grid.sst<-matrix(predict(sst, newdata=as.matrix(grid)), ncol=nlon, nrow=nlat, byrow=TRUE)
    if(peak){
      par(mfrow=c(1,2))
      ny<-length(unique(y))
      nx<-length(unique(x))
      mat<-matrix(z, nrow=ny, ncol=nx, byrow=TRUE)
      image(unique(x),unique(y)[ny:1], t(mat[ny:1,]), xlab='Longitude', ylab='Latitude', main='Raw')
      contour(unique(x),unique(y)[ny:1], t(mat[ny:1,]), add=TRUE)
      image(grid.x,grid.y, t(grid.sst), xlab='Longitude', ylab='Latitude', main='Smoothed')
      contour(grid.x,grid.y, t(grid.sst), add=TRUE)
      dummy<-readline('Press return to see next\n')
      par(mfrow=c(1,1))
    }
    write.table(round(grid.sst,mydig), file=filename, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
    cat(".")
    .myflush()
  }
  cat('\nStep 2/3:')
  for(file in dirlist){
    dat<-read.table(paste(datadir,file, sep="/"), header=FALSE)
    x<-dat[,2]; y<-dat[,1]; z<-dat[,3];
    sst.dx<-locfit(z~x:y, deriv=1, alpha=alpha)
    grid.sst.dx<-matrix(predict(sst.dx, newdata=as.matrix(grid)), ncol=nlon, nrow=nlat, byrow=TRUE)
    write.table(round(grid.sst.dx,mydig), file=filename, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
    cat(".")
    .myflush()
  }
  cat('\nStep 3/3:')
  for(file in dirlist){
    dat<-read.table(paste(datadir,file, sep="/"), header=FALSE)
    x<-dat[,2]; y<-dat[,1]; z<-dat[,3];
    sst.dy<-locfit(z~x:y, deriv=2, alpha=alpha)
    grid.sst.dy<-matrix(predict(sst.dy, newdata=as.matrix(grid)), ncol=nlon, nrow=nlat, byrow=TRUE)
    write.table(round(grid.sst.dy,mydig), file=filename, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
    cat(".")
    .myflush()
  }
  cat("\n\n")
  
  ow<-getwd()
  setwd(dirname(filename))
  filename<-file.path(getwd(), basename(filename))
  setwd(ow)

  #==================================================================#  
  cat(paste(rep('=',options()$width), collapse=''),'\n\n')
  cat('The smoothed field and its derivatives is now saved in the\n')
  cat('file',paste('\'',filename,'\'.', sep=''),'\n\n')
  cat('This filename should be given as the second argument to the\n')
  cat('\'kfsst\' function (first argument is the track). For example:\n\n')
  
  cat(paste('   fit<-kfsst(track1, sst.file=\'',filename,'\')',sep=''),'\n\n')

  cat('The function \'kfsst\' fits the default full model. It is\n') 
  cat('possible and in some cases necessary to customize the full\n') 
  cat('model, for instance by choosing a simpler variance structure,\n') 
  cat('or leaving out a bias term. For details on these and other\n') 
  cat('options, please see the documentation (?kfsst).\n\n')
  cat(paste(rep('=',options()$width), collapse=''),'\n\n')
  return(path.expand(filename))
}


kfsst<-function (data, sst.file='sst.dat', fix.first = TRUE, fix.last = TRUE, 
    theta.active = c(u.active, v.active, D.active, bx.active, by.active, 
    bsst.active, sx.active, sy.active, ssst.active, a0.active, b0.active), 
    theta.init = c(u.init, v.init, D.init, bx.init, by.init, bsst.init, 
    sx.init, sy.init, ssst.init, a0.init, b0.init), u.active = TRUE, 
    v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active=TRUE, 
    bsst.active=TRUE, sx.active = TRUE, sy.active = TRUE, ssst.active=TRUE, 
    a0.active = TRUE, b0.active = TRUE, u.init = 0, v.init = 0, D.init = 100, 
    bx.init = 0, by.init = 0, bsst.init=0, sx.init = 0.1, sy.init = 1.0, 
    ssst.init=.1, a0.init = 0.001, b0.init = 0, var.struct = "solstice", 
    dev.pen = 0, save.dir = NULL, admb.string = "")
{
    
    cat(sst.file, '\n')
    olddir <- getwd()
    dirname <- ifelse(is.null(save.dir), "_kfsst_temp_", save.dir)
    dir.create(dirname)
    file.copy(sst.file, paste(dirname,"sst.dat", sep='/'), TRUE)
    setwd(dirname)
    if(!is.null(save.dir)){
      mptfilename <- paste("mpt_", paste(as.numeric(c(theta.active[c(1:5,7:8)],
                           var.struct == "solstice", var.struct == "daily")), collapse = ""),
                           ".dat", sep = "")
      unlink(c(mptfilename, 'kfsst.par', 'kfsst.rep', 'kfsst.std'))
    }
    header <- .generate.dat.file.kfsst(data, "kfsst.dat", fix.first,
        fix.last, theta.active, theta.init, var.struct = var.struct,
        dev.pen = dev.pen)
    if (.Platform$OS.type == "windows") {
      error.code<-.sys(paste(.path.package("kfsst"),"/admb/kfsst.exe", " ", admb.string, sep = ""))
    }else {
      file.copy(paste(.path.package('kfsst'),"admb/kfsst",sep="/"),"kfsst", TRUE)
      .sys("chmod u+x kfsst")
      error.code<-.sys(paste("./kfsst", " ", admb.string, sep = ""))
    }
    kf.o <- .read.output.kfsst(data, fix.first, fix.last, theta.active,
        theta.init, var.struct = var.struct, dev.pen = dev.pen)
    kf.o$nobs <- nrow(data)
    kf.o$header <- header
    kf.o$fix.first <- fix.first
    kf.o$fix.last <- fix.last
    kf.o$theta.active <- theta.active
    kf.o$theta.init <- theta.init
    kf.o$var.struct <- var.struct
    kf.o$dev.pen <- dev.pen
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

.generate.dat.file.kfsst<-function (data, file = "kfsst.dat", fix.first = TRUE, fix.last = TRUE,
    theta.active = c(u.active, v.active, D.active, bx.active, by.active, bsst.active, sx.active,
    sy.active, ssst.active, a0.active, b0.active), theta.init = c(u.init, v.init, D.init,
    bx.init, by.init, bsst.init, sx.init, sy.init, ssst.init, a0.init, b0.init), u.active = TRUE,
    v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active=TRUE, bsst.active=TRUE, sx.active = TRUE, sy.active = TRUE,
    ssst.active=TRUE, a0.active = TRUE, b0.active = TRUE,
    u.init = 0, v.init = 0, D.init = 100, bx.init = 0, by.init = 0, bsst.init=0, 
    sx.init = 0.5, sy.init = 1.5, ssst.init=.1, a0.init = 0.001, b0.init = 0,
    var.struct = "solstice", dev.pen = 0)
{
    "%+%" <- function(s1, s2) paste(s1, s2, sep = "")
    tostr <- function(x) paste(as.numeric(x), collapse = "\t")
    var.flags <- switch(var.struct, uniform = c(0, 0, 0), solstice = c(1,
        0, 0), daily = c(0, 1, dev.pen), warning("No matching variance structure found"))
    header <- "#Auto generated data file from R-KFsst \n#" %+%
        date() %+% "\n#\n" %+% "# Number of data points\n  " %+%
        tostr(nrow(data)) %+% "\n#\n" %+% "# 1 if first point is true release position; 0 otherwise\n  " %+%
        tostr(fix.first) %+% "\n#\n" %+% "# 1 if last point is true recapture position; 0 otherwise\n  " %+%
        tostr(fix.last) %+% "\n#\n" %+% "# active parameters \n# u\tv\tD\tbx\tby\tbsst\tsx\tsy\tssst\ta0\tb0\n  " %+%
        tostr(theta.active) %+% "\n#\n" %+% "# initial values \n# u\tv\tD\tbx\tby\tbsst\tsx\tsy\tssst\ta0\tb0\n  " %+%
        tostr(theta.init) %+% "\n#\n" %+% "# latitude errors \n# cos\tdev\tdeviation penalty weight\n  " %+%
        tostr(var.flags) %+% "\n#\n" %+% "# Positions \n# day\tmonth\tyear\tlong\tlati\tsst\n  "
    cat(header, file = file)
    write.table(data, file = file, row.names = FALSE, col.names = FALSE,
        sep = "\t", eol = "\n  ", append = TRUE)
    cat("\n", file = file, append = TRUE)
    class(header) <- "kfssthead"
    return(header)
}

.read.output.kfsst<-function (data, fix.first = TRUE, fix.last = TRUE, theta.active = c(u.active,
    v.active, D.active, bx.active, by.active, sx.active, sy.active,
    a0.active, b0.active), theta.init = c(u.init, v.init, D.init,
    bx.init, by.init, sx.init, sy.init, a0.init, b0.init), u.active = TRUE,
    v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active = TRUE,
    sx.active = TRUE, sy.active = TRUE, a0.active = TRUE, b0.active = TRUE,
    u.init = 0, v.init = 0, D.init = 100, bx.init = 0, by.init = 0,
    sx.init = 0.5, sy.init = 1.5, a0.init = 0.001, b0.init = 0,
    var.struct = "solstice", dev.pen = 0)
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
    theta.names <- c("u", "v", "D", "bx", "by", "bsst", "sx", "sy", "ssst")
    if (var.struct == "solstice") {
        theta.names <- c(theta.names, "a0", "b0")
    }
    if (file.access("kfsst.par") != 0) {
        warning("File \"kfsst.par\" not found (possibly because there was no solution to minimization problem)",
            call. = FALSE)
        npar <- NA
        nlogL <- NA
        max.grad.comp <- NA
        estimates <- NA
    }
    else {
        tmp <- as.numeric(scan("kfsst.par", what = "character",
            16, quiet = TRUE)[c(6, 11, 16)])
        npar <- tmp[1]
        nlogL <- tmp[2]
        max.grad.comp <- tmp[3]
        estimates <- sapply(c("uu", "vv", "D", "bx", "by", "bsst", "vx",
            "vy", "vsst"), getpar, file = "kfsst.par")
        if (var.struct == "solstice") {
            estimates <- c(estimates, sapply(c("a0", "b0"), getpar,
                file = "kfsst.par"))
        }
        names(estimates) <- theta.names
    }
    if (file.access("kfsst.rep") != 0) {
        warning("File \"kfsst.rep\" not found", call. = FALSE)
        spd <- NA
        hdg <- NA
    }
    else {
        spd <- getrep("spd", "kfsst.rep")
        hdg <- getrep("hdg", "kfsst.rep")
    }
    if (file.access("kfsst.std") != 0) {
        warning("File \"kfsst.std\" not found (possibly the hessian was not estimated)",
            call. = FALSE)
        std.dev <- NA
    }
    else {
        dat <- read.table("kfsst.std", skip = 1)
        tmp <- dat[dat[, 2] %in% c("sduu", "sdvv", "sdD", "sdbx",
            "sdby", "sdbsst", "sdvx", "sdvy", "sdvsst"), 3:4]
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
    mptfilename <- paste("mpt_", paste(as.numeric(c(theta.active[c(1:5,7:8)],
        var.struct == "solstice", var.struct == "daily")), collapse = ""),
        ".dat", sep = "")
    if (file.access(mptfilename) != 0) {
        warning("File not found")
    }
    else {
        tmp <- read.table(mptfilename, skip = 3, header = FALSE)
        name <- strsplit(readLines(mptfilename, 3)[3], split = " ")[[1]]
        colnames(tmp) <- name[!name %in% c("", "#")]
        
        nominal.track <- cbind(x = tmp$ox, y = tmp$oy)
        pred.track <- cbind(x = tmp$px, y = tmp$py)
        most.prob.track <- cbind(x = tmp$smoothX, y = tmp$smoothY)
        SST<- cbind(o=tmp$oSST, p=tmp$pSST, smooth=tmp$smoothSST)
        days.at.liberty <- cumsum(tmp$dt)
        date <- matrix(as.numeric(unlist(strsplit(as.character(tmp$date),
            "/"))), ncol = 3, byrow = TRUE)
        colnames(date) <- c("year", "month", "day")
        var.most.prob.track <- cbind(tmp$Psmooth11, tmp$Psmooth12, tmp$Psmooth21,
            tmp$Psmooth22)
    }
    return(list(npar = npar, nlogL = nlogL, max.grad.comp = max.grad.comp,
        estimates = estimates, std.dev = std.dev, nominal.track = nominal.track,
        pred.track = pred.track, most.prob.track = most.prob.track,
        var.most.prob.track = var.most.prob.track, days.at.liberty = days.at.liberty,
        date = date, spd = spd, hdg = hdg, SST=SST))
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
