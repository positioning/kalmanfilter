rddap <- 
function (datevec, lonlow, lonhigh, latlow, lathigh, altitude = '[(0.0):1:(0.0)]',
          every.day=5, folder = tempdir(), 
          # change every.day to a smaller number to get SST images at a more frequent basis
          server = 'http://coastwatch.pfeg.noaa.gov/erddap/griddap/', type=2, 
          # see "filetype" below for file formats
          product = 'ncdcOisst2Agg', variable='sst', at=1, 
	      name = NA, repack = TRUE, repack.cols = c(-1,-2), repack.to360=F, trim = TRUE) 
{
    # More info on: http://coastwatch.pfeg.noaa.gov/erddap/info/index.html
    # product <- c('ncdcOisst2Agg', 'ncdcOisst2AmsrAgg', 'erdG1ssta1day', 'erdBAssta5day')
    # variable <- c('sst', 'anom', 'err')
    require(date)
    filetype <- c('.csv', '.tsv', '.nc', '.kml', '.largePng')
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
    testdir <- file.info(folder)$isdir
    if (is.na(testdir)) {
        dir.create(folder)
    }
    else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
    }
    fl <- dir(folder)
    if (length(fl) != 0) {
        folder <- paste(folder, "images", sep = "/")
        dir.create(folder)
    }
    if (is.data.frame(datevec)) 
        datevec <- list(datevec)
    minDate <- min(unlist(lapply(datevec, function(x) mdy.date(x[1, 
        2], x[1, 3], x[1, 1]))))
    maxDate <- max(unlist(lapply(datevec, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 3], x[nrow(x), 1]))))
    minDate <- minDate - 10
    maxDate <- maxDate + 10
    datesteps <- seq(minDate, maxDate, by = every.day)
    op <- paste(server, filetype[type], sep="")
    op <- paste(op, "?", variable, sep="")
    opt <- paste('[(DATE):1:(DATE)]',altitude, '[(MINLAT):STRIDE:(MAXLAT)]', '[(MINLON):STRIDE:(MAXLON)]', sep="")
    myurl <- opt
    for (d in datesteps) {
        opt <- myurl
        opt <- gsub("DATE", fmtDate(d), opt)
        opt <- sub("MAXLAT", lathigh, opt)
        opt <- sub("MINLON", lonlow, opt)
        opt <- sub("MAXLON", lonhigh, opt)
        opt <- sub("MINLAT", latlow, opt)
	opt <- gsub("STRIDE", at, opt)
        link <- paste(op, opt, sep = "")
        name <- ifelse(is.na(name), product, name)
        ext <- sub('large', '', filetype[type])
        if ((repack==TRUE)&&(ext=='.tsv')){
	# !! The date constructs for the filename are different - the first date is the image date
        # !! Unlike old code that centers the image date in between d1 and d2 (+/- days to position it)
	      y1 <- date.mdy(d)$year
              d1 <- d - mdy.date(month = 1, day = 1, year = y1) + 1
              y2 <- date.mdy(d + every.day - 1)$year
              d2 <- (d + every.day -1) - mdy.date(month = 1, 
                     day = 1, year = y2) + 1
              filename <- paste(substr(name,1,2), y1, fmtDay(d1), 
                         "_", y2, fmtDay(d2), "_", product, ".xyz", sep = "")}
        else {filename <- paste(name, as.Date(as.date(d)), ext, sep="")}
        dest <- paste(folder, filename, sep = "/")
	download.file(link, dest, mode = "wb")
        if ((repack==T)&&(ext=='.tsv')){
	    tmp <- read.table(dest, skip = 2)[, repack.cols]
		    # Need to flip from -180 to 180, to 0-360
		    if (repack.to360==T){ jx <- which(tmp[,2]<0) 
		                         tmp[jx,2] <- tmp[jx,2]+360}
            if (trim) {tmp <- tmp[complete.cases(tmp), ]}
            write.table(tmp, file = dest, 
                        quote = FALSE, row.names = FALSE, col.names = FALSE)
	}
    }
return(folder)
}

# source("E:\\R\\erddap\\get.erddap.R")
# setwd("E:\\R\\erddap\\")
# dd <- read.csv("E:\\R\\erddap\\points.csv")
# mydates <- get.erddap(dd, 0, 360, -75, 75, every.day=1, folder="e:/tmp")

# Get SST from Upwell Erddap, goes from -180 to 180
#sst.path<-get.sst.from.server(track[,c(3,2,1)],280-360,305-360,35,48, altitude='',
#                              server='http://upwell.pfeg.noaa.gov/erddap/griddap/', 
#							  product='noaa_pfeg_c8b0_ceea_0b43', variable='analysed_sst', 
#							  repack=T, repack.cols = c(-1), repack.to360=T, folder="c:/temp") 

