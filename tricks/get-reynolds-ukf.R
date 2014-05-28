get.reynolds <- function(track, folder = tempdir(), removeland = TRUE)
{
  # Metadata: http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html
  require(date)
  require(ncdf)
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
  unlink(paste(folder, "/*", sep=""), F)
  sstfolder <- paste(folder, "sst_files", sep="/")
  testdir <- file.info(sstfolder)$isdir
  if (is.na(testdir)) {
        dir.create(sstfolder)
  }
  else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
  }
  unlink(paste(sstfolder, "/*", sep=""), F)
  if (is.data.frame(track)) track <- list(track)
  minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 1], x[1, 3]))))
  maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 1], x[nrow(x), 3]))))
  minLon <- min(unlist(lapply(track, function(x) min(x[, 4])))) -  2
  maxLon <- max(unlist(lapply(track, function(x) max(x[, 4])))) + 2
  minLat <- min(unlist(lapply(track, function(x) min(x[, 5])))) - 4
  maxLat <- max(unlist(lapply(track, function(x) max(x[, 5])))) + 4
  latlow <- ifelse(minLat < -89.5, -89.5, trunc(minLat)-0.5)
  lathigh <- ifelse(maxLat > 89.5, 89.5, trunc(maxLat)+0.5)
  lonlow <- ifelse(minLon < 0, trunc(minLon)+360-0.5, trunc(minLon)-0.5)
  lonhigh <- ifelse(maxLon < 0, trunc(maxLon)+360+0.5, trunc(maxLon)+0.5)
  #
  # Download and subset land mask
  link <- "ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/lsmask.nc"
  fname = paste(folder, "landmask.nc", sep = "/")
  download.file(link, fname, mode="wb")
  nc <- open.ncdf(fname)
  land <- get.var.ncdf(nc, varid="mask")
  land <- land[lonlow:lonhigh,((lathigh-90)*-1):((latlow-90)*-1)]
  close.ncdf(nc)
  #
  # Get dataset ID and find out the end date and file dates of the imagery series
  link <- "http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBSearch.pl?Dataset=NOAA+Optimum+Interpolation+(OI)+SST+V2&Variable=Sea+Surface+Temperature"
  fname = paste(folder, "search.html", sep = "/")
  download.file(link, fname, mode="wb")
  dfile <- readLines(fname)
  unlink(fname)
  idstring <- dfile[grep("sst.wkmean.1990-present.nc", dfile)-3]
  idstring <- unlist(strsplit(idstring, "&DB_"))
  idstring <- idstring[grep("tid", idstring)]
  idstring <- sub("tid=", "", idstring)
  dstring <- dfile[grep("sst.wkmean.1990-present.nc", dfile)-1]
  dstring <- gsub("</td>", "", dstring)
  dstring <- gsub("<td>", "", dstring)
  dstring <- as.Date(unlist(strsplit(dstring, " ")), "%Y/%m/%d")
  enddate <- as.numeric(unlist(strsplit(as.character(dstring[2]), "-")))
  enddate <- mdy.date(enddate[2],enddate[3],enddate[1])
  filedates <- seq(dstring[1], dstring[2], 7)
  filedates <- as.date(as.numeric(filedates)+ 3653)
  if (minDate >= enddate)
      stop("Start date of your track is beyond that of the available SST images.")
  date1 <- which(minDate >= filedates)
  date1 <- ifelse(length(date1)==0, filedates[1], max(date1))
  date1 <- filedates[ifelse(date1==1, date1, date1 - 1)]
  date2 <- which(maxDate <= filedates)
  date2 <- ifelse(length(date2)==0, length(filedates), min(date2))
  date2 <- filedates[ifelse(date2==length(filedates), date2, date2 + 1)]
  cat(paste("Contacting server for SST images within this date range: ", date1, " - ", date2, "\n\n" , sep=""))
  #
  # Perform the subset and download netcdf file from FTP
  latlow <- ifelse(latlow > 0, paste(latlow, "N", sep=""), paste(latlow*-1, "S", sep=""))
  lathigh <- ifelse(lathigh > 0, paste(lathigh, "N", sep=""), paste(lathigh*-1, "S", sep=""))
  lonlow <- paste(lonlow, "E", sep="")
  lonhigh <- paste(lonhigh, "E", sep="")
  link <- "http://www.esrl.noaa.gov/psd/cgi-bin/GrADS.pl?dataset=NOAA+Optimum+Interpolation+%28OI%29+SST+V2&DB_did=62&file=%2FDatasets%2Fnoaa.oisst.v2%2Fsst.wkmean.1990-present.nc&variable=sst&DB_vid=1296&DB_tid=DATAID&units=degC&longstat=Mean&DB_statistic=Mean&stat=&lat-begin=LATLOW&lat-end=LATHIGH&lon-begin=LONLOW&lon-end=LONHIGH&dim0=time&year_begin=YEARLOW&mon_begin=MONTHLOW&day_begin=DAYLOW&year_end=YEARHIGH&mon_end=MONTHHIGH&day_end=DAYHIGH&X=lon&Y=lat&output=file&bckgrnd=black&use_color=on&cint=&range1=&range2=&scale=100&submit=Create+Plot+or+Subset+of+Data"
  link <- sub("DATAID", idstring, link)
  opt <- sub("LATLOW", latlow, link)
  opt <- sub("LATHIGH", lathigh, opt)
  opt <- sub("LONLOW", lonlow, opt)
  opt <- sub("LONHIGH", lonhigh, opt)
  opt <- sub("YEARLOW", format(as.Date(date1), "%Y"), opt)
  opt <- sub("MONTHLOW", format(as.Date(date1), "%b"), opt)
  opt <- sub("DAYLOW", as.numeric(format(as.Date(date1), "%d")), opt)
  opt <- sub("YEARHIGH", format(as.Date(date2), "%Y"), opt)
  opt <- sub("MONTHHIGH", format(as.Date(date2), "%b"), opt)
  opt <- sub("DAYHIGH", as.numeric(format(as.Date(date2), "%d")), opt)
  fname = paste(folder, "request.html", sep = "/")
  download.file(opt, fname, mode="wb")
  dfile <- readLines(fname)
  unlink(fname)
  dstring <- dfile[grep("ftp://ftp.cdc.noaa.gov/", dfile)]
  flink <- unlist(strsplit(unlist(strsplit(dstring, "a href="))[2], ">"))[1]
  fname <- paste(folder, "oisst.nc", sep = "/")
  download.file(flink, fname, mode="wb")
  cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
  cat(paste("SST data are downloaded as a netcdf file from \n\n", flink, "\n\n" , sep=""))
  #
  # Add land mask to oisst.nc
  nc <- open.ncdf(fname, write=T)
  xdim <- nc$dim[['lon']]
  ydim <- nc$dim[['lat']]
  varz = var.def.ncdf("land","flag", list(xdim,ydim), 32767, 
          longname="Land mask for SST values (1=ocean, 0=land)")
  nc <- var.add.ncdf(nc, varz)
  put.var.ncdf(nc, "land", land)
  sync.ncdf(nc)
  close.ncdf(nc)
  land <- t(land) # the dimensions are flipped
  #
  # Extract each day of data as xyz
  nc <- open.ncdf(fname)
  lon <- get.var.ncdf(nc, varid="lon")
  lat <- get.var.ncdf(nc, varid="lat")
  dates <- as.Date("1800-01-01") + get.var.ncdf(nc, varid="time")
  every.day <- 7
  vv      <- nc$var[[1]]
  varsize <- vv$varsize
  ndims   <- vv$ndims
  nt      <- varsize[ndims]  # Remember timelike dim is always the LAST dimension!
  for (i in 1:nt){
        # Initialize start and count to read one timestep of the variable.
        start <- rep(1,ndims)   # begin with start=(1,1,1,...,1)
        start[ndims] <- i       # change to start=(1,1,1,...,i) to read timestep i
        count <- varsize        # begin w/count=(nx,ny,nz,...,nt), reads entire var
        count[ndims] <- 1       # change to count=(nx,ny,nz,...,1) to read 1 tstep
        sst <- round(t(get.var.ncdf(nc, vv, start=start, count=count )),2)
        # Prepare an individual xyz file
        xyz <- rbind(rep(NA, 4))
        d <- mdy.date(as.numeric(format(dates[i], "%m")),
                      as.numeric(format(dates[i], "%d")),
                      as.numeric(format(dates[i], "%Y")))
        # !! The date constructs for the filename are different - the first date is the image date
        # !! Unlike old code that centers the image date in between d1 and d2 (+/- days to position it)
	y1 <- date.mdy(d)$year
        d1 <- d - mdy.date(month = 1, day = 1, year = y1) + 1
        y2 <- date.mdy(d + every.day - 1)$year
        d2 <- (d + every.day -1) - mdy.date(month = 1, 
               day = 1, year = y2) + 1
        filename <- paste("RS", y1, fmtDay(d1), 
                    "_", y2, fmtDay(d2), "_", "sst", ".xyz", sep = "")
        dest <- paste(sstfolder, filename, sep = "/")
        for (j in 1:length(lon)) {
             xyz <- rbind(xyz, cbind(lat, lon[j], sst[,j], land[,j]))
        }
        xyz <- na.omit(xyz)
        if (removeland) xyz <- xyz[which(xyz[,4]==1), -4] 
        write.table(xyz, file = dest, 
                    quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  close.ncdf(nc)
  cat("And repackaged to", length(dir(sstfolder)), "xyz files in:\n\n  ", sstfolder, "\n\n")
  cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
  .sstFileVector <<- paste(sstfolder, dir(sstfolder), sep = "/")
  return(sstfolder)
} 

# Not run
# setwd("C:\\Documents and Settings\\Valued Customer\\Desktop\\R\\getsst")
### source("get-reynolds-ukf.R")
# library(ukfsst)
# data(blue.shark)
# sst.path <- get.reynolds(blue.shark)
# fit <- kfsst(blue.shark, bx.active=FALSE, bsst.active=FALSE)
