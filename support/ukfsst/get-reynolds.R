get.reynolds <- function (track = NULL, datelow = "2008-01-01", datehigh = "2008-02-01", lonlow, lonhigh, latlow, lathigh, folder = tempdir(), removeland = TRUE, landvalue=NULL, minus180 = FALSE, offsetx = 0, offset.cutoff = 360, res = "low") 
{
	### Variant Aug 2025
	###-------------------------------------------------------------
	### Feed in date-range directly, instead of looking from track
	### Short-term fix for handling MTI tags for Matt's sailfish
	###-------------------------------------------------------------
    ### Updated Jan 2023
	###-------------------------------------------------------------
	### Switched to THREDDS Data Server
	### Source: OISST v2 high res @0.25 deg 
	### then up-scale to "low" res @1.00 deg & "mid" res @0.50
	### https://psl.noaa.gov/mddb2/makePlot.html?variableID=156648
	###-------------------------------------------------------------

    require(date)
    require(ncdf4)
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
    unlink(paste(folder, "/*", sep = ""), F)
    sstfolder <- paste(folder, "sst_files", sep = "/")
    testdir <- file.info(sstfolder)$isdir
    if (is.na(testdir)) {
        dir.create(sstfolder)
    }
    else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
    }
    unlink(paste(sstfolder, "/*", sep = ""), F)
    
    ### Variant starts...
    if (!is.null(track)){
      if (is.data.frame(track)) 
          track <- list(track)
      minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
          2], x[1, 3], x[1, 1]))))
      maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
          2], x[nrow(x), 3], x[nrow(x), 1]))))
    } else {
    	  md <- as.numeric(unlist(strsplit(datelow,"-")))
    	  minDate <- mdy.date(md[2],md[3],md[1])
    	  md <- as.numeric(unlist(strsplit(datehigh,"-")))
      maxDate <- mdy.date(md[2],md[3],md[1])
    }
    ### Variant ends....
        
    latlow <- ifelse(latlow < -90, -90, trunc(latlow))
    lathigh <- ifelse(lathigh > 90, 90, trunc(lathigh))
    lonlow <- ifelse(lonlow < 0, trunc(lonlow) + 360, trunc(lonlow))
    lonhigh <- ifelse(lonhigh < 0, trunc(lonhigh) + 360, trunc(lonhigh))
	### THREDDS catalogue to work out the end date
	link <- "https://psl.noaa.gov/thredds/ncml/Datasets/noaa.oisst.v2.highres/sst.week.mean.nc?catalog=http://psl.noaa.gov/thredds/catalog/Datasets/noaa.oisst.v2.highres/catalog.html&dataset=Datasets/noaa.oisst.v2.highres/sst.week.mean.ncc"
    fname = paste(folder, "search.html", sep = "/")
    download.file(link, fname, mode = "wb")
    dfile <- readLines(fname)
    unlink(fname)
	cfstring <- '    <attribute name="time_coverage_end" value="'
	dstring <- gsub(cfstring, '', dfile[grep(cfstring,dfile)])
	enddate <- mdy.date(as.numeric(substring(dstring,6,7)),
						as.numeric(substring(dstring,9,10)),
						as.numeric(substring(dstring,1,4)))
	### Extend weekly time steps by 10000					
	dbuffer <- 7*10000
    filedates <- as.Date("1800-01-01") + seq(66357,81463+dbuffer,7)  
    filedates <- as.date(as.numeric(filedates) + 3653)
    if (minDate >= enddate) 
        stop("Start date of your track is beyond that of the available SST images.")
    date1 <- which(minDate >= filedates)
    date1 <- ifelse(length(date1) == 0, filedates[1], max(date1))
    date1 <- filedates[ifelse(date1 == 1, date1, date1 - 1)]
    date2 <- which(maxDate <= filedates)
    date2 <- ifelse(length(date2) == 0, length(filedates), min(date2))
    date2 <- filedates[ifelse(date2 == length(filedates), date2, 
        date2 + 1)]
    cat(paste("Contacting server for SST images within this date range: ", 
        date1, " - ", date2, "\n\n", sep = ""))
	### Dataset link
	link <- 
	'http://psl.noaa.gov/thredds/ncss/grid/Datasets/noaa.oisst.v2.highres/sst.week.mean.nc'
    link <- paste0(link,"?var=sst&north=LATHIGH&west=LONLOW&east=LONHIGH&south=LATLOW&horizStride=1&time_start=DATELOWT00:00:00Z&time_end=DATEHIGHT00:00:00Z&&&accept=netcdf3")
	### THREDDS accepts lon in 0-360, so just need to handle a special case of (0,360)
	if (lonlow == 0 & lonhigh == 360) { lonlow = 0.001; lonhigh = 359.999}
	### Piece together the THREDDS subsetting link	
	opt <- sub("LATLOW", latlow, link)
    opt <- sub("LATHIGH", lathigh, opt)
    opt <- sub("LONLOW", lonlow, opt)
    opt <- sub("LONHIGH", lonhigh, opt)
    opt <- sub("DATELOW", format(as.Date(date1), "%Y-%m-%d"), opt)
    opt <- sub("DATEHIGH", format(as.Date(date2), "%Y-%m-%d"), opt)
    fname <- paste(folder, "oisst.nc", sep = "/")
    download.file(opt, fname, mode = "wb")
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
    cat(paste("SST data are downloaded as a netcdf file from \n\n", 
        opt, "\n\n", sep = ""))
	### Extract individual layers from netcdf file	
    nc <- nc_open(fname)
    lon <- ncvar_get(nc, varid = "lon")
    lat <- ncvar_get(nc, varid = "lat")
    dates <- as.Date("1800-01-01") + ncvar_get(nc, varid = "time")
    every.day <- 7
    vv <- nc$var[[1]]
    varsize <- vv$varsize
    ndims <- vv$ndims
    nt <- varsize[ndims]
    for (i in 1:nt) {
        start <- rep(1, ndims)
        start[ndims] <- i
        count <- varsize
        count[ndims] <- 1
        sst <- round(t(ncvar_get(nc, vv, start = start, count = count)), 2)
		# Add dummy header row
        xyz <- rbind(rep(NA,3))
        d <- mdy.date(as.numeric(format(dates[i], "%m")), as.numeric(format(dates[i], 
            "%d")), as.numeric(format(dates[i], "%Y")))
        y1 <- date.mdy(d)$year
        d1 <- d - mdy.date(month = 1, day = 1, year = y1) + 1
        y2 <- date.mdy(d + every.day - 1)$year
        d2 <- (d + every.day - 1) - mdy.date(month = 1, day = 1, 
            year = y2) + 1
        filename <- paste("RS", y1, fmtDay(d1), "_", y2, fmtDay(d2), 
            "_", "sst", ".xyz", sep = "")
        dest <- paste(sstfolder, filename, sep = "/")
        for (j in 1:length(lon)) {
            m180 <- ifelse(lon[j] > 180, lon[j] - 360, lon[j]) + offsetx
            xyz <- rbind(xyz, cbind(lat, ifelse(minus180, m180, 
                lon[j]), sst[, j])) 			  
        }
		# Remove dummy header
		xyz <- xyz[-1,]
		xyz <- as.data.frame(xyz); names(xyz) <- c("y","x","v")
		### Upscale to either 1.0 or 0.5 deg
        if (res != "high"){
		  deg = ifelse(res == "low", 1, 0.5)
		  xyz$ay <- xyz[,1] %/% deg * deg
		  xyz$ax <- xyz[,2] %/% deg * deg
		  axyz <- with(xyz, aggregate(x = v, by = list(ay,ax), FUN = "mean", na.rm=T))
		  axyz[,3] <- round(axyz[,3],2)
		  xyz <- axyz
		}
		### Flag land values, which is read in as NA
		idx <- which(is.na(xyz[,3])==T)
        if (removeland){xyz <- na.omit(xyz)} 
		else {
			# Assign land values with a user-specified number
			if (!is.null(landvalue)) {xyz[idx,3] <- landvalue}
		}
        ### Handling tracks crossing from 360 to 0 longitude and vice versa		
        if (minus180) {
            jy <- order(xyz[, 2])
            xyz <- xyz[jy, ]
            jy <- which(xyz[, 2] > offset.cutoff)[1] - 1
            xyz <- xyz[1:jy, ]
        }
        write.table(xyz, file = dest, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
    nc_close(nc)
    cat("And repackaged to", length(dir(sstfolder)), "xyz files in:\n\n  ", 
        sstfolder, "\n\n")
    cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
    .sstFileVector <<- paste(sstfolder, dir(sstfolder), sep = "/")
    return(sstfolder)
}