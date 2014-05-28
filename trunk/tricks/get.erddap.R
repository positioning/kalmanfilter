get.erddap <- 
function (track, every.day=5, folder = tempdir(), 
          # change every.day to a smaller number to get SST images at a more frequent basis
          server = 'http://coastwatch.pfeg.noaa.gov/erddap/griddap/', type=2, 
          # see "filetype" below for file formats
          product = 'ncdcOisst2Agg', variable='sst', at=1, 
		  depth = c(0,0), yearless = F, 
		  kml.image = F, colorbar = "|||||", 
		  # an example of color bar, Rainbow2|D|Linear|10|20|30
	      name = NA, repack = TRUE, trim = TRUE) 
{
    ### More info on: http://coastwatch.pfeg.noaa.gov/erddap/info/index.html
    ### product <- c('ncdcOisst2Agg', 'ncdcOisst2AmsrAgg', 'erdG1ssta1day', 'erdBAssta5day')
    ### variable <- c('sst', 'anom', 'err')
    require(date)
	track <- data.frame(track)
	names(track) <- tolower(names(track))
    filetype <- c('.csv', '.tsv', '.nc', '.kml', '.largePng')
    server <- paste(server, product, sep="")
    fmtDate <- function(date, yearless=F) {
        x <- date.mdy(date)
		yy <- x$year
		if (yearless) yy <- "0000"
        paste(yy, 
		    formatC(x$month, digits = 1, flag = "0", 
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
    if (is.data.frame(track)) track <- list(data.frame(track$year,track$month,track$day,track$lon,track$lat))
	minLon <- min(unlist(lapply(track, function(x) min(x[, 4])))) - 
        5
    maxLon <- max(unlist(lapply(track, function(x) max(x[, 4])))) + 
        5
    minLat <- min(unlist(lapply(track, function(x) min(x[, 5])))) - 
        5
    maxLat <- max(unlist(lapply(track, function(x) max(x[, 5])))) + 
        5
    minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 3], x[1, 1]))))
    maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 3], x[nrow(x), 1]))))
    minDate <- minDate - 10
    maxDate <- maxDate + 10
    datesteps <- seq(minDate, maxDate, by = every.day)
    op <- paste(server, filetype[type], sep="")
    op <- paste(op, "?", variable, sep="")
    opt <- paste('[(DATE):1:(DATE)]','[(DEPTH1):1:(DEPTH2)]', '[(MINLAT):STRIDE:(MAXLAT)]', '[(MINLON):STRIDE:(MAXLON)]', sep="")
    myurl <- opt	
    for (d in datesteps) {
        opt <- myurl
        opt <- gsub("DATE", fmtDate(d, yearless), opt)
        opt <- sub("MAXLAT", maxLat, opt)
        opt <- sub("MINLON", minLon, opt)
        opt <- sub("MAXLON", maxLon, opt)
        opt <- sub("MINLAT", minLat, opt)	
	    opt <- gsub("STRIDE", at, opt)
		opt <- gsub("DEPTH1", depth[1], opt)
		opt <- gsub("DEPTH2", depth[2], opt)		
        link <- paste(op, opt, sep = "")
        name <- ifelse(is.na(name), product, name)
        ext <- sub('large', '', filetype[type])
        if ((repack==TRUE)&&(ext=='.tsv')){
	    ### !! The date constructs for the filename are different - the first date is the image date
        ### !! Unlike old code that centers the image date in between d1 and d2 (+/- days to position it)
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
	    tmp <- read.table(dest, skip = 2)[, c(-1,-2)]
            if (trim) {tmp <- tmp[complete.cases(tmp), ]}
            write.table(tmp, file = dest, 
                        quote = FALSE, row.names = FALSE, col.names = FALSE)
	    }
		if (ext=='.kml'){
			kmlfile <- readLines(dest)
			unlink(dest)
			# Find insert point
			ix <- which(kmlfile=="  <GroundOverlay>")[1]
			# Add TimeSpan to kml
			idx1 <- which(kmlfile == "  <GroundOverlay>")+1
			idx2 <- which(kmlfile == "</kml>")	
			begind <- as.Date(as.date(d))
			ctime1 <-  format.Date(begind, "%H:%M:%S")					
			ctime2 <-  format.Date(begind, "%Y-%m-%dT%H:%M:%S")
			endd <- (every.day-1)*60*60*24
			ctime2 <- format(strptime(ctime2, "%Y-%m-%dT%H:%M:%S", 
							  tz="GMT")+ endd + 60*60*24 -1, 
							  usetz=F, format="%Y-%m-%dT%H:%M:%SZ")			
			addtag    <- "<TimeSpan>"
			addtag[2] <- paste("<begin>", begind, "T", ctime1, "Z</begin>", sep="")
			addtag[3] <- paste("<end>", ctime2, "</end>", sep="")
			addtag[4] <- "</TimeSpan>"
			kmlfile <- c(kmlfile[1:(ix-1)], addtag, kmlfile[ix:length(kmlfile)])
			# Download the image
			if (kml.image){
			    gg <- grep("transparent", kmlfile)
				ilink <- kmlfile[gg]
				ilink <- gsub("<href>", "", ilink)
				ilink <- gsub("</href>", "", ilink)
				ilink <- gsub("\\s","", ilink)
				ilink <- paste(ilink, "&.colorBar=", colorbar, sep="")
				idest <- gsub(".kml", ".png", dest)
				download.file(ilink, idest, mode = "wb")
				kmlfile[gg] <- paste("<href>", tail(unlist(strsplit(idest, "/")), 1) , "</href>", sep="")
				kmlfile <- c(kmlfile[1:(gg-3)], 
                             paste("<name>", variable, " at ", depth[1], " meters", "</name>", sep=""),
				             paste("<description>", colorbar, "</description>", sep=""), 
				             kmlfile[(gg-2):length(kmlfile)])
			}
			writeLines(kmlfile, dest)
		}
    }
return(list(folder=folder, nday=every.day, datesteps=datesteps, nodataflag=0))
}

