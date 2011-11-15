get.avhrr.sst <-
function (track, folder = tempdir(), 
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

