prepit <- function (track, fix.first, fix.last, scan = TRUE, window = c(0.05, 
    0.01), tmpfile = "input.dat", datfile = "ukf.dat", keepfiles = c(FALSE, 
    FALSE), internal = TRUE, sst = NULL, from.ystr = c(3, 6), 
    from.dstr = c(7, 9), to.ystr = c(11, 14), to.dstr = c(15, 
        17), localsstfolder = NULL) 
{
    oldDigits <- options()$digits
    options(digits = 22)
    xx <- as.numeric(mdy.date(track$month, track$day, track$year)) + 
        track$hour/24 + track$min/24/60 + track$sec/24/60/60
    if (any(diff(xx) < 0)) 
        stop("Dates (including times) are not sorted in increasing order.")
    if ((!internal) & (!keepfiles[2])) 
        stop("Either 'internal' or 'keepfiles[2]' has to be TRUE, or nothing is returned")
    cat("Writing to temp files ... ")
    ret <- list()
    ret$fix.first <- fix.first[1:8]
    if (length(fix.first) == 11) {
        ret$fix.first.var <- matrix(fix.first[c(9, 11, 11, 10)], 
            nrow = 2, ncol = 2)
    }
    else {
        ret$fix.first.var <- matrix(0, nrow = 2, ncol = 2)
    }
    ret$fix.last <- fix.last[1:8]
    if (length(fix.last) == 11) {
        ret$fix.last.var <- matrix(fix.last[c(9, 11, 11, 10)], 
            nrow = 2, ncol = 2)
    }
    else {
        ret$fix.last.var <- matrix(0, nrow = 2, ncol = 2)
    }
    fix.first[1] <- (360 - fix.first[1])%%360
    fix.last[1] <- (360 - fix.last[1])%%360
    ret$scan <- scan
    ret$window <- window
    vec2str <- function(x) paste(x, collapse = " ")
    ret$prehead <- c("# Data file written from the trackit package", 
        "# scanIt", ifelse(scan, 1, 0), "# fixFirst", vec2str(fix.first[1:8]), 
        vec2str(ret$fix.first.var[1, ]), vec2str(ret$fix.first.var[2, 
            ]), "# fixLast", vec2str(fix.last[1:8]), vec2str(ret$fix.last.var[1, 
            ]), vec2str(ret$fix.last.var[2, ]), "# fraction of day used around each solar event", 
        vec2str(window), "# dims", nrow(track), ncol(track), 
        "# year month day hour min sec depth light temp")
    writeLines(ret$prehead, tmpfile)
    write.table(track, row.names = FALSE, col.names = FALSE, 
        file = tmpfile, append = TRUE)
    cat("# Exclude dates\n 0 \n", file = tmpfile, append = TRUE)
    cat("Done.\n")
    dirname <- paste(tempdir(), "prep", sep = "")
    dir.create(dirname)
    oldwd <- getwd()
    setwd(dirname)
    file.copy(paste(oldwd, tmpfile, sep = "/"), "input.dat", 
        TRUE)
    file.copy(paste(path.package("trackit"), "/admb/src/deltaT.dat", 
        sep = "/"), "deltaT.dat", TRUE)
    file.copy(paste(path.package("trackit"), "/admb/src/lunar.dat", 
        sep = "/"), "lunar.dat", TRUE)
    cat("# dummy file", file = "prepro.dat")
    if (.Platform$OS.type == "windows") {
        file.copy(paste(path.package("trackit"), "/admb/bin/prepro.exe", 
            sep = "/"), "prepro.exe", TRUE)
        error.code <- .sys("prepro.exe")
    }
    else {
        file.copy(paste(path.package("trackit"), "/admb/bin/prepro", 
            sep = "/"), "prepro", TRUE)
        .sys("chmod u+x prepro")
        error.code <- .sys(paste("./prepro", sep = ""))
    }
    file.copy("ukf.dat", paste(oldwd, datfile, sep = "/"), TRUE)
    setwd(oldwd)
    unlink(dirname, TRUE)
    alllines <- readLines(datfile)
    skiplines <- grep("obsMat", alllines)
    ret$posthead <- alllines[1:skiplines]
    if (internal) {
        cat("Creating internal object ... ")
        ret$data <- matrix(scan(datfile, skip = skiplines, quiet = TRUE), 
            ncol = 13, byrow = TRUE)
        cat("Done.\n")
    }
    tail <- "\n"
    ret$has.sst <- !is.null(sst)
    if (ret$has.sst) {
        if (!is.null(localsstfolder)) {
            .sstFileVector <<- paste(localsstfolder, dir(localsstfolder), 
                sep = "/")
        }
        tail <- c(tail, "\n#Number of sst data files\n", length(.sstFileVector), 
            "\n")
        tail <- c(tail, "#List of file names\n")
        for (i in 1:length(.sstFileVector)) {
            tail <- c(tail, .sstFileVector[i], "\n")
        }
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
        datevec <- JD(middate$year, middate$month, middate$day, 
            type = "universal")
        tail <- c(tail, "#Corresponding mid-dates (rounded upwards)\n")
        tail <- c(tail, paste(datevec, collapse = " "), "\n")
        tail <- c(tail, "\n")
        sstJDU <- JD(sst[, 1], sst[, 2], sst[, 3] + sst[, 4]/24 + 
            sst[, 5]/1440 + sst[, 6]/86400, type = "universal")
        sstout <- cbind(sstJDU, sst[, 7])
        tail <- c(tail, "\n#Number of sst observations\n", nrow(sstout), 
            "\n")
        for (i in 1:nrow(sstout)) {
            tail <- c(tail, paste(sstout[i, ], collapse = " "), 
                "\n")
        }
    }
    else {
        tail <- c(tail, "\n#Number of sst data files\n", 0, "\n")
    }
    cat(tail, file = datfile, append = TRUE)
    ret$tail <- tail
    if (keepfiles[1]) {
        ret$prefile <- filepath(tmpfile)
    }
    else {
        unlink(tmpfile)
    }
    if (keepfiles[2]) {
        ret$postfile <- filepath(datfile)
    }
    else {
        unlink(datfile)
    }
    class(ret) <- "trackit.scan"
    options(digits = oldDigits)
    return(ret)
}
