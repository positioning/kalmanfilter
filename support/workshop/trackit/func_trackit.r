trackit <- function (prep.track, a2lpoints = 15, u.init = 0, v.init = 0, 
    D.init = 100, ss1.init = 1, ss2.init = 5, ss3.init = 1, rho.init = 0.01, 
    bsst.init = 0, sssst.init = 0.01, rad.init = 200, dep1.init = 0, 
    dep2.init = 0, phi.init = c(60, rep((200 - 60)/(a2lpoints - 
        1), a2lpoints - 1)), init = c(u.init, v.init, D.init, 
        ss1.init, ss2.init, ss3.init, rho.init, bsst.init, sssst.init, 
        rad.init, dep1.init, dep2.init, phi.init), u.ph = -1, 
    v.ph = -1, D.ph = 3, ss1.ph = 2, ss2.ph = 2, ss3.ph = 2, 
    rho.ph = 3, bsst.ph = -1, sssst.ph = 2, rad.ph = 3, dep1.ph = -1, 
    dep2.ph = -1, phi.ph = 1, phase = c(u.ph, v.ph, D.ph, ss1.ph, 
        ss2.ph, ss3.ph, rho.ph, bsst.ph, sssst.ph, rad.ph, dep1.ph, 
        dep2.ph, phi.ph), blue.light.only = FALSE, save.dir = NULL) 
{
    init[1] <- -init[1]
    oldDigits <- options()$digits
    options(digits = 22)
    vec2str <- function(x) paste(x, collapse = " ")
    confLines <- c("# Ponts in angle to light approximation", 
        a2lpoints, "# Parameter names u v D ss1 ss2 ss3 rho bsst sssst rad dep1 dep2 phi1 ... phiN", 
        "# Initial values", vec2str(init), "# Estimation phase", 
        vec2str(phase))
    if (is.null(save.dir)) {
        dirname <- paste(tempdir(), "run", sep = "")
    }
    else {
        dirname <- paste(save.dir, "run", sep = "")
    }
    dir.create(dirname)
    ukfdat <- paste(dirname, "ukf.dat", sep = "/")
    if (is.null(prep.track$postfile)) {
        writeLines(c("# Blue light only", ifelse(blue.light.only, 
            1, 0), prep.track$posthead), ukfdat)
        write.table(prep.track$data, row.names = FALSE, col.names = FALSE, 
            file = ukfdat, append = TRUE)
        cat(prep.track$tail, file = ukfdat, append = TRUE)
    }
    else {
        lines <- readLines(prep.track$postfile)
        writeLines(c("# Blue light only", ifelse(blue.light.only, 
            1, 0), lines), ukfdat)
    }
    oldwd <- getwd()
    setwd(dirname)
    writeLines(confLines, "model.cfg")
    cat("# Run program in simulation mode only 0=FALSE, 1=TRUE\n", 
        0, "\n", file = "sstsim.dat")
    if (.Platform$OS.type == "windows") {
        file.copy(paste(path.package("trackit"), "/admb/ukf.exe", 
            sep = ""), "ukf.exe", TRUE)
        error.code <- .sys("ukf.exe")
    }
    else {
        file.copy(paste(path.package("trackit"), "/admb/ukf", 
            sep = ""), "ukf", TRUE)
        .sys("chmod u+x ukf")
        error.code <- .sys(paste("./ukf", sep = ""))
    }
    ret <- list()
    pnames <- c("u", "v", "D", "ss1", "ss2", "ss3", "rho", "bsst", 
        "sssst", "rad", "dep1", "dep2", "qSparTilde")
    ret$init <- init
    ret$init[1] <- -ret$init[1]
    ret$phase <- phase
    ret$error.code
    mpt <- read.table("mpt.out")
    jdu2date <- function(jdu) as.date(jdu - 2436934.5)
    ret$decimal.date <- mpt$V1 - 2436934.5
    if (prep.track$has.sst) {
        sstobsstr <- (prep.track$tail[(grep("Number of sst observations", 
            prep.track$tail) + 2):length(prep.track$tail)])
        sstobsmat <- matrix(as.numeric(unlist(strsplit(sstobsstr[sstobsstr != 
            "\n"], " "))), ncol = 2, byrow = TRUE)
        sstobsmat[, 1] <- sstobsmat[, 1] - 2436934.5
        ret$sstobs <- sstobsmat
        ret$most.prob.sst <- mpt$V14
    }
    ret$date <- jdu2date(mpt$V1)
    ret$timeAL <- mpt$V1 - mpt$V1[1]
    ret$most.prob.track <- cbind(x = (360 - mpt$V8)%%360, y = mpt$V9)
    ret$var.most.prob.track <- cbind(mpt$V10, mpt$V11, mpt$V12, 
        mpt$V13)
    std <- read.table("ukf.std", header = FALSE, skip = 1)
    ret$est <- unlist(sapply(pnames, function(n) std$V3[std$V2 == 
        n]))
    if ("u" %in% names(ret$est)) {
        ret$est["u"] <- -ret$est["u"]
    }
    ret$sd <- unlist(sapply(pnames, function(n) std$V4[std$V2 == 
        n]))
    names(ret$est) <- gsub("qSparTilde", "phi", names(ret$est))
    names(ret$sd) <- gsub("qSparTilde", "phi", names(ret$sd))
    par <- as.numeric(scan("ukf.par", what = "", nlines = 1, 
        quiet = TRUE)[c(6, 11, 16)])
    ret$npar <- par[1]
    ret$nlogL <- par[2]
    ret$max.grad.comp <- par[3]
    setwd(oldwd)
    if (is.null(save.dir)) {
        unlink(dirname, TRUE)
    }
    class(ret) <- "trackit"
    options(digits = oldDigits)
    return(ret)
}
