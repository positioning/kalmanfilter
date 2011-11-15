kfsst <-
function (data, fix.first = TRUE, fix.last = TRUE, 
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

