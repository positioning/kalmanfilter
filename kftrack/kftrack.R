#Learn a little about the system  
.have.date<-function()"date"%in%.packages(TRUE)
.kfsys<-function(cmd)if(.Platform$OS.type=="windows"){shell(cmd)}else{system(cmd)}
.gmtok<-function(){
  .testgmt <- tempfile("testgmt"); on.exit(unlink(.testgmt))
  return(.kfsys(paste("gmtdefaults -D 1>",.testgmt))==0)
}

.generate.dat.file<-function(data, file="kftrack.dat", fix.first=TRUE, 
  fix.last=TRUE, 
  theta.active=c(u.active, v.active, D.active, bx.active, by.active, 
  sx.active, sy.active, a0.active, b0.active, vscale.active), 
  theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, 
  b0.init, vscale.init), u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, 
  by.active=TRUE, sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, 
  vscale.active=TRUE, u.init=0, v.init=0, D.init=100, bx.init=0, by.init=0, sx.init=.5, 
  sy.init=1.5, a0.init=0.001, b0.init=0, vscale.init=1, var.struct="solstice", dev.pen=0.0){

  "%+%"<-function(s1, s2)paste(s1, s2, sep="")
  tostr<-function(x)paste(as.numeric(x), collapse="\t")
  var.flags<-switch(var.struct, "uniform"=c(0,0,0,0), "solstice"=c(1,0,0,0), 
                    "daily"=c(0,1,dev.pen,0), "specified"=c(0,0,0,1), 
                    warning("No matching variance structure found"))

  header<-"#Auto generated data file from R-KFtrack \n#"%+%date()%+%"\n#\n"%+%
          "# Number of data points\n  "%+%
          tostr(nrow(data))%+%"\n#\n"%+%
          "# 1 if first point is true release position; 0 otherwise\n  "%+%
          tostr(fix.first)%+%"\n#\n"%+%
          "# 1 if last point is true recapture position; 0 otherwise\n  "%+%
          tostr(fix.last)%+%"\n#\n"%+%
          "# active parameters \n# u\tv\tD\tbx\tby\tsx\tsy\ta0\tb0\tvscale\n  "%+%
          tostr(theta.active)%+%"\n#\n"%+%
          "# initial values \n# u\tv\tD\tbx\tby\tsx\tsy\ta0\tb0\tvscale\n  "%+%
          tostr(theta.init)%+%"\n#\n"%+%
          "# latitude errors \n# cos\tdev\tdeviation penalty weight\tspecified flag\n  "%+%
          tostr(var.flags)%+%"\n#\n"%+%
          "# Positions \n# day\tmonth\tyear\tlong\tlati\tvlon\tvlat\tvlonlat (last three optional)\n  "
  cat(header, file=file)
  write.table(data, file=file, row.names=FALSE, col.names=FALSE, 
              sep="\t", eol="\n  ", append=TRUE)
  cat("\n", file=file, append=TRUE)
  class(header)<-"kfhead"
  return(header)
}

.read.output<-function(data, fix.first=TRUE, fix.last=TRUE, 
  theta.active=c(u.active, v.active, D.active, bx.active, by.active, 
  sx.active, sy.active, a0.active, b0.active, vscale.active), 
  theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, 
  b0.init, vscale.init), u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, 
  by.active=TRUE, sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, 
  vscale.active=TRUE, u.init=0, v.init=0, D.init=100, bx.init=0, by.init=0, sx.init=.5, 
  sy.init=1.5, a0.init=0.001, b0.init=0, vscale.init=1, var.struct="solstice", dev.pen=0.0){

  getpar<-function(what, file){
    txt<-readLines(file)
    return(as.numeric(strsplit(txt[grep(what, txt)+1], split=" ")[[1]]))
  }
  getrep<-function(what, file){
    txt<-readLines(file)
    return(as.numeric(strsplit(txt[grep(what, txt)], split=" ")[[1]][3]))
  }


  theta.names<-c("u","v","D","bx","by","sx","sy")
  if(var.struct=="solstice"){theta.names<-c(theta.names, "a0", "b0")}
  if(var.struct=="specified"){theta.names<-c(theta.names, "vscale")}

  if(file.access("kftrack.par")!=0){
    warning("File \"kftrack.par\" not found (possibly because there was no solution to minimization problem)", call.=FALSE)
    npar<-NA; nlogL<-NA; max.grad.comp<-NA; estimates<-NA
  }else{
    tmp<-as.numeric(scan("kftrack.par", what="character", 16, quiet=TRUE)[c(6,11,16)])
    npar<-tmp[1]; nlogL<-tmp[2]; max.grad.comp<-tmp[3]
    estimates<-sapply(c("uu","vv","D","bx","by","vx","vy"), getpar, file="kftrack.par")
    if(var.struct=="solstice"){estimates<-c(estimates,sapply(c("a0", "b0"), getpar, file="kftrack.par"))}
    if(var.struct=="specified"){estimates<-c(estimates,sapply(c("vscale"), getpar, file="kftrack.par"))}
    names(estimates)<-theta.names
  }

  if(file.access("kftrack.rep")!=0){
    warning("File \"kftrack.rep\" not found", call.=FALSE)
    spd<-NA; hdg<-NA
  }else{
    spd<-getrep("spd", "kftrack.rep")
    hdg<-getrep("hdg", "kftrack.rep")
  }

  if(file.access("kftrack.std")!=0){
    warning("File \"kftrack.std\" not found (possibly the hessian was not estimated)", call.=FALSE)
    std.dev<-NA    
  }else{
    dat<-read.table("kftrack.std", skip=1)
    tmp<-dat[dat[,2]%in%c("sduu","sdvv","sdD","sdbx","sdby","sdvx","sdvy"), 3:4]
    if(var.struct=="solstice"){
      if(theta.active[8]){tmp<-rbind(tmp,dat[dat[,2]=="a0",3:4])}else{tmp<-rbind(tmp,c(0, 0))}
      if(theta.active[9]){tmp<-rbind(tmp,dat[dat[,2]=="b0",3:4])}else{tmp<-rbind(tmp,c(0, 0))}
    }
    if(var.struct=="specified"){
      if(theta.active[10]){tmp<-rbind(tmp,dat[dat[,2]=="vscale",3:4])}else{tmp<-rbind(tmp,c(1, 0))}
    }
    std.dev<-tmp[,2]
    names(std.dev)<-paste("sd.",theta.names, sep="")
  }
  ta<-theta.active[1:7]
  if(var.struct=="specified"){
    ta[6:7]<-0
    estimates<-estimates[!names(estimates)%in%c('sx','sy')]  
    std.dev<-std.dev[!names(std.dev)%in%c('sd.sx','sd.sy')]  
  }
  mptfilename<-"mpt.dat"
  #mptfilename<-paste("mpt_", paste(as.numeric(
  #  c(ta,var.struct=="solstice",var.struct=="daily")), collapse=""),".dat",sep="")
  if(file.access(mptfilename)!=0){warning("File not found")}else{
    tmp<-read.table(mptfilename,skip=3, header=FALSE)
    name<-strsplit(readLines(mptfilename, 3)[3], split=" ")[[1]]  
    colnames(tmp)<-name[!name%in%c("","#")]
    
      nominal.track<-cbind(x=tmp$ox, y=tmp$oy)
      pred.track<-cbind(x=tmp$px,y=tmp$py)
      most.prob.track<-cbind(x=tmp$smoothX, y=tmp$smoothY)
      days.at.liberty<-cumsum(tmp$dt)
      date<-matrix(as.numeric(unlist(strsplit(as.character(tmp$date), "/"))), ncol=3, byrow=TRUE) 
      colnames(date)<-c("year", "month", "day")
      var.most.prob.track<-cbind(tmp$Psmooth11,tmp$Psmooth12,tmp$Psmooth21,tmp$Psmooth22)
     
  }

  return(list(npar=npar, nlogL=nlogL, max.grad.comp=max.grad.comp, estimates=estimates, 
         std.dev=std.dev, nominal.track=nominal.track, pred.track=pred.track, 
         most.prob.track=most.prob.track, var.most.prob.track=var.most.prob.track, 
         days.at.liberty=days.at.liberty, date=date, spd=spd, hdg=hdg))
}

kftrack<-function(data, fix.first=TRUE, fix.last=TRUE, 
  theta.active=c(u.active, v.active, D.active, bx.active, by.active, 
  sx.active, sy.active, a0.active, b0.active, vscale.active), 
  theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, 
  b0.init, vscale.init), u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, 
  by.active=TRUE, sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, 
  vscale.active=TRUE, u.init=0, v.init=0, D.init=100, bx.init=0, by.init=0, sx.init=.5, 
  sy.init=1.5, a0.init=0.001, b0.init=0, vscale.init=1, var.struct="solstice", dev.pen=0.0, 
  save.dir=NULL, admb.string=""){

  olddir<-getwd()
  dirname<-ifelse(is.null(save.dir),"_kftrack_temp_",save.dir) 
  dir.create(dirname)
  setwd(dirname)
    if(!is.null(save.dir)){
      mptfilename<-paste("mpt_", paste(as.numeric(c(theta.active[1:7],
                         var.struct=="solstice",var.struct=="daily")), collapse=""),".dat",sep="")
      unlink(c(mptfilename,'kftrack.par','kftrack.rep','kftrack.std'))
    }

    header<-.generate.dat.file(data, "kftrack.dat", fix.first, fix.last, theta.active, 
                               theta.init, var.struct=var.struct, dev.pen=dev.pen)

    filename<-dir(paste(.path.package("kftrack"),"/admb", sep=""), pattern="kf")			       
    if(.Platform$OS.type=="windows"){
      error.code<-system(paste(.path.package("kftrack"),"/admb/",filename," ",admb.string, sep=""))
    }else{
      system(paste("cp ",.path.package("kftrack"),"/admb/",filename," ",filename, sep=""))
      error.code<-system(paste("./",filename," ",admb.string, sep=""))
    }
    
    kf.o<-.read.output(data, fix.first, fix.last, theta.active, theta.init, 
                       var.struct=var.struct, dev.pen=dev.pen)
    kf.o$nobs<-nrow(data); kf.o$header<-header; kf.o$fix.first<-fix.first; 
    kf.o$fix.last<-fix.last;kf.o$theta.active<-theta.active; kf.o$theta.init<-theta.init;
    kf.o$var.struct<-var.struct; kf.o$dev.pen<-dev.pen; kf.o$call<-match.call();
    kf.o$data.name<-deparse(substitute(data))
    kf.o$error.code<-error.code
  setwd(olddir)
  if(is.null(save.dir)){unlink(dirname, recursive = TRUE)}
  class(kf.o)<-"kftrack"
  return(kf.o)
}

print.kfhead<-function(x,...){cat(x)}

print.kftrack<-function(x,...){ 
  headvec<-strsplit(x$header, split="\n")[[1]]
  "%+%"<-function(s1, s2)paste(s1, s2, sep="")
  out<-"\n\n#R-KFtrack fit\n"%+%headvec[2]%+%"\n"%+%
       "#Number of observations: "%+%x$nobs%+%"\n"%+%
       "#Negative log likelihood: "%+%x$nlog%+%"\n"%+% 
       "#The convergence criteria was "%+%
       ifelse(abs(x$max.grad.comp)>1.0e-4, "NOT ","")%+%"met\n\n"
  cat(out)
  if(abs(x$max.grad.comp)<1.0e-4){
    cat("Parameters:\n")
    print(rbind("Estimates:"=x$estimates,"Std. dev.:"=x$std.dev))

    cat("\nThis object contains the following sub-items:\n")
    print(names(x))  
  }
}

.CI.reg<-function (x, level = 0.95, npoints = 100, col="blue", border=0, density=20, lwd=0.1*par("lwd"), ...){ 
    #This function is partly stolen from the 'ellipse' package.
    t.quan<-sqrt(qchisq(level, 2))
    centre<-x[5:6]
    x<-matrix(x[1:4], 2,2)
    r <- x[1,2]
    scale <- sqrt(diag(x))
    if (scale[1] > 0){r <- r/scale[1]}
    if (scale[2] > 0){r <- r/scale[2]}
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    polygon(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1], t.quan * scale[2] *
            cos(a - d/2) + centre[2]), npoints, 2), 
            col=col,border=border, density=density, lwd=lwd,...)
  }

.addrange<-function(x, pct=.05){
  minx<-min(x, na.rm=TRUE); maxx<-max(x, na.rm=TRUE); dif<-maxx-minx
  c(minx,maxx)+c(-1,1)*dif*pct
}


.plot1<-function(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE,...){
  mypch<-c(ifelse(x$fix.first,6,4), rep(4,x$nobs-2), ifelse(x$fix.last,2,4))
  xlow<-x$most.prob.track[,"x"]-2*sqrt(x$var.most.prob.track[,1])
  xhig<-x$most.prob.track[,"x"]+2*sqrt(x$var.most.prob.track[,1])
  xrange<-.addrange(c(if(points){x$nominal.track[,"x"]}, if(pred){x$pred.track[,"x"]}, if(most){x$most.prob.track[,"x"]}, if(ci){c(xlow,xhig)}))
  plot(x$days.at.liberty, x$nominal.track[,"x"], ylab="Longitude", pch=mypch, 
       ylim=xrange, xlab="Days at liberty", type=ifelse(points,"b","n"), col=gray(.9))
  if(points){points(x$days.at.liberty, x$nominal.track[,"x"], pch=mypch)}
  if(pred){lines(x$days.at.liberty, x$pred.track[,"x"], col="green")}
  if(most){lines(x$days.at.liberty, x$most.prob.track[,"x"], col="blue")}
  if(ci){
    lines(x$days.at.liberty, xlow, col="blue", lty="dashed")
    lines(x$days.at.liberty, xhig, col="blue", lty="dashed")
  }
}

.plot2<-function(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE,...){
  mypch<-c(ifelse(x$fix.first,6,4), rep(4,x$nobs-2), ifelse(x$fix.last,2,4))
  ylow<-x$most.prob.track[,"y"]-2*sqrt(x$var.most.prob.track[,4])
  yhig<-x$most.prob.track[,"y"]+2*sqrt(x$var.most.prob.track[,4])
  yrange<-.addrange(c(if(points){x$nominal.track[,"y"]}, if(pred){x$pred.track[,"y"]}, if(most){x$most.prob.track[,"y"]}, if(ci){c(ylow,yhig)}))
  plot(x$days.at.liberty, x$nominal.track[,"y"], ylab="Latitude", pch=mypch, ylim=yrange, 
       las=3, xlab="", axes=FALSE, type=ifelse(points,"b","n"), col=gray(.9))
  if(points){points(x$days.at.liberty, x$nominal.track[,"y"], pch=mypch)}
  if(pred){lines(x$days.at.liberty, x$pred.track[,"y"], col="green")}
  if(most){lines(x$days.at.liberty, x$most.prob.track[,"y"], col="blue")}
  if(ci){
    lines(x$days.at.liberty, ylow, col="blue", lty="dashed")
    lines(x$days.at.liberty, yhig, col="blue", lty="dashed")
  }
}

.plot3<-function(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE, map=FALSE, res=3,...){
  gmt <- function(x1,x2,y1,y2,resolution=3) {
    ##############################################
    ## get mapping polygons from pscoast
    ##############################################
    read.ps.line<-function(txt){
      ##############################################
      ## Extract the stuff needed from a line 
      ##############################################  
      txt.split<-strsplit(txt, split=" ")[[1]]
      ret<-c(NA,NA)
      if(length(txt.split)==3){
        if(txt.split[3]%in%c("M","moveto","D")){ret<-as.numeric(txt.split[1:2])}
      }
      return(ret)
    }
    if(resolution<1 || resolution>5) stop("resolution from 1 (full) to 5 (crude)")
    res <- c("f","h","i","l","c")[resolution]
    filen <-tempfile("gmtmap")
    on.exit(unlink(filen))
    cmd <<- paste("pscoast -R",x1,"/",x2,"/",y1,"/",y2," -Jx2id -P -G0 -D",res," -X0 -Y0 >",filen,sep="")
    .kfsys(cmd)
    txt<-readLines(filen)
    mat<-matrix(unlist(lapply(txt, read.ps.line)),ncol=2, byrow=TRUE)
    for(i in 2:nrow(mat)){if(!is.na(mat[i,1])&!is.na(mat[i-1,1])){mat[i,]<-mat[i,]+mat[i-1,]}}  
    mat[,1]<-mat[,1]/600+x1
    mat[,2]<-mat[,2]/600+y1		
    return(mat)
  }
  
  mypch<-c(ifelse(x$fix.first,6,4), rep(4,x$nobs-2), ifelse(x$fix.last,2,4))
  xlow<-x$most.prob.track[,"x"]-2*sqrt(x$var.most.prob.track[,1])
  xhig<-x$most.prob.track[,"x"]+2*sqrt(x$var.most.prob.track[,1])
  ylow<-x$most.prob.track[,"y"]-2*sqrt(x$var.most.prob.track[,4])
  yhig<-x$most.prob.track[,"y"]+2*sqrt(x$var.most.prob.track[,4])
  xrange<-.addrange(c(if(points){x$nominal.track[,"x"]}, if(pred){x$pred.track[,"x"]}, if(most){x$most.prob.track[,"x"]}, if(ci){c(xlow,xhig)}))
  yrange<-.addrange(c(if(points){x$nominal.track[,"y"]}, if(pred){x$pred.track[,"y"]}, if(most){x$most.prob.track[,"y"]}, if(ci){c(ylow,yhig)}))
  plot(NA,NA, xlab="Longitude", ylab="Latitude", xlim=xrange, ylim=yrange, type="n")
  if(map){
    if(.gmtok()){
      xxyy<-par("usr"); x1<-xxyy[1]; x2<-xxyy[2]; y1<-xxyy[3]; y2<-xxyy[4];
      junk <- gmt(x1,x2,y1,y2,resolution=res)
      rect(x1,y1,x2,y2, col="lightblue")
      polygon(junk,border="darkgreen",col="darkgreen")
    }else{stop("GMT is not installed")}
  }
  if(points){
    par(new=TRUE)
    plot(x$nominal.track, pch=mypch, xlab="Longitude", ylab="Latitude", xlim=xrange, 
       ylim=yrange, type="b", col=gray(.9))
    points(x$nominal.track , pch=mypch)
  }
  if(ci){apply(cbind(x$var.most.prob.track, x$most.prob.track),1,.CI.reg)}
  if(pred){lines(x$pred.track, col="green")}
  if(most){lines(x$most.prob.track, col="blue")}
}

plot.kftrack<-function(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE, gmt=FALSE,...){
  par(mar=c(4, 4, 5, 1) + 0.1)
  layout(matrix(c(1,3,2,3),2,2,byrow=TRUE))
  .plot1(x=x, ci=ci, points=points, pred=pred, most=most, ...)
  xp<-par("xaxp"); xcut<-seq(xp[1], xp[2], length=xp[3]+1) 
  par(mar=c(6, 4, 3, 1) + 0.1)
  .plot2(x=x, ci=ci, points=points, pred=pred, most=most, ...)
  if(.have.date()){
    firstdate<-mdy.date(x$date[1,"month"], x$date[1,"day"], x$date[1,"year"])
    axis(2)
    axis(1, at=xcut, labels=paste(firstdate+xcut), las=2)
    box()
    mtext("Date", side=1, line=5, cex=par("cex"))
  }else{
    axis(1); axis(2); box() 
    mtext("Days at liberty", side=1, line=3, cex=par("cex"))
  }
  par(mar=c(4, 4, 5, 1) + 0.1)
  .plot3(x=x, ci=ci, points=points, pred=pred, most=most, ...) 
  title(paste("Estimated track of", x$data.name), outer=TRUE,line=-1.5)
  if(gmt){
    .gmt.plot(x, map.width = 5, ci = ci, points = points, pred = pred, most = most, ...)
  }
}

addmap <- function(kf.obj, res=3, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE,...) {
  par(mar=c(4, 4, 5, 1) + 0.1)
  layout(matrix(c(1,3,2,3),2,2,byrow=TRUE))
  .plot1(x=kf.obj, ci=ci, points=points, pred=pred, most=most, ...)
  xp<-par("xaxp"); xcut<-seq(xp[1], xp[2], length=xp[3]+1) 
  par(mar=c(6, 4, 3, 1) + 0.1)
  .plot2(x=kf.obj, ci=ci, points=points, pred=pred, most=most, ...)
  if(.have.date()){
    firstdate<-mdy.date(kf.obj$date[1,"month"], kf.obj$date[1,"day"], kf.obj$date[1,"year"])
    axis(2)
    axis(1, at=xcut, labels=paste(firstdate+xcut), las=2)
    box()
    mtext("Date", side=1, line=5, cex=par("cex"))
  }else{
    axis(1); axis(2); box() 
    mtext("Days at liberty", side=1, line=3, cex=par("cex"))
  }
  par(mar=c(4, 4, 5, 1) + 0.1)
  .plot3(x=kf.obj, ci=ci, points=points, pred=pred, most=most, map=TRUE, res=res, ...) 
  title(paste("Estimated track of", kf.obj$data.name), outer=TRUE,line=-1.5)
}

addcoast<-function(res=3, ...) {
  if(.gmtok()){
    usr<-par("usr"); x1<-usr[1]; x2<-usr[2]; y1<-usr[3]; y2<-usr[4];
    if(res<1 || res>5) stop("resolution from 1 (full) to 5 (crude)")
    res <- c("f","h","i","l","c")[res]
    filen <- tempfile("gmtmap"); on.exit(unlink(filen))
    cmd <- paste("pscoast -R",x1,"/",x2,"/",y1,"/",y2," -W -M  -D",res," >",filen,sep="")
    .kfsys(cmd)
    dat<-readLines(filen)
    ff<-function(str)if(regexpr("#",str)>0){
      c(NA, NA)
    }else{ 
      as.numeric(strsplit(str, split="\t")[[1]])
    }
    lines(t(sapply(dat, ff)), ...)
  }else{stop("GMT is not installed")}
}

.First.lib<-function(lib, pkg){
  if(.have.date())library(date)
  if(exists('.serverGlobalWD')){
    assign('.tagListFile',paste(.serverGlobalWD,"/tagListBackup.RData",sep=""), envir=as.environment(grep("kftrack",search())))
  }else{
    assign('.tagListFile',"tagListBackup.RData", envir=as.environment(grep("kftrack",search())))
  }
  if(file.exists(.tagListFile)){
    load(.tagListFile, envir=as.environment(grep("kftrack",search())))
  }else{
    assign(".tagList",list(), envir=as.environment(grep("kftrack",search())))
  }
  save(.tagList,file=.tagListFile, envir=as.environment(grep("kftrack",search())))
}

upload.track<-function(fit){
  server.str<-"https://www.soest.hawaii.edu/tag-data/metacat?action=kfsst"
  cat("\nPlease fill in a little information about the track (hit <return> after each field)\n\n")
  dataid<-readline("Track indentification name (use something unique e.g. \"species.tagID\"):\n")
  cat("\n")
  email<-readline("Contact information. Type in your email address:\n")     
  cat("\n")
  extras<-readline("Addidional infomation (tag type, notes, species, ...):\n")
  cat("\n")
  key<-readline("Type in a password (needed if you want to revise or remove the track):\n")
  fit$description<-paste("ID: ",dataid,"<br>Contact: ",email,"<br>Notes: ",extras,sep="")
  fit$data.name<-dataid
  fit$key<-key
  save(fit,file="track4upload.RData")
  path<-normalizePath("track4upload.RData")
  browseURL(server.str)
  cat("\n\n")
  cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
  cat("Now you can upload your track to the server by going to the page:\n\n")
  cat(server.str,"\n\n")
  cat("and paste in the following file path:\n\n")
  cat(" ----> ",path,"\n\nPress <upload> and you are done.\n\n")
  cat(paste(rep("=", options()$width/2-6), collapse = ""))
  cat(" Thank you! ")
  cat(paste(rep("=", options()$width/2-6), collapse = ""),"\n\n")
}

.add.fit.to.server.list<-function(fit){
  fun<-function(x)(x$data.name==fit$data.name)&(x$key!=fit$key)
  condi<-any(unlist(lapply(.tagList,fun)))
  if(condi){
    stop("TrackID and key does not match. \nEither you are using a name someone else has already used \nor you are trying to revise or remove a track, but not using the correct key")  
  }else{
    kfenv=as.environment(grep("kftrack",search()))
    eval(substitute(.tagList$name<-fitlocal,list(name=fit$data.name, fitlocal=fit)), envir=kfenv)
    idx<-!unlist(lapply(.tagList,function(ll)is.null(ll$nobs)))
    assign(".tagList",.tagList[idx], envir=kfenv)
    save(.tagList,file=.tagListFile,envir=kfenv)
  }
}

write.kml<-function(fit, description="", file='track.kml', npoints=20, level=0.95){
  fit$most.prob.track[,1]<-(fit$most.prob.track[,1]+180)%%360-180
  fit$pred.track[,1]<-(fit$pred.track[,1]+180)%%360-180
  fit$nominal.track[,1]<-(fit$nominal.track[,1]+180)%%360-180
  "%+%" <- function(s1, s2) paste(s1, s2, sep = "")
  l<-function(x)length(x)+1
  sl<-list()
  sl[1]<-"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  sl[l(sl)]<-"<kml xmlns=\"http://earth.google.com/kml/2.0\">\n"
  sl[l(sl)]<-"<Folder>\n"
  sl[l(sl)]<-"<name>"%+%fit$data.name%+%"</name>\n"
  sl[l(sl)]<-"<description>"%+%description%+%"</description>\n"
  sl[l(sl)]<-"<Placemark>\n"
  sl[l(sl)]<-"<name>"%+%"Most probable track"%+%"</name>\n"
  sl[l(sl)]<-"<description>"%+%"Most probable track"%+%"</description>\n"
  sl[l(sl)]<-"<LookAt>\n"
  sl[l(sl)]<-"<longitude>"%+%mean(fit$most[1])%+%"</longitude>\n"
  sl[l(sl)]<-"<latitude>"%+%mean(fit$most[,2])%+%"</latitude>\n"
  sl[l(sl)]<-"<range>"%+%2000000%+%"</range>\n"
  sl[l(sl)]<-"<tilt>"%+%0%+%"</tilt>\n"
  sl[l(sl)]<-"<heading>"%+%0%+%"</heading>\n"
  sl[l(sl)]<-"</LookAt>\n"
  sl[l(sl)]<-"<visibility>"%+%1%+%"</visibility>\n"
  sl[l(sl)]<-"<open>"%+%0%+%"</open>\n"
  sl[l(sl)]<-"<Style>\n"
  sl[l(sl)]<-"<LineStyle>\n"
  sl[l(sl)]<-"<color>"%+%"ff0000ff"%+%"</color>\n"
  sl[l(sl)]<-"<width>"%+%2%+%"</width>\n"
  sl[l(sl)]<-"</LineStyle>\n"
  sl[l(sl)]<-"</Style>\n"
  sl[l(sl)]<-"<LineString>\n"
  sl[l(sl)]<-"<extrude>"%+%1%+%"</extrude>\n"
  sl[l(sl)]<-"<tessellate>"%+%1%+%"</tessellate>\n"
  sl[l(sl)]<-"<altitudeMode>"%+%"relativeToGround"%+%"</altitudeMode>\n"
  sl[l(sl)]<-"<coordinates>\n"
  sl[l(sl)]<-paste(fit$most[,1],fit$most[,2],rep('0\n',nrow(fit$most)), sep=',', collapse="")     
  sl[l(sl)]<-"</coordinates>\n"
  sl[l(sl)]<-"</LineString>\n"
  sl[l(sl)]<-"</Placemark>\n"
  sl[l(sl)]<-"<Placemark>\n"
  sl[l(sl)]<-"<name>"%+%"Predicted track"%+%"</name>\n"
  sl[l(sl)]<-"<description>"%+%"Predicted track"%+%"</description>\n"
  sl[l(sl)]<-"<visibility>"%+%0%+%"</visibility>\n"
  sl[l(sl)]<-"<open>"%+%0%+%"</open>\n"
  sl[l(sl)]<-"<Style>\n"
  sl[l(sl)]<-"<LineStyle>\n"
  sl[l(sl)]<-"<color>"%+%"ff00ff00"%+%"</color>\n"
  sl[l(sl)]<-"<width>"%+%2%+%"</width>\n"
  sl[l(sl)]<-"</LineStyle>\n"
  sl[l(sl)]<-"</Style>\n"
  sl[l(sl)]<-"<LineString>\n"
  sl[l(sl)]<-"<extrude>"%+%1%+%"</extrude>\n"
  sl[l(sl)]<-"<tessellate>"%+%1%+%"</tessellate>\n"
  sl[l(sl)]<-"<altitudeMode>"%+%"relativeToGround"%+%"</altitudeMode>\n"
  sl[l(sl)]<-"<coordinates>\n"
  sl[l(sl)]<-paste(fit$pred[,1],fit$pred[,2],rep('0\n',nrow(fit$pred)), sep=',', collapse="")     
  sl[l(sl)]<-"</coordinates>\n"
  sl[l(sl)]<-"</LineString>\n"
  sl[l(sl)]<-"</Placemark>\n"

  sl[l(sl)]<-"<Placemark>\n"
  sl[l(sl)]<-"<name>"%+%"Raw geolocations"%+%"</name>\n"
  sl[l(sl)]<-"<description>"%+%"Raw geolocations"%+%"</description>\n"
  sl[l(sl)]<-"<visibility>"%+%0%+%"</visibility>\n"
  sl[l(sl)]<-"<open>"%+%0%+%"</open>\n"
  sl[l(sl)]<-"<Style>\n"
  sl[l(sl)]<-"<LineStyle>\n"
  sl[l(sl)]<-"<color>"%+%"ffff0000"%+%"</color>\n"
  sl[l(sl)]<-"<width>"%+%2%+%"</width>\n"
  sl[l(sl)]<-"</LineStyle>\n"
  sl[l(sl)]<-"</Style>\n"
  sl[l(sl)]<-"<LineString>\n"
  sl[l(sl)]<-"<extrude>"%+%1%+%"</extrude>\n"
  sl[l(sl)]<-"<tessellate>"%+%1%+%"</tessellate>\n"
  sl[l(sl)]<-"<altitudeMode>"%+%"relativeToGround"%+%"</altitudeMode>\n"
  sl[l(sl)]<-"<coordinates>\n"
  sl[l(sl)]<-paste(fit$nominal.track[,1],fit$nominal.track[,2],rep('0\n',nrow(fit$nominal.track)), sep=',', collapse="") 
  sl[l(sl)]<-"</coordinates>\n"
  sl[l(sl)]<-"</LineString>\n"
  sl[l(sl)]<-"</Placemark>\n"

  sl[l(sl)]<-"<Placemark>\n"
  sl[l(sl)]<-"<name>"%+%"Confidence region"%+%"</name>\n"
  sl[l(sl)]<-"<description>"%+%"Confidence region"%+%"</description>\n"
  sl[l(sl)]<-"<visibility>"%+%0%+%"</visibility>\n"
  sl[l(sl)]<-"<open>"%+%0%+%"</open>\n"
  sl[l(sl)]<-"<Style>\n"
  sl[l(sl)]<-"<PolyStyle>\n"
  sl[l(sl)]<-"<color>"%+%"880000ff"%+%"</color>\n"
  sl[l(sl)]<-"<fill>"%+%0%+%"</fill>\n"
  sl[l(sl)]<-"<outline>"%+%1%+%"</outline>\n"
  sl[l(sl)]<-"</PolyStyle>\n"
  sl[l(sl)]<-"</Style>\n"
  sl[l(sl)]<-"<MultiGeometry>\n"  
  cip <- apply(cbind(fit$var.most.prob.track, fit$most.prob.track), 1, .getCI, level = level, npoints = npoints)
  cip<-round(cip,3)
  for(i in  1:ncol(cip)){
    sl[l(sl)]<-"<Polygon>\n"
    sl[l(sl)]<-"<altitudeMode>"%+%"relativeToGround"%+%"</altitudeMode>\n"
    sl[l(sl)]<-"<outerBoundaryIs>\n<LinearRing>\n<coordinates>\n"
    sl[l(sl)]<-paste(cip[1:npoints,i],cip[(npoints+1):(2*npoints),i],rep('0\n',nrow(cip)), sep=',', collapse="")   
    sl[l(sl)]<-"</coordinates>\n</LinearRing>\n</outerBoundaryIs>\n</Polygon>\n"
  }
  sl[l(sl)]<-"</MultiGeometry>\n"
  sl[l(sl)]<-"</Placemark>\n"

  if(fit$fix.first){
    sl[l(sl)]<-"<Placemark>\n"
    sl[l(sl)]<-"<name>"%+%"Release point"%+%"</name>\n"
    sl[l(sl)]<-"<description>"%+%"Release point"%+%"</description>\n"
    sl[l(sl)]<-"<visibility>"%+%1%+%"</visibility>\n"
    sl[l(sl)]<-"<open>"%+%0%+%"</open>\n"
    sl[l(sl)]<-"<Point>\n"  
    sl[l(sl)]<-"<extrude>"%+%1%+%"</extrude>\n"
    sl[l(sl)]<-"<tessellate>"%+%1%+%"</tessellate>\n"
    sl[l(sl)]<-"<altitudeMode>"%+%"relativeToGround"%+%"</altitudeMode>\n"
    sl[l(sl)]<-"<coordinates>\n"
    sl[l(sl)]<-paste(fit$nominal.track[1,1],fit$nominal.track[1,2],'0\n', sep=',') 
    sl[l(sl)]<-"</coordinates>\n"
    sl[l(sl)]<-"</Point>\n"
    sl[l(sl)]<-"</Placemark>\n"
  }

  if(fit$fix.last){
    sl[l(sl)]<-"<Placemark>\n"
    sl[l(sl)]<-"<name>"%+%"Recapture point"%+%"</name>\n"
    sl[l(sl)]<-"<description>"%+%"Recapture point"%+%"</description>\n"
    sl[l(sl)]<-"<visibility>"%+%1%+%"</visibility>\n"
    sl[l(sl)]<-"<open>"%+%0%+%"</open>\n"
    sl[l(sl)]<-"<Point>\n"  
    sl[l(sl)]<-"<extrude>"%+%1%+%"</extrude>\n"
    sl[l(sl)]<-"<tessellate>"%+%1%+%"</tessellate>\n"
    sl[l(sl)]<-"<altitudeMode>"%+%"relativeToGround"%+%"</altitudeMode>\n"
    sl[l(sl)]<-"<coordinates>\n"
    n<-nrow(fit$nominal.track)
    sl[l(sl)]<-paste(fit$nominal.track[n,1],fit$nominal.track[n,2],'0\n', sep=',') 
    sl[l(sl)]<-"</coordinates>\n"
    sl[l(sl)]<-"</Point>\n"
    sl[l(sl)]<-"</Placemark>\n"
  }

  sl[l(sl)]<-"</Folder>\n"
  sl[l(sl)]<-"</kml>\n"
  cat(unlist(sl), file=file)
}


write.html<-function(fitlist, description=rep("",length(fitlist)), file='track.html', npoints=20, level=0.95, key="none"){
  thin.fit<-function(fit, maxobs=80){
    ret<-fit
    ret$oldfit<-fit
    idx<-unique(round(seq(1,fit$nobs, length=maxobs)))
    ret$nobs<-length(idx)
    ret$nominal.track<-ret$nominal.track[idx,]
    ret$pred.track<-ret$pred.track[idx,]
    ret$most.prob.track<-ret$most.prob.track[idx,]
    ret$var.most.prob.track<-ret$var.most.prob.track[idx,]
    ret$days.at.liberty<-ret$days.at.liberty[idx]
    ret$date<-ret$date[idx,]
    return(ret)
  }
  if(class(fitlist)%in%c('kftrack','kfsst')){
    fitlist<-list(fitlist)
    description=description[1]
  }
  fun<-function(fit){
    fit<-thin.fit(fit)
    fit$most.prob.track[,1]<-(fit$most.prob.track[,1]+180)%%360-180
    fit$pred.track[,1]<-(fit$pred.track[,1]+180)%%360-180
    fit$nominal.track[,1]<-(fit$nominal.track[,1]+180)%%360-180
    return(fit)
  }
  fitlist<-lapply(fitlist, fun)  
  for(i in 1:length(fitlist)){
    thisfit<-fitlist[[i]]
    if(!is.null(thisfit$description))description[i]<-thisfit$description
  }

  "%+%" <- function(s1, s2) paste(s1, s2, sep = "")
  l<-function(x)length(x)+1
  writetwocol<-function(mat){
    return(paste('\t\t',mat[,1],', ',mat[,2], rep(c(',',''),c(nrow(mat)-1,1)),'\n', sep="", collapse=""))
  }
  sl<-list()
  sl[1]<-"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:v=\"urn:schemas-microsoft-com:vml\">
  <head>
    <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\"/>
    <title>Google Maps of Tracks fitted with kftrack or kfsst</title>
    <style type=\"text/css\">
    v\\:* {
      behavior:url(#default#VML);
    }
    </style>
"
    if(key=="none"){
      sl[l(sl)]<-"<script src=\"http://maps.google.com/maps?file=js\" type=\"text/javascript\"></script>\n"
      cat("Remember to get your own Google key before publishing this page. \nYou can get it at http://www.google.com/apis/maps/signup.html\n")
    }else{
      sl[l(sl)]<-"<script src=\"http://maps.google.com/maps?file=api&v=2&key="%+%key%+%"\" type=\"text/javascript\"></script>"
    }


  sl[l(sl)]<-"
    <script type=\"text/javascript\">

    var map;    
    var tags = [];
    var tag = [];
    var obsPoints = [];
    var predPoints = [];
    var mptPoints = [];

    function createMarker(point,html) {
      // FF 1.5 fix
      html = '<div style=\"white-space:nowrap;\">' + html + '</div>';
      var marker = new GMarker(point);
      GEvent.addListener(marker, \"click\", function() {
        marker.openInfoWindowHtml(html);
      });
      return marker;
    }
"

  for(i in 1:length(fitlist)){
    fit<-fitlist[[i]]
    sl[l(sl)]<-"tag=[];\n"
    sl[l(sl)]<-"tag[0] = \"" %+% fit$data.name %+% "\";\n"
    sl[l(sl)]<-"tag[1] = [0,0,0,0,0,0];\n"

    sl[l(sl)]<-"var obs = [\n"
    sl[l(sl)]<-writetwocol(fit$nominal.track)
    sl[l(sl)]<-"];\n"
    sl[l(sl)]<-"
      obsPoints = [];
      for (i=1;i<obs.length;i+=2)obsPoints.push(new GPoint(obs[i-1],obs[i]));
      tag[2] = new GPolyline(obsPoints,\"#00ff00\",5,.3);
    "

    sl[l(sl)]<-"var pred = [\n"
    sl[l(sl)]<-writetwocol(fit$pred.track)
    sl[l(sl)]<-"];\n"
    sl[l(sl)]<-"
      predPoints = [];
      for (i=1;i<pred.length;i+=2)predPoints.push(new GPoint(pred[i-1],pred[i]));
      tag[3] = new GPolyline(predPoints,\"#0000ff\",5,.3);
    "

    sl[l(sl)]<-"var mpt = [\n"
    sl[l(sl)]<-writetwocol(fit$most.prob.track)
    sl[l(sl)]<-"];\n"
    sl[l(sl)]<-"
      mptPoints = [];
      for (i=1;i<mpt.length;i+=2)mptPoints.push(new GPoint(mpt[i-1],mpt[i]));
      tag[4] = new GPolyline(mptPoints,\"#ff0000\",5,.8);
    "

    sl[l(sl)]<-"var ci = [];\n"
    cip <- apply(cbind(fit$var.most.prob.track, fit$most.prob.track), 1, .getCI, level = level, npoints = npoints)
    cip<-round(cip,3)
    for(j in  1:ncol(cip)){
      sl[l(sl)]<-"ci.push([\n"
      sl[l(sl)]<-paste(cip[1:npoints,j],',',cip[(npoints+1):(2*npoints),j],rep(c(',\n','\n'),c(npoints-1,1)), 
                       sep='', collapse="")   
      sl[l(sl)]<-"]);\n"
    }
    sl[l(sl)]<-"
      var ciPoly = [];
      for (i=0;i<ci.length;i++){
        var temp = [];
        for (j=1;j<ci[i].length;j+=2)temp.push(new GPoint(ci[i][j-1],ci[i][j]));
        ciPoly.push(new GPolyline(temp,\"#888888\",5,.3));
      }
      tag[5] = ciPoly;
    "   

    sl[l(sl)]<-"var releasePoint = new GPoint(" %+% paste(fit$nominal.track[1,], collapse=",") %+% ");\n"
    mytext<- description[i] 
    sl[l(sl)]<-"tag[6] = createMarker(releasePoint, \"" %+% mytext %+% "\");\n"

    sl[l(sl)]<-"var recapturePoint = new GPoint(" %+% paste(fit$nominal.track[fit$nobs,], collapse=",") %+% ");\n"
    sl[l(sl)]<-"tag[7] = createMarker(recapturePoint, \"" %+% fit$data.name %+% "\");\n"

    sl[l(sl)]<-"tags.push(tag);\n"
  }

  sl[l(sl)]<-"
    function clickIt(tag,index){
      if(tag[1][index]==0){
        tag[1][index]=1;
        if(index==3){
          for (i=0;i<tag[index+2].length;i++)map.addOverlay(tag[index+2][i]);        
        }else{
          map.addOverlay(tag[index+2]);
        }
      }else{
        tag[1][index]=0;
        if(index==3){
          for (i=0;i<tag[index+2].length;i++)map.removeOverlay(tag[index+2][i]);
        }else{
          map.removeOverlay(tag[index+2]);
        }
      }
    }

    function onLoad() {
      var sidebar_html = \"\";
      map = new GMap(document.getElementById(\"map\"));
      map.setMapType(G_HYBRID_TYPE);
      map.addControl(new GLargeMapControl());
      map.addControl(new GMapTypeControl());
"
  centerPoint<-c(-180,0)#colMeans(matrix(unlist(lapply(fitlist, function(fit)fit$nominal.track[1,])),ncol=2, byrow=TRUE))
  sl[l(sl)]<-"map.centerAndZoom(new GPoint("%+%paste(centerPoint,collapse=",")%+%"), 15);\n"
  for(i in 0:(length(fitlist)-1)){
    sl[l(sl)]<-"sidebar_html += tags["%+%i%+%"][0] + ': ';\n"
    sl[l(sl)]<-"sidebar_html += '<a href=\"javascript:clickIt(tags["%+%i%+%"],0)\">' + 'obs' + '</a> ';\n"	 
    sl[l(sl)]<-"sidebar_html += '<a href=\"javascript:clickIt(tags["%+%i%+%"],1)\">' + 'pred' + '</a> ';\n"	 
    sl[l(sl)]<-"sidebar_html += '<a href=\"javascript:clickIt(tags["%+%i%+%"],2)\">' + 'mpt' + '</a> ';\n"	 
    sl[l(sl)]<-"sidebar_html += '<a href=\"javascript:clickIt(tags["%+%i%+%"],3)\">' + 'ci' + '</a> ';\n"	 
    sl[l(sl)]<-"sidebar_html += '<a href=\"javascript:clickIt(tags["%+%i%+%"],4)\">' + 'release' + '</a> ';\n"
    if(fitlist[[i+1]]$fix.last){
      sl[l(sl)]<-"sidebar_html += '<a href=\"javascript:clickIt(tags["%+%i%+%"],5)\">' + 'recapture' + '</a> ';\n"
    }
    sl[l(sl)]<-"sidebar_html += '<br>';\n"
  }
  
  sl[l(sl)]<-"
      document.getElementById(\"sidebar\").innerHTML = sidebar_html;
      for (j=0;j<tags.length;j++){
        clickIt(tags[j],4);
      }
    }
"
  sl[l(sl)]<-"
    </script>

  </head>
  <body onload=\"onLoad()\" onunload=\"GUnload()\">
      <img src=\"http://www.google.com/mapfiles/marker.png\" style=\"display:none\" />
      <img src=\"http://www.google.com/mapfiles/shadow50.png\" style=\"display:none\" />
      <img src=\"http://www.google.com/mapfiles/markerTransparent.png\" style=\"display:none\" />
      <img src=\"http://www.google.com/mapfiles/markerie.gif\" style=\"display:none\" />
      <img src=\"http://www.google.com/mapfiles/dithshadow.gif\" style=\"display:none\" />


    <table border=1>
      <tr>
        <td>
          <div id=\"map\" style=\"width: 800px; height: 800px\"></div>
        </td>
        <td width = 300 valign=\"top\" >
           <div id=\"sidebar\"></div>
        </td>

      </tr>
    </table>

    <noscript><b>JavaScript must be enabled in order for you to use Google Maps.</b> 
      However, it seems JavaScript is either disabled or not supported by your browser. 
      To view Google Maps, enable JavaScript by changing your browser options, and then 
      try again.
    </noscript>

  </body>
</html>
"
  cat(unlist(sl), file=file)
}






####################################### development part

.middate<-function(data){
  n<-round(nrow(data)/2)
  return(mdy.date(data[n,2], data[n,1], data[n,3])-mdy.date(data[1,2], data[1,1], data[1,3]))
}
  
.generate.twoseg.dat.file<-function(data, file="twosegtrack.dat", fix.first=TRUE, 
  fix.last=TRUE, no.segments=2, theta.active=c(u.active, v.active, D.active, bx.active, by.active, 
  sx.active, sy.active, a0.active, b0.active, tau.active), theta.init=c(u.init, v.init, D.init, bx.init, by.init, 
  sx.init, sy.init, a0.init, b0.init, tau.init), u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, 
  by.active=TRUE, sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, tau.active=TRUE, u.init=0, v.init=0, D.init=100, 
  bx.init=0, by.init=0, sx.init=.5, sy.init=1.5, a0.init=0.001, b0.init=0, 
  tau.init=.middate(data), 
  var.struct="solstice", dev.pen=0.0){

  "%+%"<-function(s1, s2)paste(s1, s2, sep="")
  tostr<-function(x)paste(as.numeric(x), collapse="\t")
  var.flags<-switch(var.struct, "uniform"=c(0,0,0,0,0), "solstice"=c(1,theta.init[9:8],0,0), 
                    "daily"=c(0,0,0,1,dev.pen), warning("No matching variance structure found"))

  header<-"#Auto generated data file from R-KFtrack \n#"%+%date()%+%"\n#\n"%+%
          "# Number of data points\n  "%+%
          tostr(nrow(data))%+%"\n#\n"%+%
          "# 1 if last point is true recapture position; 0 otherwise\n  "%+%
          tostr(fix.last)%+%"\n#\n"%+%
          "# Number of segments (currently only 1 or 2)\n  "%+%
          tostr(no.segments)%+%"\n#\n"%+%
          "# active parameters \n# u\tv\tD\tbx\tby\tsx\tsy\ttau\n  "%+%
          tostr(theta.active[c(1:7,10)])%+%"\n#\n"%+%
          "# initial values \n# u\tv\tD\tbx\tby\tsx\tsy\ttau\n  "%+%
          tostr(theta.init[c(1:7,10)])%+%"\n#\n"%+%
          "# latitude errors \n# cos\tinit.b0\tinit.a0\tdev\tdeviation penalty weight\n  "%+%
          tostr(var.flags)%+%"\n#\n"%+%
          "# Positions \n# day\tmonth\tyear\tlong\tlati\n  "

  cat(header, file=file)
  write.table(data, file=file, row.names=FALSE, col.names=FALSE, 
              sep="\t", eol="\n  ", append=TRUE)
  cat("\n", file=file, append=TRUE)
  class(header)<-"kfhead"
  return(header)
}

.read.twoseg.output<-function(data, fix.first=TRUE, fix.last=TRUE, no.segments=2,
  theta.active=c(u.active, v.active, D.active, bx.active, by.active, sx.active, sy.active, a0.active, b0.active, tau.active),
  theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, b0.init, tau.init), 
  u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, by.active=TRUE, 
  sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, tau.active=TRUE, u.init=0, v.init=0, D.init=100, bx.init=0, by.init=0, 
  sx.init=.5, sy.init=1.5, a0.init=0.001, b0.init=0,  tau.init=.middate(data), var.struct="solstice", dev.pen=0.0, hess.warn=TRUE){

  getpar<-function(what, file="twosegtrack.par"){
    txt<-readLines(file)
    txt<-strsplit(txt[grep(what, txt)+1], split=" ")[[1]]
    txt<-txt[txt!=""]
    return(as.numeric(txt))
  }

  getrep<-function(what, file="twosegtrack.rep"){
    txt<-readLines(file)
    txt<-strsplit(txt[grep(what, txt)], split=" ")[[1]]
    if(length(txt)==3){
      return(as.numeric(txt[3]))
    }else{
      return(as.numeric(txt[4:5]))
    }
  }

  theta.names<-c("u","v","D","bx","by","sx","sy","tau")
 
  if(file.access("twosegtrack.par")!=0){
    warning("File \"twosegtrack.par\" not found (possibly because there was no solution to minimization problem)", call.=FALSE)
    npar<-NA; nlogL<-NA; max.grad.comp<-NA; estimates<-NA
  }else{
    tmp<-as.numeric(scan("twosegtrack.par", what="character", 16, quiet=TRUE)[c(6,11,16)])
    npar<-tmp[1]; nlogL<-tmp[2]; max.grad.comp<-tmp[3]
    estimates<-sapply(c("uu","vv","D","bx","by","vx","vy","tau"), getpar)
    names(estimates)<-theta.names
    estimates<-unlist(estimates)
    if(var.struct=="solstice"){estimates<-c(estimates,sapply(c("a0", "b0"), getpar))}
  }

  if(file.access("twosegtrack.rep")!=0){
    warning("File \"twosegtrack.rep\" not found", call.=FALSE)
    spd<-NA; hdg<-NA
  }else{
    spd<-getrep("spd")
    hdg<-getrep("hdg")
  }

  if(file.access("twosegtrack.std")!=0){
    if(hess.warn){
      warning("File \"twosegtrack.std\" not found (possibly the hessian was not estimated)", call.=FALSE)
    }
    std.dev<-NA    
  }else{
    dat<-read.table("twosegtrack.std", skip=1)
    tmp<-dat[dat[,2]%in%c("sduu","sdvv","sdD","sdbx","sdby","sdvx","sdvy"), 4]
    if(theta.active[10]){tmp<-c(tmp,dat[dat[,2]=="tau",4])}else{tmp<-c(tmp,0)}
    if(var.struct=="solstice"){
      if(theta.active[8]){tmp<-c(tmp,dat[dat[,2]=="a0",4])}else{tmp<-c(tmp,0)}
      if(theta.active[9]){tmp<-c(tmp,dat[dat[,2]=="b0",4])}else{tmp<-c(tmp,0)}
    }
    std.dev<-tmp
  }

  mptfilename<-"mpt.dat"
  #mptfilename<-paste("mpt_", paste(as.numeric(
  #  c(theta.active[1:7],var.struct=="solstice",var.struct=="daily")), collapse=""),".dat",sep="")
  if(file.access(mptfilename)!=0){warning("File not found")}else{
    tmp<-read.table(mptfilename,skip=3, header=FALSE)
    name<-strsplit(readLines(mptfilename, 3)[3], split=" ")[[1]]  
    colnames(tmp)<-name[!name%in%c("","#")]
    attach(tmp)
      nominal.track<-cbind(x=ox, y=oy)
      pred.track<-cbind(x=px,y=py)
      most.prob.track<-cbind(x=smoothX, y=smoothY)
      days.at.liberty<-cumsum(dt)
      date<-matrix(as.numeric(unlist(strsplit(as.character(date), "/"))), ncol=3, byrow=TRUE) 
      colnames(date)<-c("year", "month", "day")
    detach(tmp) 
  }

  return(list(npar=npar, nlogL=nlogL, max.grad.comp=max.grad.comp, estimates=estimates, 
         std.dev=std.dev, nominal.track=nominal.track, pred.track=pred.track, 
         most.prob.track=most.prob.track, days.at.liberty=days.at.liberty, date=date, 
         spd=spd, hdg=hdg))
}


.init.scan<-function(data, where=c(.25,.5,.75) , fix.first=TRUE, fix.last=TRUE, 
  theta.active=c(u.active, v.active, D.active, bx.active, by.active, sx.active, sy.active, a0.active, b0.active, tau.active), 
  theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, b0.init, tau.init), 
  u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, by.active=TRUE, sx.active=TRUE, 
  sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, tau.active=TRUE, 
  u.init=0, v.init=0, D.init=100, bx.init=0, by.init=0, sx.init=.5, sy.init=1.5, a0.init=0.001, 
  b0.init=0, tau.init=.middate(data), var.struct="solstice", dev.pen=0.0, save.dir=NULL){

  nr<-nrow(data)
  fromto<-6 # The minimum number of data points on one side of a cut. 
  start.dal<-mdy.date(data[fromto,2], data[fromto,1], data[fromto,3])-
              mdy.date(data[1,2], data[1,1], data[1,3])
  end.dal<-mdy.date(data[nr-(fromto-1),2], data[nr-(fromto-1),1], data[nr-(fromto-1),3])-
            mdy.date(data[1,2], data[1,1], data[1,3])



  olddir<-getwd()
  dirname<-ifelse(is.null(save.dir),"_kftrack_temp_",save.dir) 
  dir.create(dirname)
  setwd(dirname)

    #ONE SEGMENT RUN
    this.theta.active<-c(theta.active[1:9], FALSE)
    header<-.generate.twoseg.dat.file(data, fix.first=fix.first, fix.last=fix.last, no.segments=1, 
      theta.active=this.theta.active, theta.init=theta.init, var.struct=var.struct, dev.pen=dev.pen)
      
    filename<-dir(paste(.path.package("kftrack"),"/admb", sep=""), pattern="two")			       
    if(.Platform$OS.type=="windows"){
      system(paste(.path.package("kftrack"),"/admb/",filename," -est ",admb.string, sep=""))
    }else{
      system(paste("cp ",.path.package("kftrack"),"/admb/",filename," ",filename, sep=""))
      system(paste("./",filename," -est ",admb.string, sep=""))
    }
     
    kf.one<-.read.twoseg.output(data, fix.first=fix.first, fix.last=fix.last, no.segments=1, 
      theta.active=this.theta.active, theta.init=theta.init, var.struct=var.struct, dev.pen=dev.pen, hess.warn=FALSE)

    #SCANNING RUNS
    scan.res<-matrix(NA, nrow=length(where), ncol=3)
    for(i in 1:length(where)){
      this.theta.active<-c(theta.active[1:9], FALSE)
      tau<-start.dal+where[i]*(end.dal-start.dal)
      this.theta.init<-c(kf.one$estimates[-8], tau)
      header<-.generate.twoseg.dat.file(data, fix.first=fix.first, fix.last=fix.last, no.segments=2, 
        theta.active=this.theta.active, theta.init=this.theta.init, var.struct=var.struct, dev.pen=dev.pen)

      filename<-dir(paste(.path.package("kftrack"),"/admb", sep=""), pattern="two")			       
      if(.Platform$OS.type=="windows"){
        system(paste(.path.package("kftrack"),"/admb/",filename," -est ",admb.string, sep=""))
      }else{
        system(paste("cp ",.path.package("kftrack"),"/admb/",filename," ",filename, sep=""))
        system(paste("./",filename," -est ",admb.string, sep=""))
      }

      kf.scan<-.read.twoseg.output(data, fix.first=fix.first, fix.last=fix.last, no.segments=2, 
        theta.active=this.theta.active, theta.init=this.theta.init, var.struct=var.struct, dev.pen=dev.pen, hess.warn=FALSE)
      scan.res[i,]<-c(tau, kf.scan$nlogL, kf.scan$max)
    }

    #FINAL RUN

    tau<-scan.res[which.min(scan.res[abs(scan.res[,3])<1.0e-4,2]),1]
    if(length(tau)==0){tau<-scan.res[which.min(scan.res[,2]),1]}    
  
    this.theta.active<-c(theta.active[1:9], TRUE)
    this.theta.init<-c(kf.one$estimates[-8], tau)
    header<-.generate.twoseg.dat.file(data, fix.first=fix.first, fix.last=fix.last, no.segments=2, 
      theta.active=this.theta.active, theta.init=this.theta.init, var.struct=var.struct, dev.pen=dev.pen)
    
    filename<-dir(paste(.path.package("kftrack"),"/admb", sep=""), pattern="two")			       
    if(.Platform$OS.type=="windows"){
      error.code<-system(paste(.path.package("kftrack"),"/admb/",filename," ",admb.string, sep=""))      
    }else{
      system(paste("cp ",.path.package("kftrack"),"/admb/",filename," ",filename, sep=""))
      error.code<-system(paste("./",filename," ",admb.string, sep=""))
    }

    kf.o<-.read.twoseg.output(data, fix.first=fix.first, fix.last=fix.last, no.segments=2, 
      theta.active=this.theta.active, theta.init=this.theta.init, var.struct=var.struct, dev.pen=dev.pen, hess.warn=FALSE)

    kf.o$nobs<-nrow(data); kf.o$header<-header; kf.o$fix.first<-fix.first; 
    kf.o$fix.last<-fix.last; kf.o$theta.active<-theta.active; kf.o$theta.init<-theta.init;
    kf.o$var.struct<-var.struct; kf.o$dev.pen<-dev.pen; kf.o$call<-match.call();
    kf.o$data.name<-deparse(substitute(data))

    kf.o$scan.res<-scan.res
    kf.o$one.seg.nlogL<-kf.one$nlogL
    kf.o$one.seg.npar<-kf.one$npar
    kf.o$error.code<-error.code
  setwd(olddir)
  if(is.null(save.dir)){unlink(dirname, recursive = TRUE)}
  class(kf.o)<-c("kftrack.scan", "kftrack")
  return(kf.o)
}

plot.kftrack.scan<-function(x,...){
  .addrange<-function(x, pct=.05){
    minx<-min(x); maxx<-max(x); dif<-maxx-minx
    c(minx,maxx)+c(-1,1)*dif*pct
  }

  par(mar=c(4, 4, 5, 1) + 0.1)
  layout(matrix(c(1,4,2,4,3,4),3,2,byrow=TRUE))
  attach(x)
    mypch<-c(ifelse(fix.first,6,4), rep(4,nobs-2), ifelse(fix.last,2,4))

    xrange<-.addrange(c(nominal.track[,"x"], pred.track[,"x"], most.prob.track[,"x"]))
    plot(days.at.liberty, nominal.track[,"x"], ylab="Longitude", pch=mypch, 
         ylim=xrange, xlab="Days at liberty", type="b", col=gray(.9))
    points(days.at.liberty, nominal.track[,"x"], pch=mypch)
    lines(days.at.liberty, pred.track[,"x"], col="green")
    lines(days.at.liberty, most.prob.track[,"x"], col="blue")
    xp<-par("xaxp"); xcut<-seq(xp[1], xp[2], length=xp[3]+1)
    abline(v=estimates["tau"], lwd=2, col="red")
   
    yrange<-.addrange(c(nominal.track[,"y"], pred.track[,"y"], most.prob.track[,"y"]))
    
    if(.have.date()){firstdate<-mdy.date(date[1,"month"], date[1,"day"], date[1,"year"])}

    plot(days.at.liberty, nominal.track[,"y"], ylab="Latitude", pch=mypch, ylim=yrange, 
         las=3, xlab="", axes=FALSE, type="b", col=gray(.9))
    points(days.at.liberty, nominal.track[,"y"], pch=mypch)
    lines(days.at.liberty, pred.track[,"y"], col="green")
    lines(days.at.liberty, most.prob.track[,"y"], col="blue")
    axis(1); axis(2); box() 
    mtext("Days at liberty", side=1, line=3, cex=par("cex"))
    abline(v=estimates["tau"], lwd=2, col="red")

    quan<-one.seg.nlogL-0.5*qchisq(c(.95, .99, .999), npar-one.seg.npar-1)
    par(mar=c(6, 4, 3, 1) + 0.1)
    plot(days.at.liberty, rep(NA,length(days.at.liberty)) , ylab="-log likelihood", 
         ylim=range(c(one.seg.nlogL, scan.res[,2], quan, nlogL)), las=3, xlab="", axes=FALSE)
    scan.res<-rbind(scan.res[,1:2], c(estimates["tau"], nlogL))
    scan.res<-scan.res[order(scan.res[,1]),]
    points(scan.res, pch=1, type="b")
    abline(h=c(one.seg.nlogL, quan), col=1:4)
    if(.have.date()){
      axis(2)
      axis(1, at=xcut, labels=paste(firstdate+xcut), las=2)
      box()
      mtext("Date", side=1, line=5, cex=par("cex"))
    }else{
      axis(1); axis(2); box() 
      mtext("Days at liberty", side=1, line=3, cex=par("cex"))
    }

    par(mar=c(4, 4, 5, 1) + 0.1)
    plot(nominal.track, pch=mypch, xlab="Longitude", ylab="Latitude", xlim=xrange, 
         ylim=yrange, type="b", col=gray(.9))
    points(nominal.track, pch=mypch)
    lines(pred.track, col="green")
    lines(most.prob.track, col="blue")
    points(most.prob.track[which.min(abs(days.at.liberty-estimates["tau"])),, drop=FALSE], pch=4, cex=2, col="red", lwd=2) 
    points(pred.track[which.min(abs(days.at.liberty-estimates["tau"])),,drop=FALSE], pch=4, cex=2, col="red", lwd=2) 

    title(paste("Estimated track of", x$data.name), outer=TRUE,line=-1.5)
  detach(x)
}


#Sibert's GMT stuff 

.make.flags<-function(f)
{
  attach(f)
  flags<-paste(as.integer(theta.active)[1:11],sep="",collapse="")
  flags<-paste(flags,as.integer(var.struct=="solstice"),sep="")
  if (fix.last)
    flags<-paste(flags,"+",sep="")
  detach(f)
  return(flags)
}

.cat.cmd<-function(cmd,file,append=TRUE)
{
# print(cmd)
  cat(paste(cmd,"\n",sep=""),file=file, append=append)
}

.cat.poly<-function(x,file)
{
  .cat.cmd(paste(x[,1]," ",x[,2],sep=""),file)
  .cat.cmd(">",file)
}

.getCI<-function (x, level = 0.95, npoints = 100)
{
    t.quan <- sqrt(qchisq(level, 2))
    centre <- x[5:6]
    x <- matrix(x[1:4], 2, 2)
    r <- x[1, 2]
    scale <- sqrt(diag(x))
    if (scale[1] > 0) {
        r <- r/scale[1]
    }
    if (scale[2] > 0) {
        r <- r/scale[2]
    }
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1],
        t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints,
        2)
}

.gmt.plot<-function (lis, map.width = 5, ci = FALSE, points = TRUE, pred = FALSE,
    most = TRUE, show.map = FALSE, remove.script = FALSE, level = 0.95,
    npoints = 25, region = NULL)
{
    noTracks=length(lis)
    nt <- unlist(lapply(lis, function(x)x$nobs))
    if (is.null(region)) {
        east <- ceiling(max(unlist(lapply(lis,function(x)x$pred.track[, 1]))) + 1)
        west <- floor(min(unlist(lapply(lis,function(x)x$pred.track[, 1]))) - 1)
        north <- ceiling(max(unlist(lapply(lis,function(x)x$pred.track[, 2]))) + 1)
        south <- floor(min(unlist(lapply(lis,function(x)x$pred.track[, 2]))) - 1)
    }
    else {
        r.vec <- as.numeric(unlist(strsplit(region, "/")))
        west <- r.vec[1]
        east <- r.vec[2]
        south <- r.vec[3]
        north <- r.vec[4]
    }

    scale <- map.width/(east - west)

    if (noTracks == 1)
    {
      flags<-.make.flags(lis[[1]])
      basemap.note <- paste(":.\"Track ",lis[[1]]$data.name, "\": -U\"", flags, "\" ",sep= "")
      map.file <- paste(lis[[1]]$data.name,"-map.eps", sep = "")
    }
    else
    {
      flags<- ""
      basemap.note <- " "
      map.file <- paste(noTracks,"track", "-map.eps", sep = "")
    }
    gmt <- "./rgmtmap"
    .cat.cmd("#!/bin/bash", file = gmt, append = FALSE)
    .cat.cmd("# script generated automatically by kftrack", gmt)
    .cat.cmd(paste("REGION=-R", west, "/", east, "/", south,
        "/", north, sep = ""), gmt)
    .cat.cmd(paste("PROJECTION=-Jm", scale, sep = ""), gmt)
    .cat.cmd(paste("FILE=", map.file, sep = ""), gmt)
    .cat.cmd("O_COLOR=/128", gmt)
    .cat.cmd("P_COLOR=1p/0/255/0", gmt)
    .cat.cmd("S_COLOR=1p/0/0/255", gmt)
    .cat.cmd("gmtset PAPER_MEDIA letter+ PAGE_ORIENTATION portrait HEADER_FONT_SIZE 16 HEADER_FONT 1 MEASURE_UNIT inch",
        gmt)
    .cat.cmd(paste("psbasemap -B10f1",basemap.note,"$REGION $PROJECTION  -K >  $FILE", sep = ""), gmt)
    .cat.cmd("pscoast $REGION $PROJECTION -Di -G237/214/151 -W154/139/98 -S194/241/253 -O -K >> $FILE",
        gmt)
    if (ci) {
        lis.cipoints <- lapply(lis, function(x){
                                  apply(cbind(x$var.most.prob.track, x$most.prob.track),
                                        1, .getCI, level = level, npoints = npoints)
                                }
                          )
        .cat.cmd("cat << EOFpoly | psxy $REGION $PROJECTION -L -M -Gp0/7:B-F255/0/0  -O -K >> $FILE", gmt)
     
        lapply(lis.cipoints, function(cipoints){
                               nellipses <- dim(cipoints)[2]
                               dummy <- sapply(1:nellipses, function(i) {
                                 poly <- cbind(cipoints[1:npoints, i], cipoints[(npoints + 1):(2 * npoints), i])
                                 .cat.poly(poly, gmt)
                                 })
                               
                             })
        .cat.cmd("EOFpoly", gmt) 
    }



    if (points) {
        .cat.cmd("cat << EOF0001 | psxy $REGION $PROJECTION -Sx0.0625i            -O -K -M>> $FILE",
            gmt)
        lapply(lis, function(x){
                      .cat.cmd(paste(x$nominal.track[, 1], " ", x$nominal.track[,2], sep = ""), gmt)
                      .cat.cmd(">", gmt)
                    }
              )
        .cat.cmd("EOF0001", gmt)
        .cat.cmd("cat << EOF0002  | psxy $REGION $PROJECTION            -W$O_COLOR -O -K -M>> $FILE",
            gmt)
        lapply(lis, function(x){
                      .cat.cmd(paste(x$nominal.track[, 1], " ", x$nominal.track[,2], sep = ""), gmt)
                      .cat.cmd(">", gmt)
                    }
              )
        .cat.cmd("EOF0002", gmt)
    }
    if (pred) {
        .cat.cmd("cat << EOF0003  | psxy $REGION $PROJECTION -W$P_COLOR -O -K -M>> $FILE",
            gmt)
        lapply(lis, function(x){
                      .cat.cmd(paste(x$pred.track[, 1], " ", x$pred.track[,2], sep = ""), gmt)
                      .cat.cmd(">", gmt)
                    }
              )
        .cat.cmd("EOF0003", gmt)
    }
    if (most) {
        .cat.cmd("cat << EOF0004  | psxy $REGION $PROJECTION -W$S_COLOR -O -K -M>> $FILE",
            gmt)
        lapply(lis, function(x){
                      .cat.cmd(paste(x$most.prob.track[, 1], " ", x$most.prob.track[,2], sep = ""), gmt)
                      .cat.cmd(">", gmt)
                    }
              )
        .cat.cmd("EOF0004", gmt)
    }
    lapply(lis, function(x){ 
                  if (x$fix.first)
                   .cat.cmd(paste("echo '", x$nominal.track[1, 1], " ",
                   x$nominal.track[1, 2], " 0.125' |psxy  $REGION $PROJECTION -Si -W1p/255/0/0 -O -K >> $FILE",
                   sep = ""), gmt)
                 if (x$fix.last)
                 {
                   nt<-nrow(x$nominal.track)
                   .cat.cmd(paste("echo '", x$nominal.track[nt, 1], " ",
                   x$nominal.track[nt, 2], " 0.125' |psxy  $REGION $PROJECTION -St -W1p/255/0/0 -O -K >> $FILE",
                   sep = ""), gmt)
                  }
                }
          )
    xlatlab <- west - 0.75/scale
    ylatlab <- south + 0.5 * (north - south)
    xlonlab <- west + 0.5 * (east - west)
    ylonlab <- south - 0.5/scale
    .cat.cmd(paste("echo '", xlonlab, " ", ylonlab, " 14 0 0 CB Longitude' | pstext $REGION $PROJECTION  -N -O -K >> $FILE",
        sep = ""), gmt)
    .cat.cmd(paste("echo '", xlatlab, " ", ylatlab, " 14 90 0 CM Latitude' | pstext $REGION $PROJECTION  -N -O    >> $FILE",
        sep = ""), gmt)
    if (show.map)
        .cat.cmd("ghostview $FILE", gmt)
    .cat.cmd("rm .gmtcommands4 .gmtdefaults4", gmt)
    system(paste("chmod u+x ", gmt, sep = ""))
    if (.Platform$OS.type == "windows") {
        sr <- system(paste("bash", gmt, sep = " "), intern = FALSE)
    }
    else {
        sr <- system(gmt, intern = FALSE)
    }
    if (remove.script)
        .cat.cmd("rm $FILE", gmt)
    return(map.file)
}

