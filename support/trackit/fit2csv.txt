fit2csv <- function(fit, folder=getwd(), name=NA)
{ 
  require(date)
  cls <- class(fit)
  if (cls=="trackit") 
  {
     if (is.na(name)) {name <- deparse(substitute(fit))}
     fln1 <- paste(folder,paste(name,"a.csv",sep = ""), sep = "/")
     fln2 <- paste(folder,paste(name,"b.csv",sep = ""), sep = "/")
     fln3 <- paste(folder,paste(name,"c.csv",sep = ""), sep = "/")
     # Output track
     mdy <- date.mdy(fit$date)
     ddate <- fit$decimal.date
     dhr <- ddate-trunc(ddate)
     hour <- trunc(dhr*24)
     min <- trunc((dhr*24-trunc(dhr*24))*60)
     stamp <- ISOdatetime(mdy$year, mdy$month, mdy$day, hour, min, 0, tz = "GMT")+2*60
     cat(c("pointid","datetime","mptlon","mptlat","varlon","varlat",paste(cls,"\n", sep="")), sep=",", file=fln1)
     write.table(data.frame(stamp,fit$most.prob.track,fit$var.most.prob.track[,c(1,4)]), 
		append=T, col.names=F, sep=",", na="", file=fln1)
     # Output parameters
     n1 <- c("uinit","vinit","Dinit",
            "ss1init","ss2init","ss3init",
            "rhoinit","bsstinit","sssstinit","radinit","dep1init", "dep2init",
            "phi1init","phi2init","phi3init","phi4init","phi5init","phi6init",
            "phi7init","phi8init","phi9init",
            "phi10init","phi11init","phi12init","phi13init","phi14init","phi15init")
     n2 <- c("uph","vph","Dph","ss1ph","ss2ph","ss3ph","rhoph",
                     "bsstph","sssstph","radph","dep1ph","dep2ph","phiph")
     names(fit$init) <- n1
     names(fit$phase) <- n2
     names(fit$nlogL) <- "nlogL"
     names(fit$max.grad.comp) <- "maxgradcomp"
     cat(c("parameter","estimates","stdev","dataname",paste(cls,"\n", sep="")), sep=",", file=fln2)
     write.table(data.frame(fit$est, fit$sd, name),append=T, col.names=F, sep=",", na="", file=fln2)
     cat(c("parameter","value",paste(cls,"\n", sep="")), sep=",", file=fln3)
     write.table(data.frame(c(fit$init,fit$phase,fit$nlogL,fit$max.grad.comp)),append=T, col.names=F, row.names=T, sep=",", na="", file=fln3)
  }
  else 
  {
   name <- ifelse(is.na(name), fit$data.name, name)
   fln1 <- paste(folder,paste(name,"a.csv",sep = ""), sep = "/")
   fln2 <- paste(folder,paste(name,"b.csv",sep = ""), sep = "/")
   fln3 <- paste(folder,paste(name,"c.csv",sep = ""), sep = "/")
    if (cls=="kfsst"){
     cat(c("pointid","dataname","year","month","day","mptlon","mptlat","varlon","varlat","taglon","taglat","tagsst", "mptsst",paste(cls,"\n", sep="")), sep=",", file=fln1)
     write.table(data.frame(fit$data.name,fit$date,fit$most.prob.track,fit$var.most.prob.track[,c(1,4)],fit$nominal.track,fit$SST[,c(1,3)]), append=T, col.names=F, sep=",", na="", file=fln1)
     # Add labels, rearrange, fill gaps
     a1 <- c("u","v","D","bx","by","bsst","sx","sy","ssst","radius","a0","b0")
     fit$theta.active <- fit$theta.active[c(1:9,12,10,11)]
     fit$theta.init <- fit$theta.init[c(1:9,12,10,11)]
     names(fit$theta.active) <- a1
     names(fit$theta.init) <- a1
     n1 <- names(fit$estimates)
     miss <- setdiff(a1,n1)
     a2 <- rep(-999, length(miss))
     if (length(a2)>0){
       names(a2) <- miss
       est <- c(fit$estimates,a2)
       std <- c(fit$std.dev, a2)
     }else 
      {est <- fit$estimates
       std <- fit$std.dev }
     cat(c("parameter","active","init","estimates","stdev","dataname",paste(cls,"\n", sep="")), sep=",", file=fln2)
     write.table(data.frame(fit$theta.active, fit$theta.init, est, std, fit$data.name),append=T, col.names=F, sep=",", na="", file=fln2)
     cat(c("dataname","fixfirst","fixlast", "varstruct", "nlogL","maxgradcomp", "call",paste(cls,"\n", sep="")), sep=",", file=fln3)
     write.table(data.frame(fit[c(21,15,16,19,2,3)],gsub("\"","'",as.character(fit[20]))),append=T, col.names=F, row.names=F, sep=",", na="", file=fln3)
    }
   if (cls=="kftrack"){
     cat(c("pointid","dataname","year","month","day","mptlon","mptlat","varlon","varlat","taglon","taglat",paste(cls,"\n", sep="")), sep=",", file=fln1)
     write.table(data.frame(fit$data.name,fit$date,fit$most.prob.track,fit$var.most.prob.track[,c(1,4)],fit$nominal.track), append=T, col.names=F, sep=",", na="", file=fln1)
     # Add labels, rearrange, fill gaps
     a1 <- c("u","v","D","bx","by","sx","sy","a0","b0")
     fit$theta.active <- fit$theta.active[1:9]
     fit$theta.init <- fit$theta.init[1:9]
     names(fit$theta.active) <- a1
     names(fit$theta.init) <- a1
     n1 <- names(fit$estimates)
     miss <- setdiff(a1,n1)
     a2 <- rep(-999, length(miss))
     if (length(a2)>0){
     names(a2) <- miss
     est <- c(fit$estimates,a2)
     std <- c(fit$std.dev, a2)
    }else 
     {est <- fit$estimates
      std <- fit$std.dev }
    cat(c("parameter","active","init","estimates","stdev","dataname",paste(cls,"\n", sep="")), sep=",", file=fln2)
    write.table(data.frame(fit$theta.active, fit$theta.init, est, std, fit$data.name),append=T, col.names=F, sep=",", na="", file=fln2)
    cat(c("dataname","fixfirst","fixlast","varstruct","nlogL","maxgradcomp","devpen","call",paste(cls,"\n", sep="")), sep=",", file=fln3)
    write.table(data.frame(fit[c(23,16,17,20,2,3,21)],gsub("\"","'",as.character(fit[22]))),append=T, col.names=F, row.names=F, sep=",", na="", file=fln3)
    }
  }
}
