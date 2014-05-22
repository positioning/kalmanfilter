.sst <- function(track, zlim=5){
    # Create daily SST dataframe from tag temperature data above zlim meters
    sst.zlimm <- evalq(tapply(temp,paste(year,month,day),function(x) mean(x)), track[track$depth<zlim,])
    n.zlimm <- evalq(tapply(temp,paste(year,month,day),function(x) length(x)), track[track$depth<zlim,])
    year<- as.numeric(unlist(lapply(strsplit(names(sst.zlimm),split=' '),function(x) x[1])))
    month<- as.numeric(unlist(lapply(strsplit(names(sst.zlimm),split=' '),function(x) x[2])))
    day <- as.numeric(unlist(lapply(strsplit(names(sst.zlimm),split=' '),function(x) x[3])))
    tag.sst.dat <- data.frame(year=year,month=month,day=day,hour=rep(0,length(year)), min=rep(0,length(year)), sec=rep(0,length(year)),sst=round(sst.zlimm,2))
    tag.sst.dat <- tag.sst.dat[n.zlimm >= zlim,]   #only include days with at least zlim temperature readings above zlim metres
    tag.sst.dat <- tag.sst.dat[order(tag.sst.dat$year,tag.sst.dat$month,tag.sst.dat$day),]
    return(tag.sst.dat)
}

