get.sst.from.server <-
function(track, folder=tempdir(), 
  server='http://atlas.nmfs.hawaii.edu/cgi-bin/reynolds_extract.py')
{
  fl<-dir(folder)
  if(length(fl)!=0){
    folder<-paste(folder,'sst_temp', sep='/')
    dir.create(folder)
  }

  if(is.data.frame(track))track<-list(track)  

  minDate<-min(unlist(lapply(track,function(x)mdy.date(x[1,2],x[1,1],x[1,3]))))
  maxDate<-max(unlist(lapply(track,function(x)mdy.date(x[nrow(x),2],x[nrow(x),1],x[nrow(x),3]))))
  minDate<-minDate-10 # add a few days for good measure and
  maxDate<-maxDate+10 # because data source is 8-day files

  yrRan<-c(date.mdy(minDate)$year,date.mdy(maxDate)$year)
  daysIntoYearRan<-c(minDate-mdy.date(1,1,yrRan[1]),maxDate-mdy.date(1,1,yrRan[2]))

  minLon<-min(unlist(lapply(track, function(x)min(x[,4]))))-1
  maxLon<-max(unlist(lapply(track, function(x)max(x[,4]))))+1
  lonRan<-c(minLon,maxLon)

  minLat<-min(unlist(lapply(track, function(x)min(x[,5]))))-5
  maxLat<-max(unlist(lapply(track, function(x)max(x[,5]))))+5
  latRan<-c(minLat,maxLat)

  string<-''
  string<-paste(string,'?lon1=',lonRan[1],'&lon2=',lonRan[2],sep='')
  string<-paste(string,'&lat1=',latRan[1],'&lat2=',latRan[2],sep='')
  string<-paste(string,'&year1=',yrRan[1],'&day1=',daysIntoYearRan[1],sep='')
  string<-paste(string,'&year2=',yrRan[2],'&day2=',daysIntoYearRan[2],sep='')
  link<-paste(server,string, sep='')
  dest<-paste(folder,'temp.zip', sep='/')
  download.file(link, dest, mode='wb')
  if((version$major<=2)&(version$minor<=9)){
    .Internal(int.unzip(paste(folder,'temp.zip',sep='/'),NULL, folder))
  }else{
    unzip(paste(folder, "temp.zip", sep = "/"), exdir=folder)
  }
  #
  # May need some special treatment for windows here using 'zip.unpack()'
  unlink(paste(folder,'temp.zip', sep='/'))
  .sstFileVector<<-paste(folder,dir(folder), sep='/')

  for(f in .sstFileVector){
    dat<-read.table(f, head=FALSE)
    write.table(dat[complete.cases(dat),], col.names=FALSE, row.names=FALSE, quote=FALSE, file=f)
  }

  cat(paste(rep('=',options()$width), collapse=''),'\n\n')
  cat('Downloaded', length(dir(folder)), 'files to:\n\n  ', folder)
  cat('\n\nNow you most likely want to run a command like:\n\n   ')
  cat('fit <- kfsst(track)','\n\n')
  cat('to fit the full model (except for estimation of the radius).\n')
  cat('It is possible and in some cases necessary to customize the\n')
  cat('full model, for instance by choosing a simpler variance\n')
  cat('structure, estimating radius, or leaving out a bias term.\n')
  cat('For details on these and other options, please see the\n')
  cat('documentation (?kfsst).\n\n')
  cat(paste(rep('=',options()$width), collapse=''),'\n\n')

  return(folder)
}

