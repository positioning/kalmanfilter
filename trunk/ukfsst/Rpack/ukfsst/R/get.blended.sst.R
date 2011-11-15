get.blended.sst <-
function(track, folder=tempdir(), 
  server="http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=", 
  nday = "5day"){
  fl<-get.avhrr.sst(track=track, folder=folder, server=server, product="TBAssta", nday=nday)
  return(fl)
}

