# Small script to install KF geolocation packages and the packages it depend on
# Modified from earlier work of Anders Nielsen
# Chi Hin (Tim) Lam <timlam@eco.umass.edu> 
# November 28, 2012
# For details, visit http://code.google.com/p/geolocation

kf.install <- function(x64=FALSE)
{
install.packages("date", repos="http://cran.rstudio.com/")
install.packages("ncdf", repos="http://cran.rstudio.com/")
dlink <- 'http://geolocation.googlecode.com/files/'
os <- Sys.info()[['sysname']]
ext <- c('.zip', '.tgz', '.tar.gz')
pac <- c('kftrack', 'ukfsst', 'trackit')
ver <- c('_0.70', '_0.3', '_0.2-6')

if (os == "Windows") {
   lext <- ext[1]
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, pac[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, .libPaths()[1], repos = NULL)
     unlink(lfile)
     }
  } else if (os == "Darwin") {
   lext <- ext[2]
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, pac[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, .libPaths()[1], repos = NULL)
     unlink(lfile)
     }   
  } else {
   lext <- ext[3]
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, .libPaths()[1], repos = NULL)
     unlink(lfile)
     } 
  }
}

kf.install() ### install 32-bit files by default