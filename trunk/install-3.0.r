# Small script to install KF geolocation packages and the packages it depend on
# Modified from earlier work of Anders Nielsen
# Chi Hin (Tim) Lam <timlam@eco.umass.edu> 
# November 28, 2012
# modified for R 3.0 by Benjamin Galuardi
# October 16, 2013
# For details, visit http://code.google.com/p/geolocation

kf.install <- function(x64=FALSE)
{
install.packages("date", repos="http://cran.rstudio.com/")
install.packages("ncdf", repos="http://cran.rstudio.com/")
# dlink <- 'http://geolocation.googlecode.com/files/'
dlink<-"http://geolocation.googlecode.com/svn/trunk/downloads/R3x/"
# https://geolocation.googlecode.com/svn/trunk/downloads/R3x/64bit/kftrack_0.70-x64.tar.gz
os <- Sys.info()[['sysname']]
ext <- c('.tar.gz')
pac <- c('kftrack', 'ukfsst', 'trackit')
ver <- c('_0.70', '_0.3', '_0.2-6')

if (os == "Windows") {
   lext <- ext[1]
   for (i in 1:3) {
     lfile <- paste(pac[i],ver[i],lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'),'/win/' ,pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, .libPaths()[1], repos = NULL, type='source')
     unlink(lfile)
     }
  } else if (os == "Darwin") {
   lext <- ext[1]
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'), '/mac/' , pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, .libPaths()[1], repos = NULL, type='source')
     unlink(lfile)
     }   
  } else {
   lext <- ext[1]
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'), '/linux/' ,pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, repos = NULL, type='source')
     unlink(lfile)
     } 
  }
}

kf.install() ### install 32-bit files by default