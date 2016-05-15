# Small script to install KF geolocation packages and the packages it depend on
# Modified from earlier work of Anders Nielsen
# Chi Hin (Tim) Lam <tagtuna@gmail.com> 
# October 16, 2013 modified for R 3.0 by Benjamin Galuardi
# For details, visit http://code.google.com/p/geolocation

# For R.2.15 or earlier
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

# For R.3.0 or later
kf3.install <- function(x64=FALSE)
{
install.packages("date", repos="http://cran.rstudio.com/")
install.packages("ncdf", repos="http://cran.rstudio.com/")
dlink<-"http://github.com/positioning/kalmanfilter/raw/master/downloads/R3x/"
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

# Switch between R versions
   ### install 32-bit files by default
if (getRversion() < "3"){
   kf.install()} else{
   kf3.install()}
