# Small script to install KF geolocation packages and the packages it depend on
# Modified from earlier work of Anders Nielsen
# Modified for R 3.0 by Benjamin Galuardi
# Last edited May 16 2016
# by Chi Hin (Tim) Lam <tagtuna@gmail.com> 

kf.install <- function(x64=FALSE, ver)
{
install.packages("date", repos="http://cran.rstudio.com/")
install.packages("ncdf", repos="http://cran.rstudio.com/")

d2 <- "http://github.com/positioning/kalmanfilter/raw/master/Rpack/"
# For R.2.15 or earlier
d3 <-"http://github.com/positioning/kalmanfilter/raw/master/downloads/R3x/"
# For R.3.0 or later
dlink <- ifelse(ver==2,d2,d3)
os <- Sys.info()[['sysname']]
lext <- c('.tar.gz')
pac <- c('kftrack', 'ukfsst', 'trackit')
ver <- c('_0.70', '_0.3', '_0.2-6')

if (os == "Windows") {
   for (i in 1:3) {
     lfile <- paste(pac[i],ver[i],lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'),'/win/' ,pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, .libPaths()[1], repos = NULL, type='source')
     unlink(lfile)
     }
  } else if (os == "Darwin") {
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'), '/mac/' , pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     download.file(llink, lfile, mode='wb')
     install.packages(lfile, .libPaths()[1], repos = NULL, type='source')
     unlink(lfile)
     }
  } else {	
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
   kf.install(F,2)} else{
   kf.install(F,3)}
