# Small script to install KF geolocation packages and the packages it depend on
# Modified from Anders Nielsen and Benjamin Galuardi
# Last edited Oct 24 2016 
# Tested ONLY for only Windows install with R-3.0.2
# by Chi Hin (Tim) Lam <tagtuna@gmail.com> 

kf.install <- function(x64=FALSE, ver)
{
pack <- c('date','ncdf','httr','devtools')
spack <- installed.packages()[,"Package"]
for (i in pack){
  if (!any(i == spack)) install.packages(i, repos="http://cran.rstudio.com/")
}
require(httr) # For https download

dlink <-"https://github.com/positioning/kalmanfilter/raw/master/downloads/R3x/"
os <- Sys.info()[['sysname']]
lext <- c('.tar.gz')
pac <- c('kftrack', 'ukfsst', 'trackit')
ver <- c('_0.70', '_0.3', '_0.2-6')

if (os == "Windows") {
   for (i in 1:3) {
     lfile <- paste(pac[i],ver[i],lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'),'/win/' ,pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     GET(llink, write_disk(lfile, overwrite=TRUE))
     install.packages(lfile, .libPaths()[1], repos = NULL, type='source')
     unlink(lfile)
     }
  } else if (os == "Darwin") {
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'), '/mac/' , pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     GET(llink, write_disk(lfile, overwrite=TRUE))
     install.packages(lfile, .libPaths()[1], repos = NULL, type='source')
     unlink(lfile)
     }
  } else {	
   for (i in 1:3) {
     lfile <- paste(pac[i], lext, sep='') 
     llink <- paste(dlink, ifelse(x64, '64bit', '32bit'), '/linux/' ,pac[i], ver[i], ifelse(x64, '-x64', ''), lext ,sep='')
     GET(llink, write_disk(lfile, overwrite=TRUE))
     install.packages(lfile, repos = NULL, type='source')
     unlink(lfile)
     } 
  }
}

kf.install(F)
