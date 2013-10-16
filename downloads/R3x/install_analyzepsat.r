# Small script to install  analyzepsat and dependencies
# Ben Galuardi March 2013 <galuardi@eco.umass.edu>

# funstion to test for installed packages

install.analyzepsat=function(R3='T'){

pkglist=c('MASS','gdata','maptools','GenKern','matlab','raster','ncdf','RODBC','adehabitat','fields','ellipse','crawl')
pkgTest <- function(x)
  {
    if(x %in% rownames(installed.packages())==F)
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }

# install required packages  
for(i in pkglist){
 pkgTest(i)
}
# install kf stuff
if('ukfsst' %in% rownames(installed.packages())==F){
 if(R3==T){
  source('http://geolocation.googlecode.com/files/install-3.0.r')
 }else{
  source('http://geolocation.googlecode.com/files/install.r')
 }
}
dfile = 'http://geolocation.googlecode.com/svn/trunk/downloads/R3x/analyzepsat_3.1.tar.gz'
destfile = 'analyzepsat_3.1.tar.gz'
download.file(dfile, destfile, mode='wb')
install.packages(destfile, .libPaths()[1], repos = NULL, type='source')
unlink(dfile)
}

install.analyzepsat(R3='T')
 



