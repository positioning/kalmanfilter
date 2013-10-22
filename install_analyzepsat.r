# Small script to install  analyzepsat and dependencies
# Ben Galuardi March 2013 <galuardi@eco.umass.edu>

install.psat = function(v3=F){
#===============================================================#
# my old version
# funstion to test for installed packages
#===============================================================#
# pkglist=c('MASS','gdata','maptools','GenKern','matlab','raster','ncdf','RODBC','adehabitat','fields','ellipse')
# pkgTest <- function(x)
  # {
    # if(x %in% rownames(installed.packages())==F)
    # {
      # install.packages(x,dep=TRUE)
        # if(!require(x,character.only = TRUE)) stop("Package not found")
    # }
  # }
  
# install required packages  
# for(i in pkglist){
 # pkgTest(i)
# }
  
#===============================================================#
# New version: borrowed from   http://www.rforge.net/FSA/InstallFSA.R
#===============================================================#
# Find out what analyzepsat needs that is not already installed
installed <- library()$results[,"Package"]
psat.depend <- c('MASS','gdata','maptools','GenKern','matlab','raster','ncdf','RODBC','adehabitat','fields','ellipse','crawl')
psat.dep.log <- psat.depend %in% installed
psat.need <- psat.depend[!psat.dep.log]  

## Install all packages from CRAN that are needed (be on the lookout for choosing a mirror)
if (length(psat.need)>0) install.packages(psat.need)

#================================================================#
# back to my version
#================================================================#
# install kf stuff
if('ukfsst' %in% rownames(installed.packages())==F){
if(v3==F){
 source('http://geolocation.googlecode.com/files/install.r')
 }else{
 source('http://geolocation.googlecode.com/svn/trunk/install-3.0.r')
 }
}

# if (.Platform$OS.type == "windows") {
  download.file('http://geolocation.googlecode.com/svn/trunk/downloads/R3x/analyzepsat_3.1.tar.gz','analyzepsat_3.1.tar.gz', mode='wb')
  install.packages('analyzepsat_3.1.tar.gz', .libPaths()[1], repos = NULL, type='source')
  unlink(c('analyzepsat_3.1.tar.gz'))
# }else{
   # download.file('http://geolocation.googlecode.com/files/analyzepsat_3.1.tar.gz','analyzepsat_3.1.tar.gz', mode='wb')
  # install.packages('analyzepsat_3.1.tar.gz', repos = NULL)
  # unlink(c('analyzepsat_3.1.tar.gz'))
# }
 
## Clean up a little
rm("installed","psat.depend","psat.dep.log","psat.need")

print('If this did not install properly (especially using Linux), you may need to install tk-dev, tcl-dev and libnetcdf-xx. \n Good Luck!')
}

install.psat()