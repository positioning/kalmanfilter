# Small script to install  analyzepsat and dependencies
# First version: Ben Galuardi March 2013 <bgaluardi@umassd.edu>
# Slight changes to match geolocation installation script for R-3.0.2
# Last edited: Oct 24 2016

install.psat = function(){
#===============================================================#
# Borrowed from   http://www.rforge.net/FSA/InstallFSA.R
#===============================================================#
# Find out what analyzepsat needs that is not already installed
installed <- library()$results[,"Package"]
psat.depend <- c('MASS','gdata','maptools','GenKern','matlab','raster','ncdf','RODBC','adehabitat','fields','ellipse','crawl','devtools','httr')
psat.dep.log <- psat.depend %in% installed
psat.need <- psat.depend[!psat.dep.log]  

## Install all packages from CRAN that are needed (be on the lookout for choosing a mirror)
if (length(psat.need)>0) install.packages(psat.need, repos="http://cran.rstudio.com/")

#================================================================#
# Analyzepsat
#================================================================#
# install kf stuff
if('ukfsst' %in% rownames(installed.packages())==F){
require(devtools)
source_url("https://raw.githubusercontent.com/positioning/kalmanfilter/master/install.r")
}

# if (.Platform$OS.type == "windows") {
  require(httr)
  llink <- 'https://github.com/positioning/kalmanfilter/raw/master/downloads/R3x/analyzepsat_3.1.tar.gz'
  lfile <- 'analyzepsat_3.1.tar.gz'
  GET(llink, write_disk(lfile, overwrite=TRUE))
  install.packages(lfile, .libPaths()[1], repos = NULL, type='source')
  unlink(lfile)
#}
 
## Clean up a little
rm("installed","psat.depend","psat.dep.log","psat.need")

print('If this did not install properly (especially using Linux), you may need to install tk-dev, tcl-dev and libnetcdf-xx. \n Good Luck!')
}

install.psat()