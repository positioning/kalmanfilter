# -- Clean up 
rm(list=ls())
setwd('.')
unlink(c('web'), TRUE)
dir.create('web')

# -- First compile the binary (linux only)

system('cd ../ukfsst; ./compilescript')


# -- Then read in the script 
source('../Rcode/ukfsst-Rpack.R')

# -- Generate a base package 
package.skeleton('ukfsst', list=ls(all=TRUE))

# -- Put in the binaries 
dir.create('ukfsst/inst')
dir.create('ukfsst/inst/admb')
system('cp ../ukfsst/bin/* ./ukfsst/inst/admb')
system('rm ../ukfsst/bin/ukfsst')

# -- Put in the tpl file  
dir.create('ukfsst/inst/admb/src')
system('cp ../ukfsst/source/* ./ukfsst/inst/admb/src')

# -- Generate the DESCRIPTION file 
str<-'Package: ukfsst
Title: Kalman Filter tracking including Sea Surface Temperature      
Version: 0.3
Author: Anders Nielsen <anders.nielsen@hawaii.edu>, John R Sibert <sibert@hawaii.edu>
Description: Archival tagged marine creatures are typically geolocated based on 
             observed light-levels. This package uses these raw geolocations and 
             observed sea surface temperatures in a coherent state-space model to 
             reconstruct the track via the extended Kalman filter.    
Maintainer: Anders Nielsen <anders.nielsen@hawaii.edu>
Depends: R (>= 1.8.0), date, kftrack
License: BSD
'
cat(str, file='ukfsst/DESCRIPTION')

# -- Overwrite the help files that are auto-generated
filelist<-dir('manEdited')
for(f in filelist)file.copy(paste('manEdited/',f,sep=""),paste('ukfsst/man/',f,sep=""), overwrite=TRUE)

# -- build
unlink(c('ukfsst/src','ukfsst/README','ukfsst/man/README'),TRUE)
system('R CMD build ukfsst')
system('R CMD Rd2dvi --no-preview --pdf --title=\'Reference manual for the ukfsst package\'  -o ukfsst-ref-manual.pdf  ./ukfsst')
dir.create('ukfsst/inst/doc')
system('cp ukfsst-ref-manual.pdf ./ukfsst/inst/doc')
system('R CMD check ukfsst')
system('R CMD build ukfsst')

# -- cheap build of the windows ver.
unlink('ukfsst.Rcheck/ukfsst/admb/ukfsst') #no need for that in win
strs<-readLines('ukfsst.Rcheck/ukfsst/DESCRIPTION')
idx<-grep('Built',strs)
pts<-strsplit(strs[11],';')[[1]]
strs[idx]<-paste(pts[1],'',pts[3],'windows', sep='; ')
writeLines(strs[-idx], 'ukfsst.Rcheck/ukfsst/DESCRIPTION')
setwd('ukfsst.Rcheck')
system('zip -r ../ukfsst.zip ukfsst')
setwd('..')

# -- cleanup 
unlink(c('doitall.R~','ukfsst', 'ukfsst.Rcheck', 'sst.dat'), TRUE)
system('mv ukfsst_*.tar.gz ./web')
system('mv ukfsst-ref-manual.pdf ./web')  
system('mv ukfsst.zip ./web')

lines<-readLines('webcode/homepage.html')
lines<-gsub('DATEFIELD', format(Sys.time(), "%d %b %Y") ,lines)
writeLines(lines, './web/index.html')

system('cp -r ./webcode/wwwukfsst ./web/')
system('cp ./webcode/install ./web/install')
