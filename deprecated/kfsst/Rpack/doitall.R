# -- Clean up 
rm(list=ls())
setwd('.')
unlink(c('web', 'ftp'), TRUE)
dir.create('web')
dir.create('ftp')

# -- First compile the binary (linux only)

system('cd ../kfsst; ./compilescript')


# -- Then read in the script 
source('../Rcode/kfsst-Rpack.R')

# -- Generate a base package 
package.skeleton('kfsst', list=ls(all=TRUE))

# -- Put in the binaries 
dir.create('kfsst/inst')
dir.create('kfsst/inst/admb')
system('cp ../kfsst/bin/* ./kfsst/inst/admb')
system('rm ../kfsst/bin/kfsst')

# -- Put in the tpl file  
dir.create('kfsst/inst/admb/src')
system('cp ../kfsst/source/* ./kfsst/inst/admb/src')

# -- Generate the DESCRIPTION file 
str<-'Package: kfsst
Title: Kalman Filter tracking including Sea Surface Temperature      
Version: 0.2
Author: Anders Nielsen <anders.nielsen@hawaii.edu>, John R Sibert <sibert@hawaii.edu>
Description: Archival tagged marine creatures are typically geolocated based on 
             observed light-levels. This package uses these raw geolocations and 
             observed sea surface temperatures in a coherent state-space model to 
             reconstruct the track via the extended Kalman filter.    
Depends: R (>= 2.1.0), locfit, kftrack, date
Maintainer: Anders Nielsen <anders.nielsen@hawaii.edu>
License: BSD
'
cat(str, file='kfsst/DESCRIPTION')

# -- Overwrite the help files that are auto-generated
filelist<-dir('manEdited')
for(f in filelist)file.copy(paste('manEdited/',f,sep=""),paste('kfsst/man/',f,sep=""), overwrite=TRUE)

# -- build
unlink(c('kfsst/src','kfsst/README','kfsst/man/README'),TRUE)
system('R CMD build kfsst')
system('R CMD Rd2dvi --no-preview --pdf --title=\'Reference manual for the KFSST package\'  -o KFSST-ref-manual.pdf  ./kfsst')
dir.create('kfsst/inst/doc')
system('cp KFSST-ref-manual.pdf ./kfsst/inst/doc')
system('R CMD check kfsst')
system('R CMD build kfsst')

# -- cheap build of the windows ver.
unlink('kfsst.Rcheck/kfsst/admb/kfsst') #no need for that in win
strs<-readLines('kfsst.Rcheck/kfsst/DESCRIPTION')
idx<-grep('Built',strs)
pts<-strsplit(strs[11],';')[[1]]
strs[idx]<-paste(pts[1],'',pts[3],'windows', sep='; ')
writeLines(strs[-idx], 'kfsst.Rcheck/kfsst/DESCRIPTION')
setwd('kfsst.Rcheck')
system('zip -r ../kfsst.zip kfsst')
setwd('..')

# -- cleanup 
unlink(c('doitall.R~','kfsst', 'kfsst.Rcheck', 'sst.dat'), TRUE)
system('mv kfsst_0.2.tar.gz ./ftp')
system('mv KFSST-ref-manual.pdf ./web')  
system('mv kfsst.zip ./ftp')

lines<-readLines('webcode/homepage.html')
lines<-gsub('DATEFIELD', format(Sys.time(), "%d %b %Y") ,lines)
writeLines(lines, './web/index.html')

system('cp -r ./webcode/wwwnielsen_etal ./web/')
system('cp ./webcode/install ./ftp/install')

