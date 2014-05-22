tomatic_erddap<-function(xpos, ypos, tpos, dtype, xlen, ylen, 
                              element="", savefile="", mytext="", 
                              alt.value=0, timeoff=0, mylink=NA, keep180=0){

#  Quite a bit of bad programming here...
#  incl. using "data" as a variable name, which is a function 							  
							  
#Created by Cindy Bessey, March.11, 2008
#Purpose: To translate Dave Foley's Xtractomatic Matlab code to R
#This code will be used to extract requested data via the web using erddap
#This code will follow Dave Foley's code as closely as possible.

#INPUTS:
#	xpos = longitude vector (in decimal degrees East, either 0-360 or -180 to 180)
#	ypos = latitude vector (in decimal degrees N; -90 to 90)
#	tpos = time vector; must be specified as 'YYYY-MM-DD'; quotes required)
#	dtype = data ID Code (data types listed below)
#             or at: http://coastwatch.pfel.noaa.gov/erddap
#	xlen = length of search box in longitude (decimal degrees)
#	ylen = length of search box in latitude (decimal degrees)

#OUTPUTS:
#	column 1 = mean of data within search box
#	column 2 = standard deviation of data within search box
#	column 3 = number of points found within search box
#	column 4 = actual time associated with satellite data set
#	column 5 = longitude of request (decimal degrees)
#	column 6 = latitude of latitude (decimal degrees)
#       column 7 = time of request
#       column 8 = Median of data within search box
#       column 9 = Median Absolute Deviation within search box
# Revisions:
#
# 7 Feb 2010   - Major bug in program execution fixed, it was producing data extractions for all permutations
# DGF            of the input time and space coordinates.  It now treats them as companion vectors and provides
#                one set of geophysical data for each x,y,t triplet.  
#                (i.e., (x1,y1,t1), (x2,y2,t2),..., (xn,yn,tn))
#
#              - Program now returns time of requested position as well as the "centered" time of the 
#                actual satellite data from which the extraction is made.
#
#              - Program also now provides estimates of the median and the median absolute deviation, as
#                more robust statistics.  These are advised for data sets such as chlorophyll which generally
#                do not exist in nature in a normal distribution.
#
#              - Program now accepts id code names available from the ERDDAP URL given below: not just numbers.
#                In fact the use of the number codes is now deprecated and will not be supported in the next 
#                version xtracto.  (One of the dumber things I've done - My bad, Dave).
#
#	       - Program now searches for the nearest data set in time for the xtraction at each point 
#                specified by the user, and not just those times which coincide with the centered times of 
#                a given data set.  E.g., is if the user wants data from the 8-day MODIS chl data for 
#                January 2nd, 2009 the program will extract the data from the January 1-8 composite image.  
#                This is how the program is supposed to work.  If there are overlapping data sets, it chooses 
#                the time slice whose centered time is closest to that of the user request. 
#
#
# 7 May 2010     - adjusted definitions of "search radius" to search box.  This original intent was to 
#   DGF            have the user input the length and width of the box.  The description now matches this.
#     

# Sample Calls:
# to extract Seawifs 8-day Primary Productivity in 22 km x 22km boxes
#   surrounding a point(i.e. 230E, 40N, 2006-01-01).
# extract<-xtractomatic_erddp(xpos=230, ypos=40, tpos=c('2006-01-01'), dtype='41', xlen=.2, ylen=.2)
#
# to extract pathfinder SST 8-day mean and median data in an 11km x 11km box around a centered point
# extract<-xtractomatic_erddap(xpos=230, ypos=40, tpos=c('2006-01-01'), dtype='18', xlen=.1, ylen=.1)
#
# To extract for multiple track points, merely provide the vectors for lon, lat, and time
#  extract<-xtractomatic_erddap(xpos=c(230,235), ypos=c(40,45), tpos=c('2006-01-01','2006-01-02'), dtype='sst', xlen=.1, ylen=.1)  
#
# 
# see the following link to get data codes and full data set information
# http://coastwatch.pfel.noaa.gov/erddap
#
# This is version 1.0.2  Released 12 Feb 2010.  DGF

#Determine if ncdf package is installed on users computer
packs<-search()
xx<-rep(NA,length(packs))
for(i in 1:length(packs)){if(packs[i]=="package:ncdf"){xx[i]<-1}else{xx[i]<-0}}
xx<-as.numeric(xx)
if(sum(xx)<1){stop("This program requires the use of the'ncdf' package. Download package from http://www.r-project.org/
Load package using menus; PC: Packages, Load Package, Select ncdf. MAC: Packages&Data, Package Manager, Select 'ncdf' to load  ")
stop
}

#Load ncdf package
library('ncdf', character.only = TRUE)

# make sure data type input is a string and not a number
# dtype = tolower(as.character(dtype));

# assume any long string is an accurate erd data type
if (nchar(dtype) > 0 ) {
  dataset<-dtype
}
else {

#List of Data Available

# AVHRR HRPT 1.4 km nighttime hourly SST data for West coast
	if(dtype=='1'|| dtype=='atsstnhday'){dataset<-'erdATsstnhday'}

# AVHRR HRPT 1.4 km daytime SST data for West coast
	if(dtype=='2'||dtype=='atssdhday'){dataset<-'erdATsstdhday'}

# AVHRR HRPT 1,4 km night and day SST 1-day composite
	if(dtype=='3'||dtype=='atssta1day'||dtype=='hrpt'||dtype=='avhrr hrpt')
       {dataset<-'erdATssta1day'}

# AVHRR HRPT 1.4 km night and day SST 3-day composite
	if(dtype=='4'||dtype=='atssta3day'){dataset<-'erdATssta3day'}

# AVHRR HRPT 1.4 km night and day SST 8-day composite
	if(dtype=='5'||dtype=='atssta8day'){dataset<-'erdATssta8day'}

# AVHRR HRPT 1.4 km night and day SST 14-day composite
	if(dtype=='6'||dtype=='atssta14day'){dataset<-'erdATssta14day'}

# AVHRR HRPT 1.4km night and day SST monthly composite
	if(dtype=='7'||dtype=='atsstamday'){dataset<-'erdATsstamday'}

# AVHRR GAC SST 11km 1-day composite
	if(dtype=='8'||dtype=='agssta1day'){dataset<-'erdAGssta1day'}

# AVHRR GAC SST 11km 3-day composite
	if(dtype=='9'||dtype=='agssta3day'){dataset<-'erdAGssta3day'}

# AVHRR GAC SST 11km 8-day composite
	if(dtype=='10'||dtype=='agssta8day'){dataset<-'erdAGssta8day'}

# AVHRR GAC SST 11km 14-day composite
	if(dtype=='11'||dtype=='agssta14day'){dataset<-'erdAGssta14day'}

# AVHRR GAC SST 11km monthly composite
	if(dtype=='12'||dtype=='agsstamday'){dataset<-'erdAGsstamday'}

# GOES SST 5.5 km 1-day composite
	if(dtype=='13'||dtype=='gassta1day'||dtype=='goes sst'||dtype=='goes'||dtype=='geostationary')
       {dataset<-'erdGAssta1day'}

# GOES SST 5.5 km 3-day composite
	if(dtype=='14'||dtype=='gassta3day'){dataset<-'erdGAssta3day'}

# GOES SST 5.5 km 8-day composite
	if(dtype=='15'||dtype=='gassta8day'){dataset<-'erdGAssta8day'}

# GOES SST 5.5 km 14-day composite
	if(dtype=='16'||dtype=='gassta14day'){dataset<-'erdGAssta14day'}

# Pathfinder v5 5.5km SST 1-day composite
	if(dtype=='17'||dtype=='phssta1day'){dataset<-'erdPHssta1day'}

# Pathfinder v5 5.5km SST 8-day composite
	if(dtype=='18'||dtype=='phssta8day'||dtype=='pathfinder'||dtype=='sst'||dtype=='avhrr'||dtype=='sea surface temperature')
      {dataset<-'erdPHssta8day'}

# Pathfinder v5 5.5km SST monthly composite
	if(dtype=='19'||dtype=='phsstmday'||dtype=='monthly sst')
      {dataset<-'erdPHsstamday'}

# MODIS Aqua 2.5 km chla 1-day composite
	if(dtype=='20'||dtype=='mbchla1day'){dataset<-'erdMBchla1day'}

# MODIS Aqua 2.5 km chla 8-day composite
	if(dtype=='21'||dtype=='mbchla8day'||dtype=='chla'||dtype=='chlorophyll'||dtype=='modis chl'||dtype=='modis aqua')
      {dataset<-'erdMBchla8day'}

# MODIS Aqua 2.5 km chla 14-day composite
	if(dtype=='22'||dtype=='mbchla14day'){dataset<-'erdMBchla14day'}

# Jason-1 25km SSH deviation, 10-day composite
	if(dtype=='23'||dtype=='j1sshd10day'||dtype=='ssh'||dtype=='ssha'||dtype=='jason'||dtype=='sea surface height')
       {dataset<-'erdJ1sshd10day'}

# Quikscat 25km zonal wind, 1-day composite; x_wind
	if(dtype=='24'||dtype=='qnux101day')
	{dataset<-'erdQNwind1day'
 	element<-'x_wind'}

# Quikscat 25 km meridional wind, 1-day composite; y_wind
	if(dtype=='25'||dtype=='qnuy101day')
	{dataset<-'erdQNwind1day'
	element<-'y_wind'}

# Quikscat 25 km zonal wind, 3-day composite;
	if(dtype=='26'||dtype=='qnux103day'||dtype=='zonal wind'||dtype=='ux10')
       {dataset<-'erdQNwind3day'
	element<-'x_wind'}

# Wind QuikSCAT, Global, Near Real Time, Merid. (3 Day Composite)
if(dtype=='27'||dtype=='qnuy103day'||dtype=='meridional wind'||dtype=='uy10')
       {dataset<-'erdQNwind3day'
	element<-'y_wind'}

# Wind, QuikSCAT, Global, Near Real Time, Modulus (3 Day Composite)
if(dtype=='28'||dtype=='qnumod3day'||dtype=='wind speed'||dtype=='wind modulus')
       {dataset<-'erdQNwind3day'
	element<-'mod'}

# Wind Stress, QuikSCAT, Global, Near Real Time, Curl (3 Day Composite)
if(dtype=='29'||dtype=='qncurl3day'||dtype=='curl'||dtype=='wind stress curl'||dtype=='curl of wind stress')
       {dataset<-'erdQNstress3day'
	element<-'curl'}

# Wind, QuikSCAT, Global, Near Real Time, Zonal (8 Day Composite)
if(dtype=='30'||dtype=='qnux108day')
       {dataset<-'erdQNwind8day'
	element<-'x_wind'}

# Wind, QuikSCAT, Global, Near Real Time, Merid. (8 Day Composite)
if(dtype=='31'||dtype=='qnuy108day')
       {dataset<-'erdQNwind8day'
	element<-'y_wind'}

# Wind, QuikSCAT, Global, Near Real Time, Modulus (8 Day Composite)
if(dtype=='32'||dtype=='qnumod8day')
       {dataset<-'erdQNwind8day'
	element<-'mod'}

# Wind Stress, QuikSCAT, Global, Near Real Time, Curl (8 Day Composite)
if(dtype=='33'||dtype=='qncurl8day')
       {dataset<-'erdQNstress8day'
	element<-'curl'}

# Wind, QuikSCAT, Global, Near Real Time, Zonal (14 Day Composite)
if(dtype=='34'||dtype=='qnux1014day')
       {dataset<-'erdQNwind14day'
	element<-'x_wind'}

# Wind, QuikSCAT, Global, Near Real Time, Merid. (14 Day Composite)
if(dtype=='35'||dtype=='qnuy1014day')
       {dataset<-'erdQNwind14day'
	element<-'y_wind'}

# Wind, QuikSCAT, Global, Near Real Time, Modulus (14 Day Composite)
if(dtype=='36'||dtype=='qnumod14day')
       {dataset<-'erdQNwind14day'
	element<-'mod'}

# Wind Stress, QuikSCAT, Global, Near Real Time, Curl (14 Day Composite)
if(dtype=='37'||dtype=='qncurl14day')
       {dataset<-'erdQNstress14day'
	element<-'curl'}

# Wind Stress, QuikSCAT, Global, Near Real Time, Curl (Monthly Composite)
if(dtype=='38'||dtype=='qncurlmday')
       {dataset<-'erdQNstressmday'
	element<-'curl'}

# Wind, QuikSCAT, Global, Near Real Time, Zonal (Monthly Composite)
if(dtype=='39'||dtype=='qnux10mday')
       {dataset<-'erdQNwindmday'
	element<-'x_wind'}

# Wind, QuikSCAT, Global, Near Real Time, Merid. (Monthly Composite)
if(dtype=='40'||dtype=='qnuy10mday')
       {dataset<-'erdQNwindmday'
	element<-'y_wind'}

# Primary productivity, 8-day, seawifs chl.
if(dtype=='41'||dtype=='ppbfp18day'||dtype=='primary productivity'||dtype=='seawifs productivity')
       {dataset<-'erdPPbfp18day'}

# Primary productivity, monthly, seawifs chl
if(dtype=='42'||dtype=='Tppbfp1mday'||dtype=='monthly productivity')
       {dataset<-'erdPPbfp1mday'}

# GOES frontal index 14-day
if(dtype=='43'||dtype=='gatfnt14day'||dtype=='GOES fronts'||dtype=='frontal index'||dtype=='frontal probability')
      {dataset<-'erdGAtfnt14day'}

# Front Probability, GOES Imager, Western Hemisphere, EXPERIMENTAL (Monthly Composite)
if(dtype=='44'||dtype=='gatfntmday')
       {dataset<-'erdGAtfntmday'}

# Wind Stress, QuikSCAT, Global, Near Real Time, Curl (4 Day Composite)
if(dtype=='45'||dtype=='qncurl4day')
	{dataset<-'erdQNstress4day'
	element<-'curl'}

# Wind, QuikSCAT, Global, Near Real Time, Zonal (4 Day Composite)
if(dtype=='46'||dtype=='qnux104day')
	{dataset<-'erdQNwind4day'
	element<-'x_wind'}

# Wind, QuikSCAT, Global, Near Real Time, Merid. (4 Day Composite)
if(dtype=='47'||dtype=='qnuy104day')
	{dataset<-'erdQNwind4day'
	element<-'y_wind'}

# Sea Surface Height Deviation, Aviso, Global, Science Quality (1 Day Composite)
if(dtype=='48'||dtype=='tasshd1day')
	{dataset<-'erdTAssh1day'
	element<-'sshd'}

# Sea Surface Height, Aviso, Global, Science Quality (1 Day Composite)
if(dtype=='49'||dtype=='tassh1day')
	{dataset<-'erdTAssh1day'
	element<-'ssh'}

# SST, Blended, Global, EXPERIMENTAL (5 Day Composite)
if(dtype=='50'||dtype=='bassta5day'){dataset<-'erdBAssta5day'}

# Chlorophyll-a, Aqua MODIS, NPP, Global, Science Quality (8 Day Composite)
if(dtype=='51'||dtype=='mhchla8day'){dataset<-'erdMHchla8day'}

# Diffuse Attenuation K490, Aqua MODIS, NPP, Global, Science Quality (8 Day Composite)
if(dtype=='52'||dtype=='mhk4908day'){dataset<-'erdMHk4908day'}

# SST, Aqua MODIS, NPP, Global, Daytime, Science Quality (8 Day Composite)
if(dtype=='53'||dtype=='mhsstd8day'){dataset<-'erdMHsstd8day'}

# Fluorescence, Aqua MODIS, NPP, West US, Science Quality (8 Day Composite)
if(dtype=='54'||dtype=='mhcflh8day'){dataset<-'erdMHcflh8day'}

# Wind Stress, QuikSCAT, Global, Science Quality, Curl (3 Day Composite)
if(dtype=='55'||dtype=='qscurl3day')
	{dataset<-'erdQSstress3day'
	element<-'curl'}

# Wind, QuikSCAT, Global, Science Quality, Modulus (3 Day Composite)
if(dtype=='56'||dtype=='qsumod3day')
	{dataset<-'erdQSwind3day'
	element<-'mod'}

# Wind, QuikSCAT, Global, Science Quality, x__wind (3 Day Composite)
if(dtype=='57'||dtype=='qsux103day')
	{dataset<-'erdQSwind3day'
	element<-'x_wind'}

# Wind, QuikSCAT, Global, Science Quality, y__wind (3 Day Composite)
if(dtype=='58'||dtype=='qsuy103day')
	{dataset<-'erdQSwind3day'
	element<-'y_wind'}

# Wind Stress, QuikSCAT, Global, Science Quality, Taux (3 Day Composite)
if(dtype=='59'||dtype=='qstau3day')
	{dataset<-'erdQSstress3day'
	element<-'taux'}

# Wind Stress, QuikSCAT, Global, Science Quality, Tauy (3 Day Composite)
if(dtype=='60'||dtype=='qstau3day')
	{dataset<-'erdQSstress3day'
	element<-'tauy'}

# Ekman Upwelling, QuikSCAT, Global, Science Quality (8 Day Composite)
if(dtype=='61'||dtype=='qswekm8day')
	{dataset<-'erdQSstress8day'
	element<-'upwelling'}

# Ekman Upwelling, QuikSCAT, Global, Science Quality (Monthly Composite)
if(dtype=='62'||dtype=='qswekmmday')
	{dataset<-'erdQSstressmday'
	element<-'upwelling'}

# Chlorophyll-a, Orbview-2 SeaWiFS, Global (8 Day Composite)
if(dtype=='63'||dtype=='swchla8day'){dataset<-'erdSWchla8day'}

# Chlorophyll-a, Orbview-2 SeaWiFS, Global (Monthly Composite)
if(dtype=='64'||dtype=='swchlamday'){dataset<-'erdSWchlamday'}

# Chlorophyll-a, Aqua MODIS, NPP, Global, Science Quality (Monthly Composite)
if(dtype=='65'||dtype=='mhchlamday'){dataset<-'erdMHchlamday'}

# Front Probability, GOES Imager, Western Hemisphere, EXPERIMENTAL (10 Day Composite)
if(dtype=='66'||dtype=='gatfnt10day'){dataset<-'erdGAtfnt10day'}

# Chlorophyll-a, Orbview-2 SeaWiFS, West US, Science Quality (Monthly Composite)
if(dtype=='67'||dtype=='sachlamday'){dataset<-'erdSAchlamday'}

# Sea Surface Height Deviation, Aviso, Global, Science Quality (Monthly Composite)
if(dtype=='68'||dtype=='tasshdmday')
	{dataset<-'erdTAsshmday'
	element<-'sshd'}

# Sea Surface Height Deviation, Aviso, Global, Science Quality (Monthly Composite)
if(dtype=='68'||dtype=='tasshmday')
	{dataset<-'erdTAsshmday'
	element<-'sshd'}
}

#===================== skip to here ===============================

# breakup string into components
satid<-substring(dataset,first=4, last=5)
param<-substring(dataset,first=6, last=8)

# Determine product name of requested data as listed in the url
productname<-NA
if(param=='sst'){productname<-'sst'}
if(param=='chl'){productname<-'chlorophyll'}
if(param=='ssh'){productname<-'ssh'}

if(param=='win'){productname<-'wind'}
if(param=='str'){productname<-'stress'}

if(param=='bfp'){productname<-'productivity'}
if(param=='tfn'){productname<-'front'}
if(param=='k49'){productname<-'k490'}
if(param=='cfl'){productname<-'fluorescence'}
if(param=='wek'){productname<-'upwelling'}

# default URL for NMFS/SWFSC/ERD grid data from erddap server
# 'http://oos.soest.hawaii.edu/erddap/griddap/'
# 'http://upwell.pfeg.noaa.gov/erddap/griddap/'
if (mylink=='') mylink <- NA
urlbase <- ifelse(is.na(mylink),'http://coastwatch.pfel.noaa.gov/erddap/griddap/',mylink)

# get list of available time periods
# First, make bad call to CW page,
requesttime <- NA
if (timeoff==0) {
bobcallbad<-paste(urlbase,dataset,'.csv?time',sep="")
string<-readLines(bobcallbad)
# sift through the crap for ISO dates
stind<-regexpr('....-..-..',string)
endind<-regexpr('....-..-..',string)
tt.stind<-rep(NA,length(stind))
for(i in 1:length(stind)){if(stind[i]>-1){tt.stind[i]<-i}}
tt.stind<-na.omit(tt.stind)
tt.endind<-rep(NA,length(endind))
for(i in 1:length(endind)){if(endind[i]>-1){tt.endind[i]<-i}}
tt.endind<-na.omit(tt.endind)
alldates<-rep(NA,length(tt.stind))

for(i in 1:length(tt.stind)){
str2<-string[tt.stind[i]:tt.endind[i]]
stind2 = regexpr('....-..-..',str2)
	for(ii in 1:length(stind2)){
	if(stind2[ii]>-1){
	alldates[i]<-substring(str2, first=stind2[ii],last=stind2[ii]+9)
     }
    }
}
alldates<-na.omit(alldates)
btime<-unique(alldates)
# convert from character to date
sattime<-as.Date(btime,format="%Y-%m-%d")}

# handle case of -180 to 180 longitude
for(i in 1:length(xpos)){if(xpos[i]<0){xpos[i]<-xpos[i]+360}}
if (keep180==1){ 
   for(i in 1:length(xpos)){if(xpos[i]>180){xpos[i]<-xpos[i]-360}}
}

# loop on positions
out.dataframe<-as.data.frame(matrix(ncol=9,nrow=length(xpos)))

if(productname=='wind'||productname=='stress'||nchar(element)>0){
dimnames(out.dataframe)[[2]]<-c(paste(element,'-mean',sep=""),'stdev','n','satellite date','requested lon','requested lat','requested date','median','mad')
}
else
dimnames(out.dataframe)[[2]]<-c(paste(productname,'-mean',sep=""),'stdev','n','satellite date','requested lon','requested lat','requested date','median','mad')
for(i in 1:length(xpos)){

# define bounding box
xmax<-xpos[i]+xlen/2
xmin<-xpos[i]-xlen/2
ymax<-ypos[i]+ylen/2
ymin<-ypos[i]-ylen/2

# if position is within bounds, get time/space indices for extraction
if (timeoff==1){
  sattime <- range(tpos)
}
if (tpos[i] <= max(sattime) && tpos[i] >= min(sattime)){

# find closest time of available data
if (timeoff==0) requesttime<-as.character(sattime[which.min(abs(as.numeric(sattime-as.Date(tpos[i]))))]) 

alt.value <- ifelse(alt.value > 0, paste("[(", alt.value,  ")]", sep=""), '')
# handle fact that erddap groups data sets - by some protocol I do not understand.
if (productname=='wind'||productname=='stress'||productname=='ssh'||nchar(element)>0){
    if (timeoff==0){
	bobcall = paste(urlbase, dataset, '.nc?', element, 
                    '[(',requesttime,'):1:(',requesttime,')]',
                    alt.value,
                    '[(',ymax,'):1:(',ymin,')][(',xmin,'):1:(',xmax,')]',sep="")}
   else{
               bobcall = paste(urlbase, dataset, '.nc?', element, 
                    alt.value,
                    '[(',ymax,'):1:(',ymin,')][(',xmin,'):1:(',xmax,')]',sep="")}
}
else  
{   if (timeoff==0){
    bobcall = paste(urlbase, dataset, '.nc?', productname, 
                    '[(',requesttime,'):1:(',requesttime,')]',
                    alt.value,
                    '[(',ymax,'):1:(',ymin,')][(',xmin,'):1:(',xmax,')]',sep="")}
   else{
               bobcall = paste(urlbase, dataset, '.nc?', productname, 
                    alt.value,
                    '[(',ymax,'):1:(',ymin,')][(',xmin,'):1:(',xmax,')]',sep="")}
}

print(bobcall)

# extract data array and import to R depending on structure
varname<-paste(substring(satid,first=2,last=3),param,sep="")

#Download the data from the website to the current R directory
destfile<-getwd()
xmean<-xpos[i]
ymean<-ypos[i]
fileout<-paste(destfile,"/tmp",xmean,"E",ymean,"N_",i,".nc",sep="")
download.file(bobcall,destfile=fileout,
              cacheOK=TRUE,mode="wb", quiet=T)

# Ensure the nc file contains data and not a text error

test<-readLines(fileout, warn=FALSE)
ind.test<-rep(NA,length(test))
for(tt in 1:length(test)){ind.test[tt]<-regexpr("There was an error",test[tt])}
if(abs(sum(ind.test))!=length(ind.test)){
out.dataframe
stop("There was an error in the url call.
There is no data; probably because the region requested is beyond the region with data in the source data \
file.")
}

out.data<-open.ncdf(fileout)

#Display in a dataframe

if(productname=='stress' || productname=='wind' || productname=='ssh' || nchar(element)>0){
data<-as.vector(get.var.ncdf(out.data, varid=element))}
else
{data<-as.vector(get.var.ncdf(out.data, varid=productname))}

# Handling missing value, only for varid = element; doesn't care about 'productname'
mval <- att.get.ncdf(out.data, varid=element, attname='_FillValue')$value
data[which(data==mval)] <- NA

out.dataframe[i,1]<-mean(data,na.rm=T)
out.dataframe[i,2]<-sd(data,na.rm=T)
out.dataframe[i,3]<-length(na.omit(data))
out.dataframe[i,4]<-requesttime
out.dataframe[i,5]<-xpos[i]
out.dataframe[i,6]<-ypos[i]
out.dataframe[i,7]<-tpos[i]
out.dataframe[i,8]<-median(data,na.rm=T)
out.dataframe[i,9]<-mad(data,na.rm=T)

mydat <- out.dataframe[i,]
mydat$notes <- mytext

### Writing data to file ###
if (nchar(savefile)>0){
  write.table(mydat, file=savefile, col.names=F, row.names=F,
              quote=F, append=T, sep=",")
}

if(is.na(out.dataframe[i,1])==TRUE){print("NOTE: The resulting output file contains NA; no data available for the requested location")}

Sys.sleep(0.3)
close.ncdf(out.data)}

else{print("Ensure tpos is contained in quotes.  If so, then the tpos requested is out of bounds")
print("Data are available for the following dates:")
print(btime)
}

}
warning(-1,call.=FALSE,immediate.=FALSE)
#print("NOTE: Any warnings regarding 'In regexp' can be ignored; They result from the method chosen to sift through text data when determining available dates for each data set.")
out.dataframe
}
