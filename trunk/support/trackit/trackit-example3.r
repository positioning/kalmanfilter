## TrackIt Example 3. Stepping up a gear - TrackIt with SST-matching (Advanced topic!)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Quick recap - we have run the drifter example with TrackIt 
##               using light only. You might not be too happy
##               about the accuracy of that model, so we now
##               we will try improve the fit by adding SST-matching
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Step 1. Load library and data; Set working directory
library(trackit)
data(drifter)
setwd('c:/trackitws')


# Step 2. Read in SST data from buoy (fitted with GPS)
sst<-read.table('driftersst.tab', head=TRUE)


# Step 3. Download Reynolds satellite-SST product from the Hawaii server
path<-get.sst.from.server(drifter, 180, 210, 10, 40)


# Step 4. Prepare track. Remember longitude is 0-360 (E)
ptsst<-prepit(drifter, sst=sst, 
              fix.first=c(360-161.45,22.85,2002,9,10,0,0,0),
              fix.last=c(360-159.87,21.95,2003,5,21,0,0,0), scan=FALSE)


# Step 5. Run the trackit model
#         We fast-forward here, it takes some tweaking to get the right parameters)
fitsst<-trackit(ptsst, bsst.ph=3, rad.ph=-1)