## TrackIt Example 2. Exporting a track

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Quick recap - we assume you have got a nice fit for your track
##               Now we will work on saving, extracting, or 
##               checking the results with a GPS double-tag/positions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###############################################################
# Before you proceed,
# Make sure you have a directory, "C:\TrackItWS"
# Copy also the R scripts that are provided into that directory
##############################################################

# Step 1. Setting working directory (only in Windows)
setwd("c:/trackitws")

#---------------
# This is done to tell R that you will work off from this directory
# And in this case, we will work from C:\TrackItWS
# Note: in R, you either use double backslash, \\ or a single forward slash, /
#       to signal to the program that you are giving it a directory path
#---------------


# Step 2a. Saving the track object
myfilename="mytrack.RData"
save(fit, file=myfilename)

# Step 2b. Alternatively, you can save up everything that you have done
#          This includes the input data or any immediate things you have assigned a value to
save.image(myfilename)


# Step 3. Extract just the track from the "fit" object
#         This will use a nice little script I put together
#         So you won't have to spend the time figuring out what things are
#         We will use the command, source(), to utilize my goodies

source('fit2csv.r')
fit2csv(fit)

#---------------
# fit2csv will dump 3 comma-separated files (.csv) into your working directory
# i.e. C:\TrackItWS
# Take a look and let me know if there are things that are unclear to you
#---------------


# Step 4. Checking the fitted track with an independent GPS location file
#         Attached to the drifter buoy, courtesy of Mike Musyl!
#         We will use two basic R commands for reading data in and plotting

# Step 4a. Read in GPS data
gps <- read.table('driftergps.tab', header=T)
head(gps)

# Step 4b. Load map with fitted track using a TrackIt function
library(trackit) # Just in case you haven't loaded the library
fitmap(fit)

# Step 4c. Plot the GPS track, as a red line and add waypoints as black dots
lines(360+gps$Gpslon, gps$Gpslat, col="red", lwd=2)
points(360+gps$Gpslon, gps$Gpslat, pch=20)


#................
# Optional: you can save plots/ graph objects with the file menu/ toolbar icons
#................
