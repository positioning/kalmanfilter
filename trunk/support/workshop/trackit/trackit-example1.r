## TrackIt Example 1. Quick run with the "drifter" data set
## February 2010 IWSS Meeting
## Chi Lam; chihinl@usc.edu

# Step 1. Load library
library(trackit)


###############################################################
# Before you proceed,
# Make sure you have a directory, "C:\TrackItWS"
# Copy also the R scripts that are provided into that directory
##############################################################


# Step 2. Setting working directory (only in Windows)
setwd("c:/trackitws")

#---------------
# This is done to tell R that you will work off from this directory
# And in this case, we will work from C:\TrackItWS
# Note: in R, you either use double backslash, \\ or a single forward slash, /
#       to signal to the program that you are giving it a directory path
#---------------

# Step 3. Get data. In this example, we will use the data that comes with the trackit package
#	  "read.table" is a generic function to read-in a text file, 
#         and in our case, drifterlight.tab, which holds the light curve data
#         We will take a quick look at the data set with the command, head()

drifter <- read.table("drifterlight.tab", header=T)
head(drifter)

#................
# Optional: Use the R help with the question mark "?" in front of a function or preloaded data set
            ?read.table
#................


# Step 3. Prepare the track data
#         Model One - Use light-data only
pt<-prepit(drifter, fix.first=c(360-161.45,22.85,2002,9,10,0,0,0),
                         fix.last=c(360-159.87,21.95,2003,5,21,0,0,0), scan=FALSE)

#---------------
# Optional: type ?prepit in the R command line to see the specifics of each input arguments
# Tips: Longitude is 0-360 (deg E), so make sure you are aware of that!
#---------------


# Step 4a. Run the trackit model
fit<-trackit(pt)

#---------------
# Note: This model setup will probably fail (unless you are on a linux machine)
#       This is intentional though, to demonstrate what failure may look like
#---------------

# Step 4b. Model can't converge, try again!
fit<-trackit(pt, D.ph=2)

# Step 4c. Success - just type in the name of the object to show the results, e.g. fit2
fit

#---------------
# Tips: A nicely formatted summary will apppear, follow the instructions in the last line 
#       to show the subitems, using the dollar sign operator, $
#       We will go through how to read the summary statistics a little bit
#       ?trackit will provide a description of the model parameters if they are unclear to you
#       Ask me if you don't understand any terms in that summary
#---------------

#................
# Optional: Use the R help with the question mark "?" in front of a function or preloaded data set
            fit$most.prob.track
#................


# Step 5. Plot up the results
plot(fit)
fitmap(fit)
