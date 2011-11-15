\name{prepit}
\alias{prepit}

\title{Prepare a track for light based geolocation}
\description{
This function prepares the track for light based geolocation, by 
converting it to the right format, pre-computing various variables, 
and selecting the intervals around sunrise and sunset where the 
data is used in the geolocation model (if needed). 
}
\usage{
prepit(track, fix.first, fix.last, scan = TRUE, window = c(0.05, 0.01), 
       tmpfile = "input.dat", datfile = "ukf.dat", 
       keepfiles = c(FALSE, FALSE), internal = TRUE, sst=NULL, 
       from.ystr=c(3,6), from.dstr=c(7,9), to.ystr=c(11,14), to.dstr=c(15,17),
       localsstfolder=NULL)
}
\arguments{
  \item{track}{A data frame with the columns \code{year} \code{month} 
  \code{day} \code{hour} \code{min} \code{sec} \code{depth} \code{light} 
  \code{temp}. The temperature is not yet used by the model, so just 
  put zeroes in that column.}
  \item{fix.first}{A vector of eight numbers giving the (longitude, 
        latitude, year, month, day, hour, minute, second) of the release 
        position. Longitude in degrees east, and time as Greenwich Mean 
        Time (GMT).}
  \item{fix.last}{A vector of eight numbers giving the (longitude, 
        latitude, year, month, day, hour, minute, second) of the recapture 
        position. Longitude in degrees east, and time as Greenwich Mean 
        Time (GMT).}
  \item{scan}{This can be either \code{TRUE} (default) or \code{FALSE}. 
        If \code{TRUE} a scanning algorithm (described in the reference) 
        is applied to locate the influential parts of the data series, 
        which are the intervals around sunrise and sunsets. If 
        \code{FALSE} it is assumed that data only consists of data near 
        sunrise and sunsets, which is for instance true if they come from 
        a PAT tag.} 
  \item{window}{If \code{scan=TRUE} these two numbers determines how large 
        an interval around each solar event is used. First the mid-point 
        is selected as the point that best separates day from night 
        (details in reference), and then two numbers are used in the 
        following way: The first number is the fraction (of a 24-hour 
        period) from the mid-point towards the day side of the solar event. 
        The second number is the fraction towards the night side. So the 
        default \code{c(0.05, 0.01)} corresponds to using an interval that 
        14.4 minutes towards the night side and 72 minutes towards the day 
        side. 86.4 minutes total.}
  \item{tmpfile}{A string specifying the name of a temporary file.}
  \item{datfile}{A string specifying the name of the file to be used by the 
        function \code{\link{trackit}}}
  \item{keepfiles}{A logical vector of two elements. First indicates if the 
        temporary file is to be saved. Second indicates if the data file 
        for \code{\link{trackit}} is to be saved. The default is 
        \code{FALSE} in both cases, which means that everything needed is  
        stored in the object returned from this function.}
  \item{internal}{Logical indicating if the scanned track is to be stored 
        in the returned object (default). If the track is long it may be 
        advisable to turn this off in combination with 
        \code{keepfile=c(FALSE,TRUE)}.}
  \item{sst}{A matrix containing columns year month day hour min sec sst}
  \item{from.ystr}{Is an integer vector with two elements describing 
                   what part of the file name describe the year of the 
                   first date the data file represents. For instance if 
                   the names of the data files all have the format 
                   \code{RSyyyyddd_YYYYDDD.dat}, where \code{yyyy} is 
                   the year of the first date the argument should be 
                   \code{c(3,6)}.}
  \item{from.dstr}{Is an integer vector with two elements describing 
                   what part of the file name describe the 'number of 
                   days into the year' of the first date the data file 
                   represents.}
  \item{to.ystr}{Is similar to \code{from.ystr}, but here for the year 
                 of the last date the data file represents.}
  \item{to.dstr}{Is similar to \code{from.dstr}, but here for the 'number 
                 of days into the year' of the last date the data file 
                 represents.}  
  \item{localsstfolder}{If the SST source is a bunch of files in a local 
        folder this is where the folder name is given as a string}
}
\details{
  See reference. 
}
\value{
  A list containing all data needed in the model \code{\link{trackit}}, or 
  references to files containing the data. 
}
\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (submitted).}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu} and 
        John Sibert \email{sibert@hawaii.edu}}

\seealso{\code{\link{trackit}}}
\examples{
  # No examples provided here, but try the ones in ?drifter and ?mooring
}

\keyword{models}

