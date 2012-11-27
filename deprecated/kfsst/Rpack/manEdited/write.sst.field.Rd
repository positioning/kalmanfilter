\name{write.sst.field}

\alias{write.sst.field}

\title{Writes a smoothed version of the SST-field to a file}

\description{
  Staring from a directory (\code{datadir}) of data files 
  each containing a grid of latitude, longitude, and 
  corresponding SST measurements, this function reads the 
  files one by one and spatially smooths the SST-field, 
  computes the gradient of the smooth field, and write these 
  informations to a combined file (\code{filename}). 
  The original data files each represent the SST-field at a 
  specific time interval (say a week). The time information 
  is contained in the name of each file.          
}

\usage{
write.sst.field(datadir, nlon = 100, nlat = 150, filename = "sst.dat", alpha = 0.05, 
  from.ystr = c(3, 6), from.dstr = c(7, 9), to.ystr = c(11, 14), to.dstr = c(15, 17), 
  peak = FALSE)
}

\arguments{
  \item{datadir}{Is the folder where the raw data files are located}
  \item{nlon}{The smoothed SST-fields and their gradient-fields are 
              represented on a \code{nlon}-by-\code{nlat} grid}
  \item{nlat}{The smoothed SST-fields and their gradient-fields are 
              represented on a \code{nlon}-by-\code{nlat} grid}
  \item{filename}{Is the name of the file where the smoothed fields 
                  are saved}
  \item{alpha}{Is a scalar between 0 and 1 determining the degree of 
               smoothing used. The default is 5\%, which means that 
               the smoothed field at any point is calculated from 
               the 5\% nearest points in the observed data.}
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
  \item{peak}{If \code{TRUE} allows to visually compare the raw and the 
                 smoothed field.}		 
}

\details{
  The grid size of the internal representation can be finer or coarser 
  than the actual data set, and should be chosen based on size of the 
  area. This way of representing the SST-field is clearly sub-optimal, 
  and will hopefully be replaced in later versions. 

  The default smoothing scale of 5\% is probably coarse in many cases, 
  especially if the area is large. 

  It is recommended to carry out sensitive analysis with respect to the 
  degree of smoothing and the grid size of the internal representation.  

  The smoothing is presently done via the \code{locfit} R-package. 
}

\value{
  The filename returned from the function is where the internal 
  representation is saved. 
}

\author{Anders Nielsen \email{anielsen@dina.kvl.dk}, John Sibert \email{sibert@hawaii.edu}}

\seealso{\code{\link{kfsst}}, \code{\link{blue.shark}}}

\examples{
  # No example supplied here, but check out the example 
  # in the blue.shark dataset documentation
}
\keyword{models}

