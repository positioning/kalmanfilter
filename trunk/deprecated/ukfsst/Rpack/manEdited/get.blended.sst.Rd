\name{get.blended.sst}
\alias{get.blended.sst}

\title{Get SST-field from blended source}
\description{
  This function allows easy access to a blended SST database. 
}
\usage{
get.blended.sst(track, folder = tempdir(), 
  server = "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=", 
  nday = "5day")
}
\arguments{
  \item{track}{A single \code{data.frame} containing a track or a 
               \code{list} of \code{data.frame}s each containing a 
               track. The idea is that the function should only 
               download the SST-data spanning the region and period 
               of the tracks that needs to be analyzed.}
  \item{folder}{Is where the downloaded raw data files are stored. 
               This defaults to a temporary directory.}
  \item{server}{the url of the server}
  \item{nday}{Time resolution should be either '5day' or '8day'}
}
\details{
}
\value{
  The path returned from the function is where all the raw SST
  files are saved. 
}
\references{TALK TO DAVE FOLEY}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu},
        Chi Hin Lam \email{chihinl@usc.edu},   
        Dave Foley \email{Dave.Foley@noaa.gov}}

\seealso{\code{\link{blue.shark}}, \code{\link{get.sst.from.server}}, \code{\link{get.avhrr.sst}}}
\examples{
  # No example supplied here, but check out the example 
  # in the blue.shark dataset documentation
}
\keyword{models}
