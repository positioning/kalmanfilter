\name{get.avhrr.sst}
\alias{get.avhrr.sst}

\title{Get SST-field from avhrr source}
\description{
  This function allows easy access to a avhrr SST database. 
}
\usage{
get.avhrr.sst(track, lonlow, lonhigh, latlow, lathigh, folder = tempdir(), 
              server = "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=",
              product = "TAGssta", nday = "8day", centertime="12") 
}
\arguments{
  \item{track}{A single \code{data.frame} containing a track or a 
               \code{list} of \code{data.frame}s each containing a 
               track. The idea is that the function should only 
               download the SST-data spanning the region and period 
               of the tracks that needs to be analyzed.}
  \item{lonlow}{The lowest longitude where SST is downloaded}
  \item{lonhigh}{The highest longitude where SST is downloaded}
  \item{latlow}{The lowest latitude where SST is downloaded}
  \item{lathigh}{The highest latitude where SST is downloaded}
  \item{folder}{Is where the downloaded raw data files are stored. 
                This defaults to a temporary directory.}
  \item{server}{The url of the server}
  \item{product}{The 7-character name of the imagery product.
                 Refer to the Coastwatch site for the complete list of relevant SST products.
                 \url{http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=}}
  \item{nday}{Time resolution should be either '5day' or '8day'}
  \item{centertime}{The time stamp of the image '00' is from midnight to midnight, and 
                    '12' is from noon to noon.}
}
\details{
}
\value{
  The path returned from the function is where all the raw SST
  files are saved. 
}
\references{TALK TO DAVE FOLEY}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu},
        Chi Lam \email{chihinl@usc.edu},        
        Dave Foley \email{Dave.Foley@noaa.gov}}

\seealso{\code{\link{get.sst.from.server}}, \code{\link{get.avhrr.sst}}}
\examples{
  # No example supplied here
}
\keyword{models}
