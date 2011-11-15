\name{get.sst.from.server}
\alias{get.sst.from.server}

\title{Get SST-field from server}
\description{
  This function allows easy access to three different SST sources  
  that has been setup for this purpose. Data is downloaded from 
  within R and stored in a format ready to be used.  
}
\usage{
get.sst.from.server(track, folder = tempdir(), 
       server = "http://atlas.nmfs.hawaii.edu/cgi-bin/reynolds_extract.py")
}
\arguments{
  \item{track}{A single \code{data.frame} containing a track or a 
               \code{list} of \code{data.frame}s each containing a 
               track. The idea is that the function should only 
               download the SST-data spanning the region and period 
               of the tracks that needs to be analyzed.}
  \item{folder}{Is where the downloaded raw data files are stored. 
               This defaults to a temporary directory.}
  \item{server}{Presently three servers are available. The default is 
		the fairly coarse Reynold's one 1x1-degree 8-day 
		composites. This source is fast to download, but may 
		be too coarse in some areas. The two other servers are 
		the AVHRR-GAC 3-day and AVHRR-GAC 8-day composites 
		these have a 0.1x0.1-degree resolution. The server names 
		are: 

		\code{http://atlas.nmfs.hawaii.edu/cgi-bin/gac3day_extract.py}

		and

		\code{http://atlas.nmfs.hawaii.edu/cgi-bin/gac8day_extract.py}}
}
\details{
  The servers has been set up to extract SST-fields that
  covers a track (or a set of tracks) in simple way from
  within R. To use the default source type a command similar to:

     sst.path <- get.sst.from.server(track1)

  Notice 'track1' can be replaced by a list of tracks like:

     sst.path <- get.sst.from.server(list(track1, track2))

  to obtain an SST-field covering a set of tracks.
  
  To use one of the two other servers simply supply the server 
  name as in: 

     \code{sst.path <- get.sst.from.server(list(track1, track2), 
  server='http://atlas.nmfs.hawaii.edu/cgi-bin/gac3day_extract.py')}

  or 

     \code{sst.path <- get.sst.from.server(list(track1, track2), 
  server='http://atlas.nmfs.hawaii.edu/cgi-bin/gac8day_extract.py')} 
  

  To use a user supplied SST-source please see documentation 
  for the function ....

}
\value{
  The path returned from the function is where all the raw SST
  files are saved. 
}
\references{TALK TO RUSS}

\author{Anders Nielsen \email{anielsen@dina.kvl.dk}, 
        Russell Moffitt \email{Russell.Moffitt@noaa.gov}}

\seealso{\code{\link{blue.shark}}}
\examples{
  # No example supplied here, but check out the example 
  # in the blue.shark dataset documentation
}
\keyword{models}
