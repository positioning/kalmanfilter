\name{upload.track}
\alias{upload.track}

\title{Upload a track to be mapped on the server}
\description{
  Uploads a track to be mapped on the server using Google maps
}
\usage{
upload.track(fit)
}

\arguments{
  \item{fit}{A \code{fit} as returned from \code{kftrack} or \code{kfsst}}
}
\details{
  This function will prepare a data file with the information in the 
  fitted object and some information about the track and on how to 
  contact the person who posts it. The track can then be viewed on 
  a Google map on the server.
}
\value{
  No value is returned, but a file named 'track4upload.RData' is saved 
  in the present working directory. 
}
\references{Google maps API \url{http://www.google.com/apis/maps/}}
\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen
\email{anielsen@dina.kvl.dk}}
\seealso{\code{\link{addmap}},\code{\link{kftrack}},\code{\link{write.html}},\code{\link{write.kml}}}

\keyword{programming}


