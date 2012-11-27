\name{write.kml}
\alias{write.kml}
\title{Write fit to kml file}
\description{
  Writes a copy of the track, the predicted track, the most proable 
  track, and its confidence ellipses to a kml file. These files can 
  be used with Google Earth to see the track.    
}
\usage{
write.kml(fit, description = "", file = "track.kml", npoints = 20, level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{The \code{fit} is either the returned object from a kftrack fit 
             or a kfsst fit }
  \item{description}{A \code{description} of the track can be added here}
  \item{file}{A filename where the output is written}
  \item{npoints}{Number of points in the polygon representation of the 
                 confidence ellipses}
  \item{level}{The confidence level}
}
\references{\url{http://earth.google.com/}}
\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen \email{anielsen@dina.kvl.dk}}
\seealso{\code{\link{addmap}} and \code{\link{kftrack}}}
\keyword{programming}
