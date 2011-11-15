\name{write.html}
\alias{write.html}

\title{Write html page with a map of the track(s)}

\description{
Write html page with a map of the track(s). This html code Google maps 
interface to display the estimated track(s) on a pretty map.  
}
\usage{
write.html(fitlist, description = rep("", length(fitlist)), file = "track.html", npoints = 20, level = 0.95, key = "none")
}

\arguments{
  \item{fitlist}{One fit or a list of fits as returned from \code{kftrack} or \code{kfsst}}
  \item{description}{One string (in case of only one fit), or a vector of strings, each 
                     with a description of a tag. Html tags are allowed}
  \item{file}{The file where the html code is written}
  \item{npoints}{Number of points in the polygon representation of the confidence ellipses}
  \item{level}{The confidence level}
  \item{key}{The Google Maps API key corresponding to the site where the page is to be 
             published. If no key is supplied the page will still work, but should only 
             be used locally}
}

\references{Google maps API \url{http://www.google.com/apis/maps/}}
\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen \email{anielsen@dina.kvl.dk}}
\seealso{\code{\link{addmap}} and \code{\link{kftrack}}}

\examples{
  data(big.241)
  fit<-kftrack(big.241, fix.last=FALSE)
  write.html(fit)
  #browseURL(normalizePath('track.html'))
}
\keyword{programming}
