\name{addcoast}
\alias{addcoast}
\title{Adds coastline to plotted track}
\description{
  Adds costline to a plotted track, if GMT is installed on the 
  system.  
}
\usage{
addcoast(res = 3, ...)
}

\arguments{
  \item{res}{an integer resolution from 1 (full) to 5 (crude) }
  \item{\dots}{additional graphical parameters}
}
\note{GMT must be installed in order to use this function}
\references{\url{http://gmt.soest.hawaii.edu}}
\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen \email{anielsen@dina.kvl.dk}}
\seealso{\code{\link{addmap}}}
\examples{
  data(big.241)
  fit<-kftrack(big.241, fix.last=FALSE)
  plot(fit)
  #addcoast()
}
\keyword{programming}
