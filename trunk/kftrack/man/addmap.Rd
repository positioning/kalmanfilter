\name{addmap}
\alias{addmap}
\title{Adds map to plotted track}
\description{
  Adds blue/green areas to the map with the estimated track, if GMT is 
  installed on the system.   
}
\usage{
addmap(kf.obj, res = 3, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE, ...)
}
\arguments{
  \item{kf.obj}{an object of type \code{kftrack}}
  \item{res}{the resolution}
  \item{ci}{If TRUE adds confidence regions for the most probable track to the plot}
  \item{points}{If FALSE the raw geo-locations are omitted}
  \item{pred}{If FALSE the predicted plot is omitted}
  \item{most}{If FALSE the most probable track is omitted}
  \item{\dots}{additional graphical parameters}
}
\note{GMT must be installed in order to use this function}
\references{\url{http://gmt.soest.hawaii.edu}}
\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen \email{anielsen@dina.kvl.dk}}
\seealso{\code{\link{addcoast}}}
\examples{
  data(big.241)
  fit<-kftrack(big.241, fix.last=FALSE)
  plot(fit)
  #addmap(fit)
}
\keyword{programming}
