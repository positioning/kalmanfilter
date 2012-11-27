\name{plot.kftrack}
\alias{plot.kftrack}
\title{Plot a kftrack object}
\description{
  Plots the estimated track.
  }
\usage{
plot.kftrack(x, ci=FALSE, points=TRUE, pred=TRUE, most=TRUE, gmt=FALSE, ...)
}
\arguments{
  \item{x}{is a \code{kftrack} object typically generated
           with the \code{\link{kftrack}} function}
  \item{ci}{If TRUE adds confidence regions for the most probable track to the plot}
  \item{points}{If FALSE the raw geo-locations are omitted}
  \item{pred}{If FALSE the predicted plot is omitted}
  \item{most}{If FALSE the most probable track is omitted}
  \item{gmt}{If TRUE (and if gmt is correctly installed) a a GMT-based
postscript version of the plot will be saved in the working directory}
  \item{\dots}{additional graphics parameters}
}
\value{
  No value is returned this function is invoked for its side effects.
}

\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen \email{anielsen@dina.kvl.dk}}

\seealso{\code{\link{kftrack}}, \code{\link{addmap}}, \code{\link{addcoast}}}

\examples{
data(big.241)
fit<-kftrack(big.241, fix.last=FALSE)
plot(fit)
}
\keyword{programming}
