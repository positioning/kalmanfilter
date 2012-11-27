\name{plot.kfsst}
\alias{plot.kfsst}

\title{Plot a fit from kfsst.}
\description{
  Plots four figures based on the reconstructed track. One plot of the 
  reconstructed track (possibly on top of a map, but it requires GMT 
  to be installed also see: \url{http://gmt.soest.hawaii.edu/}. Three 
  additional plots illustrating how well each of the observed 
  coordinates matches the fitted track.)
}
\usage{
plot.kfsst(x, ci = FALSE, points = TRUE, pred = TRUE, most = TRUE, gmt=FALSE, ...)
}

\details{
  Besides the arguments described above there is one other option if GMT 
  is installed on the system. If \code{map=TRUE} is specified the drawing 
  of the track is overlaid on a simple land/water map.   
}


\arguments{
  \item{x}{is a \code{kfsst} object typically generated
           with the \code{\link{kfsst}} function}
  \item{ci}{If TRUE adds confidence regions for the most probable track to the plot}
  \item{points}{If FALSE the raw geo-locations are omitted}
  \item{pred}{If FALSE the predicted plot is omitted}
  \item{most}{If FALSE the most probable track is omitted}
  \item{gmt}{If TRUE (and GMT is correctly installed) a GMT-based postscript
file with the track saved in the working directory}
  \item{\dots}{additional graphics parameters}
}
\value{
  No value is returned. This function is invoked for its side effects.
}
\author{Anders Nielsen \email{anders.nielsen@hawaii.edu}, 
        John Sibert \email{sibert@hawaii.edu}}

\seealso{\code{\link{kfsst}}, \code{\link{blue.shark}}}
\examples{
  # No example supplied here, but check out the example 
  # in the blue.shark dataset documentation
}
\keyword{models}
