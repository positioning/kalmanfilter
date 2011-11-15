\name{plot.trackit}
\alias{plot.trackit}
\title{Plot a fitted track}
\description{Plots a nice representation of the fitted most probable 
track.   
}
\usage{
plot.trackit(x, onlylonlat=FALSE, ...)
}
\arguments{
  \item{x}{The fitted object as returned from the function \code{\link{trackit}}}
  \item{onlylonlat}{For fitted objects with sst this option turns off the SST 
                    panel in the plot}
  \item{\dots}{additional graphical parameters (currently not used)}
}
\value{
  No value is returned. This function is invoked for its side effects.
}
\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (submitted).}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu} and John Sibert \email{sibert@hawaii.edu}}

\seealso{\code{\link{trackit}}}
\examples{
  # No examples provided here, but try the ones in ?drifter and ?mooring
}

\keyword{models}
