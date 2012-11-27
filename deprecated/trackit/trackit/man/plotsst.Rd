\name{plotsst}
\alias{plotsst}
\title{SST plot}
\description{
Nice plot of the SST component of the most probable track.
}
\usage{
plotsst(fit, exr = NULL)
}
\arguments{
  \item{fit}{The fitted object as returned from the function \code{\link{trackit}}}
  \item{exr}{A vector of points. The y-range of the plot is extended to also span 
             these points (if necessary).}
}
\value{
  No value is returned. This function is invoked for its side effects.
}
\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (in press).}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu} and John Sibert \email{sibert@hawaii.edu}}

\seealso{\code{\link{trackit}}}
\examples{
  # No examples provided here, but try the ones in ?drifter and ?mooring
}

\keyword{models}
