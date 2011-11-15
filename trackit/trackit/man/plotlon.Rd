\name{plotlon}
\alias{plotlon}
\title{Longitude plot}
\description{
Nice plot of the longitude component of the most probable track.
}
\usage{
plotlon(fit, exr = NULL, top = FALSE)
}
\arguments{
  \item{fit}{The fitted object as returned from the function \code{\link{trackit}}}
  \item{exr}{A vector of points. The y-range of the plot is extended to also span 
             these points (if necessary).}
  \item{top}{Mainly used internally. If \code{TRUE} it is assumed that this plot 
             is placed on top of a similar latitude plot, and the axis are adjusted 
             accordingly.}
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
