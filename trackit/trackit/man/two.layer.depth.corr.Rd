\name{two.layer.depth.corr}
\alias{two.layer.depth.corr}
\title{Two layer depth correction}
\description{Two layer depth correction done by estimating two extinction coefficients, As suggested be Phil Ekstrom}
\usage{
two.layer.depth.corr(track, daybyday=FALSE, D0=50)
}
\arguments{
  \item{track}{A data.frame with a light column and corresponding depth, day month and year columns}
  \item{daybyday}{If TRUE the this is done separately for each days data with separate coefficients for each day}
  \item{D0}{The depth that separates the two layers}
}
\value{
  A data.frame similar to the input, but with the light column corrected.  
}
\references{\url{http://www.lotek.com/irradiance.pdf} (2002 paper by Phil Ekstrom)}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu} and John Sibert \email{sibert@hawaii.edu}}

\keyword{models}
