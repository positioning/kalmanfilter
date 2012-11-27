\name{fitmap}
\alias{fitmap}
\title{Plot the most probable track on a map}
\description{
  Plot the most probable track and possibly its confidence region on a map
}

\usage{
fitmap(x, ci=FALSE, ...)
}
\arguments{
  \item{x}{An object returned from the \code{\link{trackit}} function}
  \item{ci}{If TRUE the 95\% confidence region is added to the plot}
  \item{...}{additionel arguments to \code{\link{plotbasemap}}}
}
\value{
  Value is \code{NULL} 
}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu}}

\keyword{models}
