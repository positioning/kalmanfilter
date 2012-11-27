\name{deltat}
\alias{deltat}
\docType{data}
\title{deltat}
\description{
  A table of DeltaT, which is the empirical difference between 
  dynamic time (DT) and universal time UT in seconds.  
}
\usage{data(deltat)}
\format{
  A data frame with 70 observations on the following 5 variables.
  \describe{
    \item{\code{day}}{a numeric vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
    \item{\code{DTminusUT}}{a numeric vector}
    \item{\code{JDE}}{a numeric vector}
  }
}
\details{
  A small internal detail is that if the dataset is loaded as below 
  the column JDE will not show up, but if it is not loaded it is 
  available for internal use. 
}
\source{
  \url{http://www.stjarnhimlen.se/comp/time.html}
}
\examples{
  data(deltat)
  plot(deltat[,3]+deltat[,2]/12, deltat[,4], xlab="year", ylab=expression(Delta*T))
}
\keyword{datasets}
