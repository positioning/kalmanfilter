\name{mooring}
\alias{mooring}
\docType{data}
\title{mooring}
\description{
  Data from a Wildlife Computers archival tag (MK9, version 1.03) 
}
\usage{data(mooring)}
\format{
  A data frame with 323418 observations on the following 9 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{day}}{a numeric vector}
    \item{\code{hour}}{a numeric vector}
    \item{\code{min}}{a numeric vector}
    \item{\code{sec}}{a numeric vector}
    \item{\code{depth}}{a numeric vector}
    \item{\code{light}}{a numeric vector}
    \item{\code{temp}}{a numeric vector}
  }
}
\source{
  Data is provided by Bruno Leroy from the Oceanic Fisheries Programme 
  of the Secretariat of the Pacific Community. 
}
\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (submitted).}
\examples{
  data(mooring)
  prep.track<-prepit(mooring, fix.first=c(166.47,-22.48,2003,9,8,2,32,0), 
                              fix.last=c(166.47,-22.48,2004,4,19,16,49,0))
  # try to run (not in the example, because it takes a while)
  #   fit<-trackit(prep.track)
  #   plot(fit)
}
\keyword{datasets}
