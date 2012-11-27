\name{drifter}
\alias{drifter}
\docType{data}
\title{drifter}
\description{
  Data from a Wildlife Computer pop-up archival tag (PAT, version 2)
}
\usage{data(drifter)}
\format{
  A data frame with 4536 observations on the following 9 variables.
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
\details{
  Notice in the example that \code{scan=FALSE}. This is the correct 
  option, because PAT version 2 scans the data and saves only the 
  data around the solar events.  
}
\source{
  Data provided by Mike Musyl. 
}
\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (submitted).}
\examples{
  data(drifter)
  prep.track<-prepit(drifter, fix.first=c(360-161.45,22.85,2002,9,10,0,0,0), 
                              fix.last=c(360-159.87,21.95,2003,5,21,0,0,0), scan=FALSE)
  # try to run (not in the example, because it takes a while)
  #   fit<-trackit(prep.track)
  #   plot(fit)
}
\keyword{datasets}
