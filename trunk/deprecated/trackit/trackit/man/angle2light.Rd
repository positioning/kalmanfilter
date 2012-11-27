\name{angle2light}
\alias{angle2light}
\docType{data}
\title{angle2light}
\description{A dataset describing the estimated relationship between 
             solar altitude in degrees (-90)-(90) above the horizon 
             and the transformed light level measured in the tag}
\usage{data(angle2light)}
\format{
  A data.frame containing two columns \code{angle} and \code{light}. 
  There are 1801 observations (rows).  
}
\details{
  This data set, or another one of similar structure is required input 
  for the function \code{\link{light.simulator}}. 

  This particular data set is extracted from the fitted relationship 
  between altitude and light in the \code{\link{mooring}} example.   
}
\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (submitted).}
\examples{
  data(angle2light)
  plot(angle2light, type='l', lwd=3, col='red')
}
\keyword{datasets}
