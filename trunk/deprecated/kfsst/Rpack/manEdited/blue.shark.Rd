\name{blue.shark}
\alias{blue.shark}
\docType{data}
\title{A track of a blue shark released near Hawai'i}
\description{
  This blue shark was released from longline fishing gear in 
  April 2001. The PSAT tag from Microwave Telemetry (PTT-100) 
  was attached and reported back after around 100 days at 
  liberty.  
}
\usage{data(blue.shark)}
\format{
  A data frame with 45 observations on the following 6 variables.
  \describe{
    \item{\code{day}}{Integer giving the day of month}
    \item{\code{month}}{Integer giving the month number}
    \item{\code{year}}{Four digit integer giving the year}
    \item{\code{Long}}{Raw light-based longitude in degrees east}
    \item{\code{Lat}}{Raw light-based latitude in degrees north}
    \item{\code{sst}}{Sea surface temperature (SST) derived from 
                      the tag in degrees Celsius.}
  }
}

\examples{
  data(blue.shark)
  sst.path<-get.sst.from.server(blue.shark)
  sst.file<-write.sst.field(sst.path)
  fit<-kfsst(blue.shark, sst.file, bx.active=FALSE, bsst.active=FALSE)
  fit
  plot(fit, ci=TRUE, pred=FALSE)
}
\keyword{datasets}
