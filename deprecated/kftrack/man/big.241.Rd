\name{big.241}
\alias{big.241}
\title{Data for bigeye tuna tag number 241}
\usage{data(big.241)}
\description{
The \code{big.241} data frame has 76 rows and 5 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{day}{a numeric vector containing integer values 
               corresponding to the day part of the observation dates}
    \item{month}{a numeric vector containing integer values 
                 corresponding to the month part of the observation dates }
    \item{year}{a numeric vector containing four digit integer values 
                corresponding to the year part of the observation dates }
    \item{long}{a numeric vector containing the longitude measurements}
    \item{lati}{a numeric vector containing the latitude measurements}
  }
}
\note{The dates should be in increasing order.}

\references{Sibert, J., Musyl, M. K. and Brill, R.W. (2002) Horizontal movements 
            of bigeye tuna near Hawaii determined by Kalman filter analysis of 
            archival tagging data. Fish. Oceanogr. In press(?):??--??.}
\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen \email{anielsen@dina.kvl.dk}}

\seealso{\code{\link{kftrack}}}

\examples{
data(big.241)
big.241[1:10,]
#fit<-kftrack(big.241, fix.last=FALSE)
#plot(fit)
}

\keyword{datasets}
