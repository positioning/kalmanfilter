\name{print.kfsst}
\alias{print.kfsst}
\title{Print kfsst object}
\description{
  Prints a pretty summary of an object of class \code{kfsst}
}
\usage{
print.kfsst(x, ...)
}
\details{
Notice also that below the printing of estimates and standard 
deviations are a list of names. Each of these refer to an object 
that is also stored in the fitted object. For instance to get 
the negative log likelihood value type \code{fit$nlogL}, 
and to get the number of parameters type \code{fit$npar}, where 
\code{fit} is the name of the object returned from the function 
\code{\link{kfsst}}.    
}
\arguments{
  \item{x}{an object of class \code{kfsst} typically generated
           with the \code{\link{kfsst}} function.}
  \item{\dots}{additional arguments}
}
\author{Anders Nielsen \email{anders.nielsen@hawaii.edu}, John Sibert \email{sibert@hawaii.edu}}
\seealso{\code{\link{kfsst}}}
\keyword{models}

