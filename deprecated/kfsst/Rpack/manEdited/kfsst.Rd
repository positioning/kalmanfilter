\name{kfsst}
\alias{kfsst}

\title{Kalman Filter based optimization of the tracking model including Sea Surface Temperature}
\description{
  After the track has been read in and the SST-field has been constructed 
  (see the \code{road.map} function for advice) this function does the 
  actual optimization of the model and reconstruction of the track. 

  Basically this function only needs the raw track and the SST file, but it 
  has a lot of options to modify the model, which are presented here. Quite 
  often it is necessary to simplify the model to get a converging fit - 
  especially for short tracks.    
}
\usage{
kfsst(data, sst.file = "sst.dat", fix.first = TRUE, fix.last = TRUE, 
      theta.active = c(u.active, v.active, D.active, bx.active, by.active, bsst.active, sx.active, sy.active, ssst.active, a0.active, b0.active), 
      theta.init = c(u.init, v.init, D.init, bx.init, by.init, bsst.init, sx.init, sy.init, ssst.init, a0.init, b0.init), 
      u.active = TRUE, v.active = TRUE, D.active = TRUE, bx.active = TRUE, by.active = TRUE, bsst.active = TRUE, 
      sx.active = TRUE, sy.active = TRUE, ssst.active = TRUE, a0.active = TRUE, b0.active = TRUE, 
      u.init = 0, v.init = 0, D.init = 100, bx.init = 0, by.init = 0, bsst.init = 0, 
      sx.init = 0.1, sy.init = 1, ssst.init = 0.1, a0.init = 0.001, b0.init = 0, 
      var.struct = "solstice", dev.pen = 0, save.dir = NULL, admb.string = "")
}

\arguments{
  \item{data}{A data.frame consisting of six columns. The first three columns should contain 
              \code{day}, \code{month} and \code{year} corresponding to valid dates. The dates 
             must be sorted in ascending order. Column four and five should contain the 
             \code{longitude} and \code{latitude} in degrees. The final column should contain
             the SST measurement derived from the tag on the fish.}
  \item{sst.file}{A string containing the filename (including path) to a file returned from 
                  the function \code{\link{write.sst.field}}.}
  \item{fix.first}{\code{TRUE} (default) if the first position in the data set is the true 
                   release position (known without error), \code{FALSE} otherwise. 
  }
  \item{fix.last}{\code{TRUE} (default) if the last position in the data set is the true 
                  recapture/popoff position (known without error), \code{FALSE} otherwise.
  }
  \item{theta.active}{A logical vector of eleven elements, each corresponding to a model 
		      parameter. If an element is set to \code{TRUE} the value of corresponding 
                      parameter is optimized, otherwise it is kept at its initial value.
                      The default value is \code{TRUE} for all parameters. The values 
                      \code{1/0} can be used instead of \code{TRUE/FALSE}. The order of the 
                      elements in this vector is \code{c(u.active, v.active, D.active, 
                      bx.active, by.active, bsst.active, sx.active, sy.active, ssst.active, 
                      a0.active, b0.active)}, hence a value of \code{c(0,0,1,1,1,1,1,1,1,1,1)} 
                      would result in a model where \eqn{u} and \eqn{v} were fixed at there 
                      initial values. 
                      }
  \item{theta.init}{A numeric vector of eleven elements, each corresponding to a model 
                    parameter. The order of the elements in this vector is 
                    \code{c(u.init, v.init, D.init, bx.init, by.init, bsst.init, sx.init, 
                    sy.init, ssst.init, a0.init, b0.init)}
                    and the default value is \code{c(0, 0, 100, 0, 0, 0, 0.1, 1.0, 0.1, 
                    0.001, 0)}. It is unwise to initialize elements \code{D.init}, 
                    \code{sx.init}, \code{sy.init}, and \code{ssst.init} below zero, 
                    as they correspond to standard deviations.
  }
  \item{u.active}{\code{TRUE} (default) if \eqn{u} should be optimized, \code{FALSE} if 
                  it should be fixed at its initial value.}
  \item{v.active}{\code{TRUE} (default) if \eqn{v} should be optimized, \code{FALSE} if 
                  it should be fixed at its initial value.}
  \item{D.active}{\code{TRUE} (default) if \eqn{D} should be optimized, \code{FALSE} if 
                  it should be fixed at its initial value.}
  \item{bx.active}{\code{TRUE} (default) if \eqn{b_x}{b[x]} should be optimized, \code{FALSE} 
                  if it should be fixed at its initial value.}
  \item{by.active}{\code{TRUE} (default) if \eqn{b_y}{b[y]} should be optimized, \code{FALSE} 
                   if it should be fixed at its initial value.}
  \item{bsst.active}{\code{TRUE} (default) if \eqn{b_{sst}}{b[sst]} should be optimized, 
                   \code{FALSE} if it should be fixed at its initial value.}
  \item{sx.active}{\code{TRUE} (default) if \eqn{\sigma_x}{sigma[x]} should be optimized, 
                   \code{FALSE} if it should be fixed at its initial value.}
  \item{sy.active}{\code{TRUE} (default) if \eqn{\sigma_y}{sigma[y]} should be optimized, 
                   \code{FALSE} if it should be fixed at its initial value.}
  \item{ssst.active}{\code{TRUE} (default) if \eqn{\sigma_{sst}}{sigma[sst]} should be 
                   optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{a0.active}{If the variance structure \code{var.struct="solstice"} is chosen this 
                   flag should be set to \code{TRUE} (default) if \eqn{a_0}{a[0]} should be 
                   optimized, \code{FALSE} if it should be fixed at its initial value. If a 
                   different variance structure is selected this flag is ignored.}
  \item{b0.active}{If the variance structure \code{var.struct="solstice"} is chosen this 
                   flag should be set to \code{TRUE} (default) if \eqn{b_0}{b[0]} should be 
                   optimized, \code{FALSE} if it should be fixed at its initial value. 
                   If a different variance structure is selected this flag is ignored.}
  \item{u.init}{The initial value of \eqn{u}. Default is 0.}
  \item{v.init}{The initial value of \eqn{v}. Default is 0.}
  \item{D.init}{The initial value of \eqn{D}. Default is  100.}
  \item{bx.init}{The initial value of \eqn{b_x}{b[x]}. Default is 0.}
  \item{by.init}{The initial value of \eqn{b_y}{b[y]}. Default is 0.}
  \item{bsst.init}{The initial value of \eqn{b_{sst}}{b[sst]}. Default is 0.}
  \item{sx.init}{The initial value of \eqn{\sigma_x}{sigma[x]}. Default is 0.1.}
  \item{sy.init}{The initial value of \eqn{\sigma_y}{sigma[y]}. Default is 1.0.}
  \item{ssst.init}{The initial value of \eqn{\sigma_{sst}}{sigma[sst]}. Default is 0.1.}
  \item{a0.init}{If the variance structure \code{var.struct="solstice"} is chosen this sets 
                 the initial value of \eqn{a_0}{a[0]}. Default is 0.001. If a different 
                 variance structure is selected this is ignored.}
  \item{b0.init}{If the variance structure \code{var.struct="solstice"} is chosen this sets 
                 the initial value of \eqn{b_0}{b[0]}. Default is 0. If a different variance 
                 structure is selected this is ignored.
                }
  \item{var.struct}{Three options are available: \code{"uniform"}, \code{"solstice"}(default), 
                    and \code{"daily"}.}
  \item{dev.pen}{If \code{var.struct="daily"} is set, this parameter sets the derivative 
                 penalty.}
  \item{save.dir}{\code{NULL} (default) if the estimation should be done in a temporary 
                  directory, otherwise the quoted name of the directory where the estimation 
                  should be saved.}
  \item{admb.string}{Additional command line arguments to the underlying AD Model Builder 
                     program can be passed as a string. For instance "-est". The available 
                     command line arguments can be found in the AD Model Builder documentation 
                     (see \url{http://otter-rsch.com})}
}
\details{
  Here should the model briefly be described, but for now read all about it in the 
  reference given below. 
}

\value{
  An object of class \code{kftrack} is returned. This object contains 
  information about the fit and estimated tracks.   
}

\references{\url{http://www.tracking.nielsensweb.org}}

\author{Anders Nielsen \email{anielsen@dina.kvl.dk}, John Sibert \email{sibert@hawaii.edu}}


 

\seealso{\code{\link{road.map}}, \code{link{blue.shark}}}

\examples{
  # No example supplied here, but check out the example 
  # in the blue.shark dataset documentation
}

\keyword{models}

