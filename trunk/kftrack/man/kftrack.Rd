\name{kftrack}
\alias{kftrack}
\title{Kalman filter tracking (of tagged individuals)}
\description{
  Fit a state space model to observed geo--locations via the extended Kalman filter. 
  A few variations of the model is available.   
}
\usage{
kftrack(data, fix.first=TRUE, fix.last=TRUE, 
  theta.active=c(u.active, v.active, D.active, bx.active, by.active, 
                 sx.active, sy.active, a0.active, b0.active, vscale.active), 
  theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, 
               a0.init, b0.init, vscale.init), 
  u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, 
  by.active=TRUE, sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, 
  b0.active=TRUE, vscale.active=TRUE, u.init=0, v.init=0, D.init=100, 
  bx.init=0, by.init=0, sx.init=.5, sy.init=1.5, a0.init=0.001, b0.init=0, 
  vscale.init=1, var.struct="solstice", dev.pen=0.0, save.dir=NULL, admb.string="")
}

\arguments{
  \item{data}{A data.frame consisting of five columns. The first three columns should contain 
              \code{day}, \code{month} and \code{year} corresponding to valid dates. The dates 
             must be sorted in ascending order. Column four and five should contain the 
             \code{longitude} and \code{latitude} in degrees. A valid data set example is 
             supplied as part of the package (see \code{\link{big.241}).}
  }
  \item{fix.first}{\code{TRUE} (default) if the first position in the data set is the true 
                   release position (known without error), \code{FALSE} otherwise. 
  }
  \item{fix.last}{\code{TRUE} (default) if the last position in the data set is the true 
                  recapture/popoff position (known without error), \code{FALSE} otherwise.
  }
  \item{theta.active}{A logical vector with nine elements, each corresponding to a model 
		      parameter. If an element is set to \code{TRUE} the value of corresponding 
                      parameter is optimized, otherwise it is kept at its initial value.
                      The default value is \code{TRUE} for all parameters. The values \code{1/0} can 
                      be used instead of \code{TRUE/FALSE}. The order of the elements in this vector is 
                      \code{c(u.active, v.active, D.active, bx.active, by.active, sx.active, sy.active, 
                      a0.active, b0.active)}, hence a value of \code{c(0,0,1,1,1,1,1,1,1)} would result 
                      in a model where \eqn{u} and \eqn{v} were fixed at there initial values. 
                      }
  \item{theta.init}{A numeric vector with nine elements, each corresponding to a model parameter.
                    The order of the elements in this vector is 
                    \code{c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, b0.init)}
                    and the default value is \code{c(0, 0, 100, 0, 0, 0.5, 1.5, 0.001, 0)}. It is unwise to 
                    initialize elements \code{D.init}, \code{sx.init} and \code{sy.init} below zero, as they 
                    correspond to standard deviations.
  }
  \item{u.active}{\code{TRUE} (default) if \eqn{u} should be optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{v.active}{\code{TRUE} (default) if \eqn{v} should be optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{D.active}{\code{TRUE} (default) if \eqn{D} should be optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{bx.active}{\code{TRUE} (default) if \eqn{b_x}{b[x]} should be optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{by.active}{\code{TRUE} (default) if \eqn{b_y}{b[y]} should be optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{sx.active}{\code{TRUE} (default) if \eqn{\sigma_x}{sigma[x]} should be optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{sy.active}{\code{TRUE} (default) if \eqn{\sigma_y}{sigma[y]} should be optimized, \code{FALSE} if it should be fixed at its initial value.}
  \item{a0.active}{If the variance structure \code{var.struct="solstice"} is chosen this flag should be set to \code{TRUE} (default) if 
                   \eqn{a_0}{a[0]} should be optimized, \code{FALSE} if it should be fixed at its initial value. If a different variance 
                   structure is selected this flag is ignored.}
  \item{b0.active}{If the variance structure \code{var.struct="solstice"} is chosen this flag should be set to \code{TRUE} (default) if 
                   \eqn{b_0}{b[0]} should be optimized, \code{FALSE} if it should be fixed at its initial value. If a different variance 
                   structure is selected this flag is ignored.}
  \item{vscale.active}{If the variance structure \code{var.struct="specified"} and this is \code{TRUE} a common scaling parameter is estimated for the specified covariance matrices}
  \item{u.init}{The initial value of \eqn{u}. Default is 0.}
  \item{v.init}{The initial value of \eqn{v}. Default is 0.}
  \item{D.init}{The initial value of \eqn{D}. Default is  100.}
  \item{bx.init}{The initial value of \eqn{b_x}{b[x]}. Default is 0.}
  \item{by.init}{The initial value of \eqn{b_y}{b[y]}. Default is 0.}
  \item{sx.init}{The initial value of \eqn{\sigma_x}{sigma[x]}. Default is 0.5.}
  \item{sy.init}{The initial value of \eqn{\sigma_y}{sigma[y]}. Default is 1.5.}
  \item{a0.init}{If the variance structure \code{var.struct="solstice"} is chosen this sets the initial value of \eqn{a_0}{a[0]}. 
                 Default is 0.001. If a different variance structure is selected this is ignored.}
  \item{b0.init}{If the variance structure \code{var.struct="solstice"} is chosen this sets the initial value of \eqn{b_0}{b[0]}. 
                 Default is 0. If a different variance structure is selected this is ignored.}
  \item{vscale.init}{Initial value for the common scaling parameter for the specified covariance matrices}
  \item{var.struct}{Four options are available: \code{"uniform"}, \code{"specified"}, \code{"solstice"}(default) and \code{"daily"}. These are defined in the details section.}
  \item{dev.pen}{If \code{var.struct="daily"} is set, this parameter sets the derivative penalty.}
  \item{save.dir}{\code{NULL} (default) if the estimation should be done in a temporary directory, otherwise the quoted name of the 
                 directory where the estimation should be saved.}
  \item{admb.string}{Additional command line arguments to the underlying AD Model Builder program can be passed as a string. For 
                     instance "-est". The available command line arguments can be found in the AD Model Builder documentation 
                     (see \url{http://otter-rsch.com})}
}

\details{
  
  The model is a state space model, where the state equation is 
  describing the movements of a fish in an axis--parallel plane. 
  A random walk model is assumed:
  \deqn{
    \alpha_i = \alpha_{i-1}+c_i+\eta_i, \quad i=1,\ldots,T
  }{
    alpha[i] = alpha[i-1]+c[i]+eta[i], \ \ i=1,...,T
  }
  Here \eqn{\alpha_i}{alpha[i]} is a two dimensional vector containing the coordinates 
  at time \eqn{t_i}{t[i]}, \eqn{c_i}{c[i]} is the drift vector describing the deterministic part 
  of the movement, and \eqn{\eta_i}{eta[i]} is the noise vector a describing the random 
  part of the movement. The deterministic part of the movement is assumed to 
  be proportional to time:
  \deqn{
    c_i=(u \Delta t_i , v \Delta t_i)'
  }{
    c[i] = ( u(t[i]-t[i-1]), v(t[i]-t[i-1]) )'
  }
  The random part is assumed to be serially uncorrelated and follow a two 
  dimensional Gaussian distribution with mean vector 0 and covariance matrix 
  \eqn{Q_i}{Q[i]}, where 
  \deqn{
    Q_i = 2D \Delta t_i I, \quad \mbox{(here `I' is the $2\times2$ unit matrix).}
  }{
    Q[i] = 2D(t[i]-t[i-1])I,\ \ (here `I' is the 2x2 unit matrix)
  }
  The measurement equation of the state space model is a non--linear 
  mapping of the coordinates on the axis--parallel plane on to the sphere. 
  The original coordinates were in Nautical miles and the coordinates on 
  the sphere are in degrees of longitude and latitude. The 
  measurement equation describing the actual position \eqn{y_i}{y[i]} is:
  \deqn{
    y_i=z(\alpha_i)+d_i+\varepsilon_i, \quad i=1,\ldots,T
  }{
    y[i]=z(alpha[i])+d[i]+epsilon[i], \ \  i=1,...,T
  }
  where \eqn{z} is the coordinate change function given by:
  \deqn{
    z(\alpha_i) = \left( 
                        \frac{\alpha_{i,1}}{60\cos(\alpha_{i,2}\pi/180/60)}\  ,\ 
                        \frac{\alpha_{i,2}}{60}  \right)'
  }{
    z(alpha[i]) = ( alpha[i,1]/(60cos(alpha[i,2]PI/180/60))\  ,\ alpha[i,2]/60 )'
  }
  \eqn{d_i}{d[i]} is the observed bias:
  \deqn{
    d_i=(b_x\ , \ b_y)'
  }{
    d[i]=( b[x] , b[y] )' 
  }     
  and \eqn{\varepsilon_i}{epsilon[i]} is the measurement error which is assumed to 
  follow a Gaussian distribution with mean vector 0 and covariance 
  matrix \eqn{H_i}{H[i]}, where
  \deqn{
    H_i=\mbox{diag}(\sigma_x^2, \sigma_{y_i}^2) 
  }{
    H[i]=diag(sigma[x]^2, sigma[y[i]]^2) 
  } 

  The arguments \code{u.active},  \code{v.active}, \code{D.active}, 
  \code{bx.active}, \code{by.active}, \code{sx.active} and \code{sy.active} 
  offers an alternative way of specifying the argument vector 
  \code{theta.active}. This is useful for two reasons. Firstly, if only a 
  few of the elements of \code{theta.active} is changed from their defaults, 
  it is convenient not having to specify the entire vector. Secondly, it is 
  not required to remember the correct order of the arguments in 
  \code{theta.active}, if they are specified individually. 
  If \code{theta.active} is specified any individually specified arguments 
  are ignored and the values of \code{theta.active} is used.

  Similarly the arguments \code{u.init}, \code{v.init}, \code{D.init}, 
  \code{bx.init}, \code{by.init}, \code{sx.init} and \code{sy.init} offers 
  an alternative way of specifying the argument vector \code{theta.init}. 

  The argument \code{var.struct} sets the model for the latitude error 
  \eqn{\sigma_{y_i}^2}{sigma[y[i]]^2}. Three options are 
  available. 

  If \code{"uniform"} the same variance is assumed for all 
  observations.
  
  If \code{"solstice"} the variance is assumed to follow the model:
  \deqn{
    \sigma_{y_i}^2 =
    \sigma^2_{y_0}\Big/\Big(\cos^2\big(2\pi(J_i+(-1)^{s_i}b_0)/365.25\big)+a_0\Big)
  }{
    sigma[y[i]]^2 =
    sigma[y[0]]^2 / (cos^2(2pi(J[i]+(-1)^(s[i])b[0])/365.25)+a[0])
  }
  where \eqn{J_i}{J[i]} is the number of days since last solstice prior to all
  observations, \eqn{s_i}{s[i]} is the season number since the beginning of
  the track (one for the first 182.625 days, then two for the next
  182.625, then three and so on). \eqn{a_0}{a[0]}, \eqn{b_0}{b[0]} and 
  \eqn{\sigma^2_{y_0}}{sigma[y[0]]^2} are model parameters.

  If \code{"daily"} the variance is assumed to have a different value at 
  each time step, and \eqn{\psi_i}{psi[i]} are normally distributed random 
  variables with mean zero and variance \eqn{\sigma_{\psi}^2}{sigma[psi]^2} 
  representing transient deviations in the latitude error.
}
\value{
  An object of class \code{kftrack} is returned. This object contains 
  information about the fit and estimated tracks.   
}
\references{Sibert, J., Musyl, M. K. and Brill, R.W. (2002) Horizontal movements 
            of bigeye tuna near Hawaii determined by Kalman filter analysis of 
            archival tagging data. Fish. Oceanogr. In press(?):??--??.}
\author{John Sibert \email{jsibert@soest.hawaii.edu}, Anders Nielsen \email{anielsen@dina.kvl.dk}}

\seealso{\code{\link{plot.kftrack}}}

\examples{
data(big.241)
fit<-kftrack(big.241, fix.last=FALSE)
plot(fit)
}
\keyword{programming}
