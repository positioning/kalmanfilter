\name{trackit}
\alias{trackit}
\title{trackit: Light based tracking}
\description{
This function fits a state space model developed to estimate the most 
probable track of archival tagged marine animals. The underlying movement 
model is a random walk with drift, and the observations are the light 
measurements surrounding each solar event.  
}
\usage{
trackit(prep.track, a2lpoints=15,
        u.init=0, v.init=0, D.init=100, 
        ss1.init=1, ss2.init=5, ss3.init=1, rho.init=0.01, 
        bsst.init=0, sssst.init=0.01, rad.init=200, dep1.init=0, dep2.init=0, 
        phi.init=c(60,rep((200-60)/(a2lpoints-1),a2lpoints-1)),    
        init=c(u.init,v.init,D.init,ss1.init,ss2.init,ss3.init,rho.init,
               bsst.init,sssst.init,rad.init,dep1.init,dep2.init,phi.init), 
        u.ph=-1, v.ph=-1, D.ph=3, ss1.ph=2, ss2.ph=2, ss3.ph=2, rho.ph=3, 
        bsst.ph=-1, sssst.ph=2, rad.ph=3, dep1.ph=-1, dep2.ph=-1,phi.ph=1,  
        phase=c(u.ph,v.ph,D.ph,ss1.ph,ss2.ph,ss3.ph,rho.ph,
                bsst.ph,sssst.ph,rad.ph,dep1.ph,dep2.ph,phi.ph),
        blue.light.only=FALSE, save.dir=NULL)
}

\arguments{
  \item{prep.track}{A prepared track, which is the object returned from 
        the function \code{\link{prepit}}}
  \item{a2lpoints}{The number of support points to be used in the cubic 
                   spline approximation of the relationship between solar 
                   altitude angle and the light reading in the tag.}
  \item{u.init}{Set initial value for model parameter \eqn{u}}
  \item{v.init}{Set initial value for model parameter \eqn{v}}
  \item{D.init}{Set initial value for model parameter \eqn{D}}
  \item{ss1.init}{Set initial value for model parameter \eqn{\sigma_1^2}{sigma[1]^2}}
  \item{ss2.init}{Set initial value for model parameter \eqn{\sigma_2^2}{sigma[2]^2}}
  \item{ss3.init}{Set initial value for model parameter \eqn{\sigma_3^2}{sigma[3]^2}}
  \item{rho.init}{Set initial value for model parameter \eqn{\rho}}
  \item{bsst.init}{Set initial value for model parameter \eqn{b_{sst}}{b[sst]}}
  \item{sssst.init}{Set initial value for model parameter \eqn{\sigma^2_{sst}}{sigma[sst]^2}}
  \item{rad.init}{Set initial value for model parameter \eqn{r} (the radius)}
  \item{dep1.init}{Set initial value for model parameter \eqn{d_1}{d1}}
  \item{dep2.init}{Set initial value for model parameter \eqn{d_2}{d2}}
  \item{phi.init}{Set initial value for model parameter \eqn{\phi} (a vector of a2lpoints elements)}
  \item{init}{A vector of initial values for all model parameters. Sticking 
              to the model description in the reference the order of the 
              elements are: u, v, D, \eqn{\sigma_1^2}{sigma[1]^2}, 
              \eqn{\sigma_2^2}{sigma[2]^2}, \eqn{\sigma_3^2}{sigma[3]^2},  
              \eqn{\rho}, \eqn{b_{sst}}{b[sst]}, 
              \eqn{\sigma^2_{sst}}{sigma[sst]^2}, \eqn{r}, \eqn{d_1}{d1}, 
              \eqn{d_2}{d2}, \eqn{\phi_1}{phi[1]}, ..., \eqn{\phi_n}{phi[n]}.              
             }
  \item{u.ph}{Set estimation phase for model parameter \eqn{u}. The options 
              are: '-1' (kept fixed at initial value), '1' (first phase 
              reserved for \eqn{phi}) , '2' (second phase), or '3' (third phase).
              If estimation of \eqn{u} is turned on it is recommended to use phase '3'.}
  \item{v.ph}{Set estimation phase for model parameter \eqn{v}. If estimation of \eqn{v} 
              is turned on it is recommended to use phase '3'.}
  \item{D.ph}{Set estimation phase for model parameter \eqn{D}}
  \item{ss1.ph}{Set estimation phase for model parameter \eqn{\sigma_1^2}{sigma[1]^2}}
  \item{ss2.ph}{Set estimation phase for model parameter \eqn{\sigma_2^2}{sigma[2]^2}}
  \item{ss3.ph}{Set estimation phase for model parameter \eqn{\sigma_3^2}{sigma[3]^2}}
  \item{rho.ph}{Set estimation phase for model parameter \eqn{\rho}}
  \item{bsst.ph}{Set estimation phase for model parameter \eqn{b_{sst}}{b[sst]}. If 
                 estimation of \eqn{b_{sst}}{b[sst]} is turned on it is recommended to 
                 use phase '2'.}
  \item{sssst.ph}{Set estimation phase for model parameter \eqn{\sigma^2_{sst}}{sigma[sst]^2}}
  \item{rad.ph}{Set estimation phase for model parameter \eqn{r} (the radius)}
  \item{dep1.ph}{Set estimation phase for model parameter \eqn{d_1}{d1}}
  \item{dep2.ph}{Set estimation phase for model parameter \eqn{d_2}{d2}}
  \item{phi.ph}{Set one common estimation phase for model parameters in \eqn{\phi}}
  \item{phase}{A vector to set the phase in which each parameter is to be 
               optimized. It is only recommended to tinker with these it 
               you know what you are doing. There are three phases 1, 2, 
               and 3. If it is desired to keep a parameter fixed at its 
               initial value the phase should be set to -1. The order is 
               the same as for the initial values, except that the phase 
               of all the \eqn{\phi}{phi} parameters are set by one 
               element (the last one).   
              }
  \item{blue.light.only}{A logic. If TRUE only solar angles between -3 and 
               5 are used. This option is under development and should not 
               be used yet. Most often there is no convergence and if there 
               is it should not be trusted.}
  \item{save.dir}{Optionally a string specifying a permanent directory name}
}
\details{
See reference. 
}
\value{
  A list containing all relevant information about the model fit 
  including the estimated most probable track. 
}

\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (in press).}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu} and 
        John Sibert \email{sibert@hawaii.edu}}

\seealso{\code{\link{prepit}}}
\examples{
  # No examples provided here, but try the ones in ?drifter and ?mooring
}

\keyword{models}

