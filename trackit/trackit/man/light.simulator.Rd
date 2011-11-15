\name{light.simulator}
\alias{light.simulator}

\title{Simulate artificial datasets}
\description{
This function is mainly used for model validation by the package authors.
For a given set of model parameters it simulate a dataset that follows the 
model. It can be useful for setting up simulation studies.      
}
\usage{
light.simulator(u = 0, v = 0, D = 100, ss1 = 80, ss2 = 15, ss3 = 2.5, rho = 0.05, bsst=0, sssst=0.5, rad=80, sundata = angle2light, sstdates=NULL, fix.first = c(166.5, -22.5, 2003, 1, 1), t = seq(0, 365, by = 1/300), window = c(0.05, 0.01), tmpfile = "input.dat", datfile = "ukf.dat", truefile = "truth.dat", keepfiles = c(tmp = FALSE, dat = TRUE, true = FALSE), localsstfolder=NULL,sst.fun=function(x,...)get.sst.from.server(x,...), from.ystr=c(3,6), from.dstr=c(7,9), to.ystr=c(11,14), to.dstr=c(15,17))
}

\arguments{
  \item{u}{Model parameter (see reference)}
  \item{v}{Model parameter (see reference)}
  \item{D}{Model parameter (see reference)}
  \item{ss1}{Model parameter (see reference)}
  \item{ss2}{Model parameter (see reference)}
  \item{ss3}{Model parameter (see reference)}
  \item{rho}{Model parameter (see reference)}
  \item{bsst}{Model parameter (see reference)}
  \item{sssst}{Model parameter (see reference)}
  \item{rad}{Model parameter (see reference)}
  \item{sundata}{A dataset like the built-in \code{\link{angle2light}} 
                 describing the relationship between solar altitude 
                 angle and light level}
  \item{sstdates}{Dates where sstdata are to be used}
  \item{fix.first}{Release position}
  \item{t}{Light sampling times (before scanning)}
  \item{window}{Two numbers determining how large an interval around each 
        solar event is used. First the mid-point is selected as the point 
        that best separates day from night (details in reference), and then 
        two numbers are used in the following way: The first number is the 
        fraction (of a 24-hour period) from the mid-point towards the day 
        side of the solar event. The second number is the fraction towards 
        the night side. So the default \code{c(0.05, 0.01)} corresponds to 
        using an interval that 14.4 minutes towards the night side and 72 
        minutes towards the day side. 86.4 minutes total.}
  \item{tmpfile}{Name of temporary file}
  \item{datfile}{Name of data file}
  \item{truefile}{Name of file containing the true simulated track}
  \item{keepfiles}{Logical vector determining what files to keep}
  \item{localsstfolder}{Option to supply saved or own sst files}
  \item{sst.fun}{supply your function to retrive sst}
  \item{from.ystr}{Is an integer vector with two elements describing 
                   what part of the file name describe the year of the 
                   first date the data file represents. For instance if 
                   the names of the data files all have the format 
                   \code{RSyyyyddd_YYYYDDD.dat}, where \code{yyyy} is 
                   the year of the first date the argument should be 
                   \code{c(3,6)}.}
  \item{from.dstr}{Is an integer vector with two elements describing 
                   what part of the file name describe the 'number of 
                   days into the year' of the first date the data file 
                   represents.}
  \item{to.ystr}{Is similar to \code{from.ystr}, but here for the year 
                 of the last date the data file represents.}
  \item{to.dstr}{Is similar to \code{from.dstr}, but here for the 'number 
                 of days into the year' of the last date the data file 
                 represents.}  
}
\value{
  A list containing the needed information about the simulation 
}

\references{Nielsen, A., and Sibert, J.R. 2007. State space model 
            for light based tracking of marine animals. 
            Can. J. Fish. Aquat. Sci. (submitted).}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu} and 
        John Sibert \email{sibert@hawaii.edu}}

\seealso{\code{\link{trackit}}}
\examples{
  # No examples provided here, but try the ones in ?drifter and ?mooring
}

\keyword{models}


