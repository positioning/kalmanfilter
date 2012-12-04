\name{get.sst.from.server}
\alias{get.sst.from.server}

\title{
Download SST data and parse input files for ukfsst from NOAA ESRL
}
\description{
  Download and reformat SST data from http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html
}
\usage{
get.sst.from.server(track, folder = tempdir(), removeland = TRUE, save = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{
an input track for ukfsst
}
  \item{folder}{
A place to save your data
}

}

\value{
files for use in ukfsst
}

\author{
Benjamin Galuardi and Chi Hin Lam
}

\seealso{\code{\link{get.blended.sst}}}
\examples{
  # No example supplied here
}
\keyword{models}
