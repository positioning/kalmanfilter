\name{plotbasemap}
\alias{plotbasemap}
\title{ Plot land area on a map with colored polygons}
\description{
  Plots a map within given rectangular region showing land areas as
  colored polygons.  Requires the mapping utility GMT.
}
\usage{
plotbasemap(lon1, lon2, lat1, lat2, grid=FALSE, zoom=FALSE,
        landcolor="darkgreen", seacolor="lightblue", data=gmt3)
}
\arguments{
  \item{lon1}{Longitude of lower left corner of rectangle }
  \item{lon2}{Longitude of upper right corner of rectangle }
  \item{lat1}{Latitude of lower left corner of rectangle }
  \item{lat2}{Latitude of upper right corner of rectangle }
  \item{grid}{Whether to plot grid lines on map }
  \item{zoom}{Whether to start in interactive zoom mode }
  \item{landcolor}{Color of polygons }
  \item{seacolor}{Color of ocean }
  \item{data}{dataset to use}
}
\details{
  A map is plotted with polygons clipped at borders of map region. 

  If the function is started in zoom mode two left-clicks on the 
  map will zoom it to the rectangle spanned by the two points. This 
  zooming is repeated until a right-click on the map is done.   
}
\value{
  Value is \code{NULL} 
}

\author{Anders Nielsen \email{anders.nielsen@hawaii.edu}, and Pierre Kleiber.}

\examples{
  plotbasemap(8,13,53,58)
}

\keyword{models}
