\name{plotmap}
\alias{plotmap}
\title{ Plot land area on a map with colored polygons}
\description{
  Plots a map within given rectangular region showing land areas as
  colored polygons.  Requires the mapping utility GMT.
}
\usage{
plotmap(x1, x2, y1, y2, resolution=3,
        grid=FALSE, add=FALSE, save=FALSE,
        landcolor="darkgreen", seacolor="lightblue", 
        zoom=FALSE)
}
\arguments{
  \item{x1}{Longitude of lower left corner of rectangle }
  \item{x2}{Longitude of upper right corner of rectangle }
  \item{y1}{Latitude of lower left corner of rectangle }
  \item{y2}{Latitude of upper right corner of rectangle }
  \item{resolution}{Map resolution, integer: 1 (highly detailed) to 5 (crude) }
  \item{grid}{Whether to plot grid lines on map }
  \item{add}{Whether to add polygons to an existing plot }
  \item{save}{Whether to return matrix of polygons }
  \item{landcolor}{Color of polygons }
  \item{seacolor}{Color of ocean }
  \item{zoom}{Whether to start in interactive zoom mode }
}
\details{
  A map is plotted with polygons clipped at borders of map region. 

  If the function is started in zoom mode two left-clicks on the 
  map will zoom it to the rectangle spanned by the two points. This 
  zooming is repeated until a right-click on the map is done.   
}
\value{
  Value is \code{NULL} unless \code{save} is \code{TRUE}, in which case
a 2-column matrix is returned containing latitude and longitude
coordinates of the polygon vertices.  Polygons are separated by
\emph{NA}s in both columns.
}

\author{Pierre Kleiber, and Anders Nielsen \email{anders.nielsen@hawaii.edu}}

\seealso{\code{\link{kfsst}}}

\examples{
  #This function requires GMT to be installed. If you have it try typing:
  #  plotmap(8,13,53,58,res=1,zoom=TRUE)
}

\keyword{models}
