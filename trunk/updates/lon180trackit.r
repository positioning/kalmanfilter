plotbasemap <- 
function (lon1, lon2, lat1, lat2, grid = FALSE, zoom = FALSE, 
    landcolor = "darkgreen", seacolor = "lightblue", data = gmt3) 
{
    #Change gmt3 from 0-360 to -180 to 180
	x1 <- data[, 1]
	x1 <- ifelse(x1>180,x1-360,x1)
	data[, 1] <- x1
    xrange <- c(lon1, lon2)
    yrange <- c(lat1, lat2)
    aspect <- c(cos((mean(yrange) * pi)/180), 1)
    d <- c(diff(xrange), diff(yrange)) * (1 + 2 * 0) * aspect
    if (!par("new")) 
        plot.new()
    p <- par("fin") - as.vector(matrix(c(0, 1, 1, 0, 0, 1, 1, 
        0), nrow = 2) %*% par("mai"))
    par(pin = p)
    p <- par("pin")
    p <- d * min(p/d)
    par(pin = p)
    d <- d * 0 + ((p/min(p/d) - d)/2)/aspect
    realusr <- c(xrange, yrange) + rep(c(-1, 1), 2) * rep(d, 
        c(2, 2))
    par(usr = realusr)
    rect(lon1, lat1, lon2, lat2, col = seacolor)
    if (grid) {
        axis(1, tck = 1)
        axis(2, tck = 1)
    }
    # if (xrange[1] < 0) {
        # par(usr = realusr + c(360, 360, 0, 0))
        # polygon(data, border = landcolor, col = landcolor)
    # }
    # if (xrange[2] > 360) {
        # par(usr = realusr - c(360, 360, 0, 0))
        # polygon(data, border = landcolor, col = landcolor)
    # }
    par(usr = realusr)
    polygon(data, border = landcolor, col = landcolor)
    rect(lon1, lat1, lon2, lat2, lwd = 1)
    axis(1)
    mtext("Longitude", side = 1, line = 3)
    par(las = 1)
    axis(2)
    mtext("Latitude", side = 2, line = 3, las = 0)
    if (zoom) {
        ret <- locator(2)
        if (length(ret$x) < 2) {
            zoom <- FALSE
        }
        else {
            lon1 <- min(ret$x)
            lon2 <- max(ret$x)
            lat1 <- min(ret$y)
            lat2 <- max(ret$y)
        }
        plotbasemap(lon1, lon2, lat1, lat2, grid, zoom, landcolor, 
            seacolor, data)
    }
}

hotfix.dayline <- function(prep.track, direction="EW")
{
  #direction: East to West (EW); West to East (WE)
  tt <- prep.track$posthead[9]
  xx <- unlist(strsplit(tt, " "))[2]
  if (direction=="EW"){
    yy <- as.numeric(xx)+360
  } else {
    yy <- as.numeric(xx)-360
  }
  tt <- sub(xx,yy,tt)
  zz <- as.numeric(unlist(strsplit(prep.track$posthead[4], " "))[2])
  zz <- (yy+zz)/2
  prep.track$posthead[9] <- tt
  prep.track$posthead[2] <- zz
  return(prep.track)
}

switch2lon180 <- function(fit)
{
  x1 <- fit$most.prob.track[,1]
  x1 <- ifelse(x1>180,x1-360,x1)
  fit$most.prob.track[,1]=x1
  return(fit)
}
