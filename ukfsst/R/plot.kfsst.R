`plot.kfsst` <-
function (x, ci = FALSE, points = TRUE, pred = TRUE, most = TRUE, 
    gmt = FALSE, ...) 
{
    par(mar = c(4, 4, 5, 1) + 0.1)
    layout(matrix(c(1, 4, 2, 4, 3, 4), ncol = 2, nrow = 3, byrow = TRUE))
    .plot1(x = x, ci = ci, points = points, pred = pred, most = most, 
        ...)
    xp <- par("xaxp")
    xcut <- seq(xp[1], xp[2], length = xp[3] + 1)
    par(mar = c(6, 4, 3, 1) + 0.1)
    .plot2(x = x, ci = ci, points = points, pred = pred, most = most, 
        ...)
    axis(1)
    axis(2)
    box()
    .plotsst(x = x, ci = ci, points = points, pred = pred, most = most, 
        ...)
    if (.have.date()) {
        firstdate <- mdy.date(x$date[1, "month"], x$date[1, "day"], 
            x$date[1, "year"])
        axis(2)
        axis(1, at = xcut, labels = paste(firstdate + xcut), 
            las = 2)
        box()
        mtext("Date", side = 1, line = 5, cex = par("cex"))
    }
    else {
        axis(1)
        axis(2)
        box()
        mtext("Days at liberty", side = 1, line = 3, cex = par("cex"))
    }
    par(mar = c(4, 4, 5, 1) + 0.1)
    .plot3(x = x, ci = ci, points = points, pred = pred, most = most, 
        ...)
    title(paste("Estimated track of", x$data.name), outer = TRUE, 
        line = -1.5)
    if (gmt) {
        .gmt.plot(x, map.width = 5, ci = ci, points = points, 
            pred = pred, most = most, ...)
    }
}
