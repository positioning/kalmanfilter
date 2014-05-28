fitmap2 <- 
function (x, ci = T, x1, x2, y1, y2 ,...) 
{
    par(mfrow = c(1, 1))
    .addrange <- function(x, pct = 0.05) {
        minx <- min(x, na.rm = TRUE)
        maxx <- max(x, na.rm = TRUE)
        dif <- maxx - minx
        c(minx, maxx) + c(-1, 1) * dif * pct
    }
    .CI.reg <- function(x, level = 0.95, npoints = 100, col = "blue", 
        border = 0, density = 20, lwd = 0.1 * par("lwd"), ...) {
        t.quan <- sqrt(qchisq(level, 2))
        centre <- x[5:6]
        x <- matrix(x[1:4], 2, 2)
        r <- x[1, 2]
        scale <- sqrt(diag(x))
        if (scale[1] > 0) {
            r <- r/scale[1]
        }
        if (scale[2] > 0) {
            r <- r/scale[2]
        }
        r <- min(max(r, -1), 1)
        d <- acos(r)
        a <- seq(0, 2 * pi, len = npoints)
        polygon(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1], 
            t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints, 
            2), col = col, border = border, density = density, 
            lwd = lwd, ...)
    }
    x$nobs <- nrow(x$most.prob.track)
    xlow <- x$most.prob.track[, 1] - 2 * sqrt(x$var.most.prob.track[, 
        1])
    xhig <- x$most.prob.track[, 1] + 2 * sqrt(x$var.most.prob.track[, 
        1])
    ylow <- x$most.prob.track[, 2] - 2 * sqrt(x$var.most.prob.track[, 
        4])
    yhig <- x$most.prob.track[, 2] + 2 * sqrt(x$var.most.prob.track[, 
        4])
    xrange <- .addrange(c(x$most.prob.track[, 1], if (ci) {
        c(xlow, xhig)
    }))
    yrange <- .addrange(c(x$most.prob.track[, 2], if (ci) {
        c(ylow, yhig)
    }))
    #plotbasemap(xrange[1], xrange[2], yrange[1], yrange[2], ...)
	plotbasemap(x1, x2, y1, y2, ...)
    if (ci) {
        apply(cbind(x$var.most.prob.track, x$most.prob.track), 
            1, .CI.reg)
    }
    lines(x$most.prob.track, col = "blue")
    points(x$most.prob.track[c(1, x$nobs), ], pch = c(6, 2))
}