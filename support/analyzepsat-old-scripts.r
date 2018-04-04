track2KD <- function (track, xsize = 0.1, ysize = 0.1, range.x = c(-100, 
    -5), range.y = c(20, 50)) 
{
    require(GenKern)
    dd = dim(track)
    xgsize = (range.x[2] - range.x[1])/xsize
    ygsize = (range.y[2] - range.y[1])/ysize
    if (dd[2] > 6) {
        xidx = 8
        yidx = 9
        coridx = 4:7
        varidx = c(4, 7)
    }
    else {
        xidx = 5
        yidx = 6
        coridx = 1:4
        varidx = c(1, 4)
    }
    len = length(track[, xidx])
    x = track[, xidx]
    y = track[, yidx]
    vars = cbind(track[, varidx])
    cormat = NULL
    for (i in 1:len) {
        cormat[i] = as.vector(cov2cor(matrix(as.numeric(track[i, 
            coridx]), 2, 2))[2])
    }
    cormat[is.nan(cormat)] = 0
    cormat[is.infinite(cormat)] = 0
    vars[vars == 0] = 1e-10
    op = KernSur(x, y, xgridsize = xgsize, xbandwidth = vars[, 
        1], ybandwidth = vars[, 2], ygridsize = ygsize, correlation = cormat, 
        range.x = range.x, range.y = range.y)
    op[[3]][is.na(op[[3]])] = 0
    op[[3]][is.nan(op[[3]])] = 0
    op
}
