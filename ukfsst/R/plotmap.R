`plotmap` <-
function (x1, x2, y1, y2, resolution = 3, grid = FALSE, add = FALSE, 
    save = FALSE, landcolor = "darkgreen", seacolor = "lightblue", 
    zoom = FALSE) 
{
    gmt <- function(x1, x2, y1, y2, resolution = 3) {
        read.ps.line <- function(txt) {
            txt.split <- strsplit(txt, split = " ")[[1]]
            ret <- c(NA, NA)
            if (length(txt.split) == 3) {
                if (txt.split[3] %in% c("M", "moveto", "D")) {
                  ret <- as.numeric(txt.split[1:2])
                }
            }
            return(ret)
        }
        if (resolution < 1 || resolution > 5) 
            stop("resolution from 1 (full) to 5(crude)")
        res <- c("f", "h", "i", "l", "c")[resolution]
        filen <- tempfile("gmtmap")
        on.exit(unlink(c(filen, ".gmtcommands4")))
        cmd <- paste("pscoast -R", x1, "/", x2, "/", y1, "/", 
            y2, " -Jx2id -P -G -D", res, " -X0 -Y0 >", filen, 
            sep = "")
        .sys(cmd)
        txt <- readLines(filen)
        mat <- matrix(unlist(lapply(txt, read.ps.line)), ncol = 2, 
            byrow = TRUE)
        for (i in 2:nrow(mat)) {
            if (!is.na(mat[i, 1]) & !is.na(mat[i - 1, 1])) 
                mat[i, ] <- mat[i, ] + mat[i - 1, ]
        }
        maxx <- max(mat[, 1], na.rm = TRUE)
        maxy <- max(mat[, 2], na.rm = TRUE)
        mat[, 1] <- mat[, 1]/600 + x1
        mat[, 2] <- mat[, 2]/600 + y1
        return(mat)
    }
    junk <- gmt(x1, x2, y1, y2, resolution = resolution)
    if (!add) {
        plot(c(x1, x2), c(y1, y2), type = "n", ylab = "", xlab = "", 
            xaxs = "i", yaxs = "i")
        rect(x1, y1, x2, y2, col = seacolor)
        if (grid) {
            axis(1, tck = 1)
            axis(2, tck = 1)
        }
    }
    polygon(junk, border = landcolor, col = landcolor)
    if (zoom) {
        ret <- locator(2)
        if (length(ret$x) < 2) {
            zoom <- FALSE
        }
        else {
            x1 <- min(ret$x)
            x2 <- max(ret$x)
            y1 <- min(ret$y)
            y2 <- max(ret$y)
        }
        plotmap(x1, x2, y1, y2, resolution, grid, add, save, 
            landcolor, seacolor, zoom)
    }
    if (save) {
        dimnames(junk)[[2]] <- c("longitude", "latitude")
        return(junk)
    }
}
