.trimit <- function (scan) 
{
    oldDigits <- options()$digits
    options(digits = 22)
    if (is.null(scan$data)) 
        stop("This function can only be used if internal=TRUE in call to prepit()")
    hid <- (scan$data[, 1] - floor(scan$data[, 1])) * 24
    hid <- (hid + (12 - mean(hid)))%%24
    plot(hid, ylab = "Hours into day")
    idx <- as.integer(unique((scan$data[, 12])))
    remove <- logical(length(idx))
    no <- as.numeric(table(scan$data[, 12]))
    xx <- 1:nrow(scan$data)
    xscale <- nrow(scan$data) - 1
    while (TRUE) {
        xy <- locator(1)
        if (is.null(xy)) 
            break
        sunsetno <- scan$data[which.min(((xx - xy$x)/xscale)^2 + 
            ((hid - xy$y)/24)^2), 12]
        remove[sunsetno] <- !remove[sunsetno]
        locIdx <- which(scan$data[, 12] == sunsetno)
        points(xx[locIdx], hid[locIdx], col = ifelse(remove[sunsetno], 
            "red", "black"))
    }
    keep <- scan$data[, 12] %in% idx[!remove]
    scan$data <- scan$data[keep, ]
    scan$data[, 12] <- pmatch(scan$data[, 12], unique(scan$data[, 
        12]), duplicates.ok = TRUE)
    scan$posthead[14] <- nrow(scan$data)  #change 8 to 14
    scan$posthead[18] <- length(unique(scan$data[, 12]))  #change 12 to 18
    no <- no[!remove]
    scan$posthead[20] <- paste(as.integer(cumsum(c(1, no[-length(no)]))), 
        collapse = " ")   # change 14 to 20
    scan$posthead[22] <- paste(as.integer(cumsum(no)), collapse = " ")
    options(digits = oldDigits)
    return(scan)  #change 16 to 22
}