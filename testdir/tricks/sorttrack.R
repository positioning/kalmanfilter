track <- function(track){
    xx <- as.numeric(mdy.date(track$month, track$day, track$year)) +
        track$hour/24 + track$min/24/60 + track$sec/24/60/60
    track <- track[order(xx),]
    return(track)
}
