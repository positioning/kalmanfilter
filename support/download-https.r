download.file <- function(url, destfile, mode){
  require(httr)
  GET(url, write_disk(destfile, overwrite=TRUE))
}
