ry(RODBC)
myconn <-odbcConnect("bluefin")
sql <- "select * from mtdepth"
dat <- sqlQuery(myconn, sql)
close(myconn)
