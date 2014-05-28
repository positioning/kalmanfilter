dat$temp1 <- round(dat$temp,1)    # round temperature values to the nearest degree C
dat$depth1 <- round(dat$depth/10)*10  # round depth values to the nearest 10 meters

# > head(dat)
#  year month day hour min sec depth  temp light temp1 depth1
# 1 2002     8  13    0   0  56   138 11.70   120    12    140
# 2 2002     8  13    0   2  56   152 11.10   116    11    150
# 3 2002     8  13    0   4  56   168 10.80   113    11    170
# 4 2002     8  13    0   6  56   182 10.65   107    11    180
# 5 2002     8  13    0   8  56   182 10.50   105    10    180
# 6 2002     8  13    0  10  56   174 10.50   108    10    170

groupby1 <- factor(dat$temp1, levels=seq(8,22,1)) # temperature from 8 to 22 degC
groupby2 <- factor(dat$depth1, levels=seq(0,360,10)) # depth from 0 to 360 meters

# > head(groupby1)
# [1] 12 11 11 11 10 10
# Levels: 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
# > head(groupby2)
# [1] 140 150 170 180 180 170
# 41 Levels: 0 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 ... 400

mymatrix <- tapply(dat$year, list(groupby1,groupby2), FUN="length")

# >head(mymatrix)
#     0 10 20 30 40 50 60 70 80  90 100 110 120 130 140 150 160 170 180 190 200
# 8  NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
# 9  NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA   1  NA  NA
# 10 NA NA NA NA NA NA NA NA NA  NA  NA   1   3   8   8  18  34  40  25  17  16
# 11 NA NA NA NA NA NA NA  1  4  14  26  74 135 113 139 140  82  34  24   8   1
# 12 NA NA NA NA NA NA  2  4 15  23 146 252 166  92  70  25  16   2   1   1  NA
# 13 NA NA NA NA NA  2  5 32 71 105 225 130  40  16   3   2  NA  NA  NA  NA  NA
#   210 220 230 240 250 260 270 280 290 300 310 320 330 340 350 360
# 8   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
# 9    1  NA  NA  NA   1   1   2  NA  NA   1   2  NA   1  NA  NA   1
# 10  11   6   6   2   2   2   1   2  NA  NA  NA  NA   1  NA  NA  NA
# 11   1   1  NA   1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
# 12  NA  NA   1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
# 13  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA

