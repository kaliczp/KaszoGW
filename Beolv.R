## Read in
library(readxl)
nevek <- read_excel("Adat/vegetációs időszakok.xlsx", 1, "A3:C20")
## Table 2 is corrected?
rawdata <- read_excel("Adat/vegetációs időszakok.xlsx", 2, "G2:IB20")

### Convert data to timeseries
## Transpose (implicitly convert to matrix from tibble)
rawdata.mat <- t(rawdata)
## First date point from index
strptime("2014/20", format = "%Y/%W") # Bug?? format(as.Date("2014-05-19"), "%Y/%W")
## Create epochs
date.index <- seq(as.Date("2014-05-19"), by = "week", length.out = nrow(rawdata.mat))
library(xts)
gw.xts <- xts(rawdata.mat, date.index)
