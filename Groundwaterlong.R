library(readxl)
GWfeld.df <- as.data.frame(read_excel("Adat/Feldolgozott regisztráltból_2005_2021.xlsx", range = "A2:B28527"))
library(xts)
GWfeld.xts <- xts(160.41 - GWfeld.df[,2]/100, GWfeld.df[,"Idopont"])
