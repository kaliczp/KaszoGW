## Data import
library(readxl)
GWfeld.df <- as.data.frame(read_excel("Adat/Feldolgozott regisztráltból_2005_2021.xlsx", range = "A2:B28527"))
GWnyers.df <- as.data.frame(read_excel("Adat/Nyers észlelt_1998_2004.xlsx", range = "A2:B732"))

## Conversion to xts
library(xts)
GWfeld.xts <- xts(0.78 - GWfeld.df[,2]/100, GWfeld.df[,"Idopont"])
GWnyers.xts <- xts(0.78 - GWnyers.df[,2]/100, GWnyers.df[,"Idopont"])


## Plot
pdf("HA20221104/LongTerm.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 1, mar = c(3.1, 4.6, 0.3, 0.1), mgp = c(3.5,1,0))
plot.zoo(c(GWnyers.xts,GWfeld.xts), xlab = "", ylab = "GW level [from surface m]", xaxs = "i", type = "n")
grid(nx = NA, ny = NULL)
lines(as.zoo(c(GWnyers.xts,GWfeld.xts)), lwd = 2)
dev.off()
