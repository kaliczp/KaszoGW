library(readxl)
raw <- read_excel("HA20240705/Kaszó_meteo.xlsx")
csapnew.xts <- xts(dplyr::pull(raw, 2), dplyr::pull(raw,Index))

## Hidr. év.
sum(csapnew.xts['2014-10-01/2015-09-30'],na.rm=T)
sum(csapnew.xts['2015-10-01/2016-09-30'],na.rm=T)
sum(csapnew.xts['2016-10-01/2017-09-30'],na.rm=T)
sum(csapnew.xts['2017-10-01/2018-09-30'],na.rm=T)

png("meteonew.png", width = 12.5, height = 5.3, units = "cm", pointsize = 9, res = 600)
# pdf("meteo.pdf", width = 12/2.54, height = 5.3/2.54, pointsize = 9)
par(mar = c(2.1, 3.1, 0.5, 3.1), las = 1, lend = 1)
plot.zoo(csapnew.xts, type = "n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = "",
         xlim = IdoLim, ylim = c(92, 0),
         axes = FALSE)
grid(nx = NA, ny = NULL)
axis(1, as.POSIXct(paste(2015:2018, "01-01", sep = "-")),
     tck = 1, lab = FALSE, col = "lightgray", lty = "dotted")
axis(4)
axis(4, c(10,30,50,70), lab = FALSE)
mtext("Napi csapadékösszeg [mm]", side = 4, line = 2, las = 0, col = "blue")
lines(as.zoo(csap.xts), type = "h", col = "blue", lwd = 2)
par(new = TRUE, lend = 0, mgp = c(2,1,0))
plot.zoo(hom.xts, type ="n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = expression(paste("Napi középhőmérséklet [",degree*C,"]")),
         xlim = IdoLim, ylim = c(-11, 35))
lines(as.zoo(hom.xts), lwd = 2)
axis(1,as.POSIXct('2016-10-01'),tck = 1, lab = FALSE, col = "red", lwd = 4, lty = 2, lend = 2)
box()
dev.off()
