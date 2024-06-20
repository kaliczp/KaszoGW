library(readxl)
raw <- read_excel("Meteo/KaszoDailyTempPrec.xlsx")
## dplyr package necessary
csap.xts <- xts(dplyr::pull(raw, 3), dplyr::pull(raw,Date))
hom.xts <- xts(dplyr::pull(raw, 2), dplyr::pull(raw,Date))

## Ábrázolás
IdoLim <- c(as.POSIXct("2014-10-01"), as.POSIXct("2018-10-16"))

pdf("meteo.pdf", width = 18/2.54, height = 8/2.54)
par(mar = c(2.1, 4.1, 0.5, 4.1), las = 1, lend = 1)
plot.zoo(csap.xts, type = "n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = "",
         xlim = IdoLim, ylim = c(92, 0),
         axes = FALSE)
grid(nx = NA, ny = NULL)
axis(1, as.POSIXct(paste(2015:2018, "01-01", sep = "-")),
     tck = 1, lab = FALSE, col = "lightgray", lty = "dotted")
axis(4)
axis(4, c(10,30,50,70), lab = FALSE)
mtext("Napi csapadékösszeg [mm]", side = 4, line = 2.5, las = 0, col = "blue")
lines(as.zoo(csap.xts), type = "h", col = "blue", lwd = 2)
par(new = TRUE, lend = 0)
plot.zoo(hom.xts, type ="n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = expression(paste("Napi hőmérséklet [",degree*C,"]")),
         xlim = IdoLim, ylim = c(-11, 35))
lines(as.zoo(hom.xts), lwd = 2)
box()
dev.off()

## Yearly and monthly sums
csapyear.xts <- apply.yearly(csap.xts, function(x){sum(x, na.rm = TRUE)})
csapmonthnoshift.xts <- apply.monthly(csap.xts, function(x){sum(x, na.rm = TRUE)})
csapmonth.xts <- xts(coredata(csapmonthnoshift.xts), index(csapmonthnoshift.xts) - 15 * 24 * 60 *60)
write.zoo(csapmonth.xts, "csapmonth.csv", dec = ",", sep = ";", eol = "\r\n", quote = FALSE)

pdf("meteomonth.pdf", width = 18/2.54, height = 8/2.54)
par(mar = c(2.1, 3.4, 0.5, 4.7), las = 1, lend = 1)
plot.zoo(csapmonth.xts, type = "n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = "",
         xlim = IdoLim, ylim = c(510, 0),
         yaxt = "n") # xaxt = "n", 
grid(nx = NA, ny = NULL)
axis(4, at = c(50, 150), tck = 1, col = "lightgray", lty = "dotted")
axis.POSIXct(1, as.POSIXct(paste(2015:2018, "01-01", sep = "-")),
     tck = 1, lab = FALSE, col = "lightgray", lty = "dotted")
# axis(4)
axis(4, c(0,50,100,150))
mtext("Precipitation", adj = 0.5, at = 75, side = 4, line = 2.5, las = 0, col = "blue")
mtext("[mm/month]", adj = 0.5, at = 75, side = 4, line = 3.5, las = 0, col = "blue")
lines(as.zoo(csapmonth.xts), type = "h", col = "blue", lwd = 5)
par(new = TRUE, lend = 0)
plot.zoo(hom.xts, type ="n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = "",
         axes = FALSE,
         xlim = IdoLim, ylim = c(-11, 40))
lines(as.zoo(hom.xts), lwd = 2)
axis(2, c(-10,0,10,20,30))
mtext(expression(paste("Daily temperature [",degree*C,"]")), side = 2, at = 10, adj = 0.5, line = 2.1, las = 0)
box()
dev.off()
