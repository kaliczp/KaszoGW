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
mtext("Csapadék [mm]", side = 4, line = 2.5, las = 0, col = "blue")
lines(as.zoo(csap.xts), type = "h", col = "blue", lwd = 2)
par(new = TRUE, lend = 0)
plot.zoo(hom.xts, type ="n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = expression(paste("Hőmérséklet [",degree*C,"]")),
         xlim = IdoLim, ylim = c(-11, 35))
lines(as.zoo(hom.xts), lwd = 2)
box()
dev.off()

## Yearly and monthly sums
csapyear.xts <- apply.yearly(csap.xts, function(x){sum(x, na.rm = TRUE)})
csapmonth.xts <- apply.monthly(csap.xts, function(x){sum(x, na.rm = TRUE)})
write.zoo(csapmonth.xts, "csapmonth.csv", dec = ",", sep = ";", eol = "\r\n", quote = FALSE)
