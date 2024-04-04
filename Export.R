## GW
write.zoo(gw.xts, "gwxts.csv") ## Excelbe mentve.

## Meteo heti
csapweek.xts <- apply.weekly(csap.xts, function(x){sum(x, na.rm = TRUE)})
homweek.xts <- round(apply.weekly(hom.xts, mean),1)
write.zoo(cbind(homweek.xts, csapweek.xts), "meteoweek.csv", dec = ",", sep = ";", eol = "\r\n", quote = FALSE)
