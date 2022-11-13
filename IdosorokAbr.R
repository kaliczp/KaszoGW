## Calculate gw limits
summary(unlist(as.vector(rawdata)))

for(tti in 1:ncol(gw.xts)) {
    pdf(paste0("gw",tti,".pdf"), width = 420/25.4, height = 297 / 25.4)
    plot.zoo(gw.xts[,tti], main = paste("GW", tti, "(", nevek[tti,2], nevek[tti,1], ")"), lwd = 2, xaxs = "i", ylim = c(-465, 85))
    dev.off()
}
## pdfjam gw* --papersize '{420mm,297mm}' --outfile Gw.pdf

### Plot data together

Separate.df <- data.frame(Name = c("Controls", "KST", "MÉ1", "MÉ2")
                        , PlotTitle = c("Kontroll kutak", "KST kutak", "Égerek 1", "Égerek 2")
                        , TimeserNum = c("2,3,4,18",
                                         "7,8,12,15",
                                         "1,5,6,9,10",
                                         "11,13,14,16,17")
                        , LabelPlace = c(14, 185, 100, 185)
                          )

## For A3 page
for(ttsel in 1:nrow(Separate.df)) {
    pdf(paste0(Separate.df[ttsel, "Name"], ".pdf"), width = 420/25.4, height = 297 / 25.4)
    timesernums <- as.numeric(unlist(strsplit(Separate.df[ttsel, "TimeserNum"], ",")))
    tti  <-  timesernums[1]
    ttszin <- 1
    titlepoint <- Separate.df[ttsel, "LabelPlace"]
    plot.zoo(gw.xts[,tti], main = Separate.df[ttsel, "PlotTitle"],
             lwd = 2, xaxs = "i", ylim = c(-465, 85), ylab = "GW depth [cm]")
    text(x = index(as.zoo(gw.xts[titlepoint, tti])),
         y = coredata(as.zoo(gw.xts[titlepoint, tti])),
         paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1))
    for(tti in timesernums[-1]) {
        ttszin <- ttszin + 1
        lines(as.zoo(gw.xts[,tti]), col = ttszin)
        text(x = index(as.zoo(gw.xts[titlepoint ,tti])),
             y = coredata(as.zoo(gw.xts[titlepoint ,tti])),
             paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1.5))
    }
    dev.off()
}

## For publication
for(ttsel in 1:nrow(Separate.df)) {
    pdf(paste0(Separate.df[ttsel, "Name"], ".pdf"), width = 180/25.4, height = 50 / 25.4)
    timesernums <- as.numeric(unlist(strsplit(Separate.df[ttsel, "TimeserNum"], ",")))
    tti  <-  timesernums[1]
    ttszin <- 1
    titlepoint <- Separate.df[ttsel, "LabelPlace"]
    par(mar = c(2.1, 4.1, 0.3, 0.1), las = 1)
    plot.zoo(gw.xts[,tti], main = "",
             lwd = 2, xaxs = "i", ylim = c(-465, 85), ylab = "GW depth [cm]")
    text(x = index(as.zoo(gw.xts[titlepoint, tti])),
         y = coredata(as.zoo(gw.xts[titlepoint, tti])),
         paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1))
    for(tti in timesernums[-1]) {
        ttszin <- ttszin + 1
        lines(as.zoo(gw.xts[,tti]), col = ttszin, lwd = 2)
        text(x = index(as.zoo(gw.xts[titlepoint ,tti])),
             y = coredata(as.zoo(gw.xts[titlepoint ,tti])),
             paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1.5))
    }
    legend("bottomleft", legend = paste("GW", timesernums), col = 1:length(timesernums), lwd = 2)
    dev.off()
}


csapweek.xts <- apply.weekly(csap.xts, function(x){sum(x, na.rm = TRUE)})
csapweek.xts <- xts(coredata(csapweek.xts), as.Date(index(csapweek.xts)))
IdoLim <- c(as.Date("2014-10-01"), as.Date("2018-10-08"))

pdf("KaszóCompare.pdf", width = 180/25.4, height = 50 / 25.4)
par(mar = c(2.1, 4.1, 0.5, 4.1), las = 1)
plot.zoo(csapweek.xts, type = "n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = "",
         xlim = IdoLim, ylim = c(200, 0),
         xaxt = "n", yaxt = "n")
grid(nx = NA, ny = NULL)
axis(1, as.POSIXct(paste(2015:2018, "01-01", sep = "-")),
     tck = 1, lab = FALSE, col = "lightgray", lty = "dotted")
axis(4, at = c(0,50,100))
axis(4, c(25,75), lab = FALSE)
mtext("Csapadék [mm]", side = 4, line = 2.5, las = 0, col = "blue")
lines(as.zoo(csapweek.xts), type = "h", col = "blue", lwd = 3, lend = 1)
par(new = TRUE)
plot.zoo(gw.xts[,18], main = "", type = "n",
         xaxs = "i", yaxs = "i",
         xlab = "", ylab = "",
         xlim = IdoLim, ylim = c(-250, 100),
         yaxt = "n")
lines(as.zoo(gw.xts[,18]), col = 1, lwd = 2)
lines(as.zoo(gw.xts[,9]), col = 2, lwd = 2)
axis(2, at = seq(-250, 50, by = 50))
mtext("Talajvízmélység [cm]", side = 2, line = 2.5, las = 0)
box()
dev.off()
