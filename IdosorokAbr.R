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
