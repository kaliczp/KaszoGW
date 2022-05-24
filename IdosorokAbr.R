## Calculate gw limits
summary(unlist(as.vector(rawdata)))

for(tti in 1:ncol(gw.xts)) {
    pdf(paste0("gw",tti,".pdf"), width = 420/25.4, height = 297 / 25.4)
    plot.zoo(gw.xts[,tti], main = paste("GW", tti, "(", nevek[tti,2], nevek[tti,1], ")"), lwd = 2, xaxs = "i", ylim = c(-465, 85))
    dev.off()
}
## pdfjam gw* --papersize '{420mm,297mm}' --outfile Gw.pdf

## Kontroll kutak

pdf(paste0("Controls.pdf"), width = 420/25.4, height = 297 / 25.4)
tti  <-  2
ttszin <- 1
titlepoint <- 14
plot.zoo(gw.xts[,tti], main = "Kontroll kutak",
         lwd = 2, xaxs = "i", ylim = c(-465, 85))
text(x = index(as.zoo(gw.xts[titlepoint ,tti])),
     y = coredata(as.zoo(gw.xts[titlepoint ,tti])),
     paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1))
for(tti in c(3:4,18)) {
    ttszin <- ttszin + 1
    lines(as.zoo(gw.xts[,tti]), col = ttszin)
    text(x = index(as.zoo(gw.xts[titlepoint ,tti])),
         y = coredata(as.zoo(gw.xts[titlepoint ,tti])),
         paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1.5))
}
dev.off()


## KST kutak
pdf(paste0("KST.pdf"), width = 420/25.4, height = 297 / 25.4)
tti  <-  7
ttszin <- 1
titlepoint <- 185 
plot.zoo(gw.xts[,tti], main = "KST kutak",
         lwd = 2, xaxs = "i", ylim = c(-465, 85))
text(x = index(as.zoo(gw.xts[titlepoint ,tti])),
     y = coredata(as.zoo(gw.xts[titlepoint ,tti])),
     paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1))
for(tti in c(8,12,15)) {
    ttszin <- ttszin + 1
    lines(as.zoo(gw.xts[,tti]), col = ttszin)
    text(x = index(as.zoo(gw.xts[titlepoint ,tti])),
         y = coredata(as.zoo(gw.xts[titlepoint ,tti])),
         paste("GW", tti), col = ttszin, lwd = 2, adj = c(0.5, 1.5))
}
dev.off()
