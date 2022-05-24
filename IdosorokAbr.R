## Calculate gw limits
summary(unlist(as.vector(rawdata)))

for(tti in 1:ncol(gw.xts)) {
    pdf(paste0("gw",tti,".pdf"), width = 420/25.4, height = 297 / 25.4)
    plot.zoo(gw.xts[,tti], main = paste("GW", tti, "(", nevek[tti,2], nevek[tti,1], ")"), lwd = 2, xaxs = "i", ylim = c(-465, 85))
    dev.off()
}
## pdfjam gw* --papersize '{420mm,297mm}' --outfile Gw.pdf
