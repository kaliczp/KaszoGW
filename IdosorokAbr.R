## Calculate gw limits
summary(unlist(as.vector(rawdata)))

tti <- 6
pdf(paste0("gw",tti,".pdf"), width = 420/25.4, height = 297 / 25.4)
plot.zoo(gw.xts[,tti], main = paste("GW", tti, "(", nevek[1,2], nevek[1,1], ")"), lwd = 2, xaxs = "i", ylim = c(-465, 85))
dev.off()
