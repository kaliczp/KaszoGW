## Import
## Minta SpaceTime.png
library(readxl)
RawTable <- as.data.frame(read_excel("Teszt.xlsx"))
GrowingTable <- RawTable[3:16,1:10]
FullTable <- RawTable[21:34,1:10]

pdf("SpaceTimeDotFig.pdf", width = 13/2.54, height = 7.5/2.54,
    pointsize = 9)
par(las = 2, mar = c(5.1, 4.1, 0.2, 0.2))
plot(1:nrow(GrowingTable), GrowingTable[,10],
     ylim = c(-17,90),
     xaxt = "n",
     xlab = "",
     ylab = "Időbeli különbségek a teljes időszakban")
grid(nx = NA, ny = NULL)
for(tti in 1:nrow(GrowingTable)) {
    lines(c(tti,tti), lwd = 2,
          c(GrowingTable[tti,10], FullTable[tti, 10]))
}
points(1:nrow(FullTable), FullTable[,10], pch = 21, bg = "#c0504d")
points(1:nrow(GrowingTable), GrowingTable[,10], pch = 21, bg = "#92d050")
axis(1, at = 1:nrow(GrowingTable),
     label = paste(GrowingTable[,1], GrowingTable[,2], sep = "-"))
mtext("Talajvízkút-párok", side = 1, line = 3.5, las = 1)
dev.off()
