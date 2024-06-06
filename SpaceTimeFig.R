## Import
## Minta SpaceTime.png
library(readxl)
RawTable <- as.data.frame(read_excel("Teszt.xlsx"))
GrowingTable <- RawTable[3:16,1:10]
FullTable <- RawTable[21:34,1:10]

plot(1:nrow(GrowingTable), GrowingTable[,10],
     ylim = c(-17,90),
     xlab = "Talajvízkút-párok",
     ylab = "Időbeli különbségek a teljes időszakban")
grid(nx = NA, ny = NULL)
for(tti in 1:nrow(GrowingTable)) {
    lines(c(tti,tti),
          c(GrowingTable[tti,10], FullTable[tti, 10]))
}
points(1:nrow(FullTable), FullTable[,10], pch = 21, bg = "#c0504d")
points(1:nrow(GrowingTable), GrowingTable[,10], pch = 21, bg = "#92d050")
