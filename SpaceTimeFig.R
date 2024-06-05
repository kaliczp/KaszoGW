## Import
## Minta SpaceTime.png
library(readxl)
RawTable <- as.data.frame(read_excel("Teszt.xlsx"))
GrowingTable <- RawTable[3:16,1:10]
FullTable <- RawTable[21:34,1:10]

plot(1:nrow(GrowingTable), GrowingTable[,10])
points(1:nrow(FullTable), FullTable[,10])
