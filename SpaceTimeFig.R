## Import
## Minta SpaceTime.png
library(readxl)
RawTable <- as.data.frame(read_excel("Teszt.xlsx"))
GrowingTable <- RawTable[3:16,1:10]
FullTable <- RawTable[21:34,1:10]
