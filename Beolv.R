## Read in
library(readxl)
nevek <- read_excel("Adat/vegetációs időszakok.xlsx", 1, "A3:C20")
## Table 2 is corrected?
rawdata <- read_excel("Adat/vegetációs időszakok.xlsx", 2, "G2:IB20")
