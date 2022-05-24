## Read in
library(readxl)
nevek <- read_excel("Adat/vegetációs időszakok.xlsx", 1, "A3:C20")
rawdata <- read_excel("Adat/vegetációs időszakok.xlsx", 1, "H2:HI20")


