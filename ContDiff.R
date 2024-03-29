## Space-time difference

Diff.df <- data.frame(
    Treat = c(5,6,7,8,9,10,11,12,13,14,15,16,17,1),
    Control = c(4,18,2,3,18,18,18,3,18,18,3,18,18,18),
    Tree = factor(c("A", "A", "O", "O", "A", "A", "A", "O", "A", "A", "O", "A", "A", "A"), labels = c("Alder", "Oak")),
    TreatmentType = factor(c("Weir", "Weir", "Reservoir", "Reservoir", "Reservoir", "Weir",
                             "Weir", "Weir", "Weir", "Weir", "Weir", "Weir", "Weir", "Weir"))
)

## Calculate list of min & max
limits.df <- data.frame(Min = numeric(),
                        Max = numeric())
for(tti in 1:nrow(Diff.df)) {
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    aktdiff <- gw.xts[, aktNumTreat] - gw.xts[, aktNumCtrl]
    limits.df[tti, "Min"]  <- min(aktdiff)
    limits.df[tti, "Max"]  <- max(aktdiff)
}

for(tti in 1:nrow(Diff.df)) {
    fname <- paste0("Diff_",aktNumTreat, "Treat_", aktNumCtrl, "Ctrl")
    pdf(paste0(fname, ".pdf"), width = 297/25.4, height = 210 / 25.4)
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    plot.zoo(gw.xts[, aktNumTreat] - gw.xts[, aktNumCtrl],
             main = paste("GW",aktNumTreat, "Treat - ", aktNumCtrl, "Ctrl"),
             lwd = 2, xaxs = "i", ylim = c(-100, 350), ylab = "Diff [cm]", xlab = ""
             )
    axis(1, as.Date("2016-10-01"), tck =1, col = "gray", lab = F)
    dev.off()
}

for(tti in 1:nrow(Diff.df)) {
    fname <- paste0("BoxDiff_",aktNumTreat, "Treat_", aktNumCtrl, "Ctrl")
    pdf(paste0(fname, ".pdf"), width = 297/25.4, height = 210 / 25.4)
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    Before <- gw.xts['2014-10-01/2016-09-30', aktNumTreat] - gw.xts['2014-10-01/2016-09-30', aktNumCtrl]
    After <- gw.xts['2016-10-01/2018-09-30', aktNumTreat] - gw.xts['2016-10-01/2018-09-30', aktNumCtrl]
    boxplot(list(Before = coredata(Before),
                 After = coredata(After)),
             main = paste("Teljes GW",aktNumTreat, "Treat - ", aktNumCtrl, "Ctrl")
             )
    dev.off()
}

for(tti in 1:nrow(Diff.df)) {
    fname <- paste0("VegetBoxDiff_",aktNumTreat, "Treat_", aktNumCtrl, "Ctrl")
    pdf(paste0(fname, ".pdf"), width = 297/25.4, height = 210 / 25.4)
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    Before <- gw.xts['2015-04-01/2015-09-30', aktNumTreat] - gw.xts['2015-04-01/2015-09-30', aktNumCtrl]
    Before <- c(Before, gw.xts['2016-04-01/2016-09-30', aktNumTreat] - gw.xts['2016-04-01/2016-09-30', aktNumCtrl])
    After <- gw.xts['2017-04-01/2017-09-30', aktNumTreat] - gw.xts['2017-04-01/2017-09-30', aktNumCtrl]
    After <- c(After, gw.xts['2018-04-01/2018-09-30', aktNumTreat] - gw.xts['2018-04-01/2018-09-30', aktNumCtrl])
    boxplot(list(Before = coredata(Before),
                 After = coredata(After)),
             main = paste("Veget GW",aktNumTreat, "Treat - ", aktNumCtrl, "Ctrl")
             )
    dev.off()
}


NoShftFull.df <- data.frame(Diff = numeric(),
                            Mode = character(),
                            Season = character(),
                            WellC = numeric(),
                            WellT = numeric()#,
##                            Code = character()
                            )
for(tti in 1:nrow(Diff.df)) {
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    Before <- gw.xts['2014-10-01/2016-09-30', aktNumTreat] - gw.xts['2014-10-01/2016-09-30', aktNumCtrl]
    After <- gw.xts['2016-10-01/2018-09-30', aktNumTreat] - gw.xts['2016-10-01/2018-09-30', aktNumCtrl]
    aktData = c(as.vector(coredata(Before)), as.vector(coredata(After)))
    akt.Month <- as.numeric(format(index(gw.xts['2014-10-01/2018-09-30']), "%m"))
    akt.df <- data.frame(Diff = aktData,
                         Mode = c(rep("B", nrow(Before)),
                                  rep("A", nrow(After))),
                         Season = ifelse(akt.Month > 3 & akt.Month < 10, "Veget", "Dormant"),
                         WellC = aktNumCtrl,
                         WellT = aktNumTreat)
    NoShftFull.df <- rbind(NoShftFull.df, akt.df)
}
ttcode  <- paste0(NoShftFull.df$Mode, NoShftFull.df$WellC, NoShftFull.df$WellT)
NoShftFull.df$Code = factor(ttcode, levels = unique(ttcode))
NoShftFull.df$Season = factor(NoShftFull.df$Season, levels = unique(NoShftFull.df$Season))

fname <- paste0("BoxDiffAll")
pdf(paste0(fname, ".pdf"), width = 297/25.4, height = 210 / 25.4)
boxplot(Diff ~ Code, NoShftFull.df, main = "Full period", col = c(3,4), las = 2, xlab = "", ylab = "Difference [cm]")
dev.off()

ShftFull.df <- data.frame(Diff = numeric(),
                            Mode = character(),
                            WellC = numeric(),
                            WellT = numeric()#,
##                            Code = character()
                            )
for(tti in 1:nrow(Diff.df)) {
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    Before <- gw.xts['2014-10-01/2016-09-30', aktNumTreat] - gw.xts['2014-10-01/2016-09-30', aktNumCtrl]
    After <- gw.xts['2016-10-01/2018-09-30', aktNumTreat] - gw.xts['2016-10-01/2018-09-30', aktNumCtrl]
    CenterDiff <- median(Before)
    Before <- Before - CenterDiff
    After <- After - CenterDiff
    aktData = c(as.vector(coredata(Before)), as.vector(coredata(After)))
    akt.df <- data.frame(Diff = aktData,
                         Mode = c(rep("B", nrow(Before)),
                                  rep("A", nrow(After))),
                         WellC = aktNumCtrl,
                         WellT = aktNumTreat)
    ShftFull.df <- rbind(ShftFull.df, akt.df)
}
ttcode  <- paste0(ShftFull.df$Mode, ShftFull.df$WellC, ShftFull.df$WellT)
ShftFull.df$Code = factor(ttcode, levels = unique(ttcode))
## Generate type
ttcode[grep("[AB]189", ttcode)] <- "Reservoir"
ttcode[grep("[AB][23][78]", ttcode)] <- "Reservoir"
ttcode[grep("[AB]", ttcode)] <- "Weir"
ShftFull.df$Type <- factor(ttcode, levels = unique(ttcode))

boxplot(Diff ~ Code, ShftFull.df, col= c(3,4))

## Nem normalizált ábra
pdf("HA20221104/NemNormalizáltKülönbségek.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, NoShftFull.df, col= c(3,4), xlab = "", ylab = "STD")
dev.off()

## Normalizált ábra
pdf("HA20221104/NormalizáltKülönbségek.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, ShftFull.df, col= c(3,4), xlab = "", ylab = "NSTD")
dev.off()


## Normalizált ábra gát és küszöb
pdf("HA20231213/NormalizáltKülönbségekReserv.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, droplevels(ShftFull.df[ShftFull.df$Type == "Reservoir",]), col= c(3,4), xlab = "", ylab = "NSTD")
dev.off()

pdf("HA20231213/NormalizáltKülönbségekWeir.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, droplevels(ShftFull.df[ShftFull.df$Type == "Weir",]), col= c(3,4), xlab = "", ylab = "NSTD")
dev.off()



## Vegetációs nem normalizált ábra
pdf("HA20230209/VegetNemNormalizáltKülönbségek.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, NoShftFull.df[NoShftFull.df$Season == "Veget", ], col= c(3,4), xlab = "", ylab = "STD")
dev.off()

## Vegetációs normalizált ábra
VegetShftFull.df <- data.frame(Diff = numeric(),
                               Mode = character(),
                               WellC = numeric(),
                               WellT = numeric()#,
                               ## Code = character()
                            )
for(tti in 1:nrow(Diff.df)) {
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    Before <- gw.xts['2015-04-01/2015-09-30', aktNumTreat] - gw.xts['2015-04-01/2015-09-30', aktNumCtrl]
    Before <- c(Before, gw.xts['2016-04-01/2016-09-30', aktNumTreat] - gw.xts['2016-04-01/2016-09-30', aktNumCtrl])
    After <- gw.xts['2017-04-01/2017-09-30', aktNumTreat] - gw.xts['2017-04-01/2017-09-30', aktNumCtrl]
    After <- c(After, gw.xts['2018-04-01/2018-09-30', aktNumTreat] - gw.xts['2018-04-01/2018-09-30', aktNumCtrl])
    CenterDiff <- median(Before)
    Before <- Before - CenterDiff
    After <- After - CenterDiff
    aktData = c(as.vector(coredata(Before)), as.vector(coredata(After)))
    akt.df <- data.frame(Diff = aktData,
                         Mode = c(rep("B", nrow(Before)),
                                  rep("A", nrow(After))),
                         WellC = aktNumCtrl,
                         WellT = aktNumTreat)
    VegetShftFull.df <- rbind(VegetShftFull.df, akt.df)
}
ttcode  <- paste0(VegetShftFull.df$Mode, VegetShftFull.df$WellC, VegetShftFull.df$WellT)
VegetShftFull.df$Code = factor(ttcode, levels = unique(ttcode))
## Generate type
ttcode[grep("[AB]189", ttcode)] <- "Reservoir"
ttcode[grep("[AB][23][78]", ttcode)] <- "Reservoir"
ttcode[grep("[AB]", ttcode)] <- "Weir"
VegetShftFull.df$Type <- factor(ttcode, levels = unique(ttcode))


pdf("HA20230209/VegetNormalizáltKülönbségek.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, VegetShftFull.df, col= c(3,4), xlab = "", ylab = "NSTD")
dev.off()

## logweir 3,2,4,18,12,15,11,10,6,16,14,5,1,13,17
## reservoir 8,7,9, azaz 27, 38, 189

pdf("HA20231213/VegetNormalizáltKülönbségekWeir.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, droplevels(VegetShftFull.df[VegetShftFull.df$Type == "Weir",]), col= c(3,4), xlab = "", ylab = "NSTD")
dev.off()

pdf("HA20231213/VegetNormalizáltKülönbségekReserv.pdf", height = 12 / 2.54, width = 18 / 2.54, points = 11)
par(las = 2, mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(Diff ~ Code, droplevels(VegetShftFull.df[VegetShftFull.df$Type == "Reservoir",]), col= c(3,4), xlab = "", ylab = "NSTD")
dev.off()
