## Pontdiagram a különbségekre

SummaryPoints.df <- data.frame(
    Median = numeric(),
    Diff = numeric(),
    Tree = character(),
    Treat = character(),
    WellC = numeric(),
    WellT = numeric()
)
for(tti in 1:nrow(Diff.df)) {
    aktNumCtrl <- Diff.df[tti, "Control"]
    aktNumTreat <- Diff.df[tti, "Treat"]
    akt.num <- paste0(aktNumCtrl, aktNumTreat)
    akt.median <- median(gw.xts[, aktNumTreat])
    akt.difference <- median(NoShftFull.df[NoShftFull.df$Code == paste0("B", akt.num), "Diff"]) -
        median(NoShftFull.df[NoShftFull.df$Code == paste0("A", akt.num), "Diff"])
    akt.df <- data.frame(Median = akt.median,
                         Diff = akt.difference,
                         Tree = Diff.df[tti, "Tree"],
                         Treat = Diff.df[tti, "TreatmentType"],
                         WellC = aktNumCtrl,
                         WellT = aktNumTreat
                         )
    SummaryPoints.df <- rbind(SummaryPoints.df, akt.df)
}
SummaryPoints.df$Median <- SummaryPoints.df$Median * -1

pdf("SummaryPointsBA.pdf", width = 10)
plot(Diff ~ Median, SummaryPoints.df, xlim = c(-26, 169), ylim = c(-77,23),
     xlab = "Median groundwater level", ylab = "Median of temporal differences")
text(SummaryPoints.df[, c("Median", "Diff")], label = paste(SummaryPoints.df$Tree, SummaryPoints.df$Treat), adj = c(0.5,1.4))
text(SummaryPoints.df[, c("Median", "Diff")], label = paste(SummaryPoints.df$WellT, SummaryPoints.df$WellC, sep = "-"), adj = c(0.5,2.6))
dev.off()

## Visszavált korábbi kiolsztásra
SummaryPoints.df$DiffNeg <- SummaryPoints.df$Diff * -1

pdf("SummaryPoints.pdf", width = 10)
plot(DiffNeg ~ Median, SummaryPoints.df, xlim = c(-26, 169), ylim = c(-30,75),
     xlab = "Median groundwater level", ylab = "Median of temporal differences")
text(SummaryPoints.df[, c("Median", "DiffNeg")], label = paste(SummaryPoints.df$Tree, SummaryPoints.df$Treat), adj = c(0.5,1.4))
text(SummaryPoints.df[, c("Median", "DiffNeg")], label = paste(SummaryPoints.df$WellT, SummaryPoints.df$WellC, sep = "-"), adj = c(0.5,2.6))
dev.off()


### Válogatás a regresszióhoz egy pdf fájlba
pdf("SummaryPointsSeparated.pdf", width = 10)
### Alábbi sorokkal külön-külön futtatni a lenti ábrát
## Csak tölgy 
SummaryAktTree.df <- SummaryPoints.df[SummaryPoints.df$Tree == "Oak",]
## Csak éger
SummaryAktTree.df <- SummaryPoints.df[SummaryPoints.df$Tree == "Alder",]
## Csak éger kiugró töröl
SummaryAktTree.df <- SummaryAktTree.df[-1,]

## Ábra a fenti válogatásokhoz
AktTree.lm <- lm(DiffNeg ~ Median, SummaryAktTree.df)
plot(DiffNeg ~ Median, SummaryAktTree.df, xlim = c(-26, 169), ylim = c(-30,75),
     xlab = "Median groundwater level", ylab = "Median of temporal differences")
text(SummaryAktTree.df[, c("Median", "DiffNeg")], label = paste(SummaryAktTree.df$Tree, SummaryAktTree.df$Treat), adj = c(0.5,1.4))
text(SummaryAktTree.df[, c("Median", "DiffNeg")], label = paste(SummaryAktTree.df$WellT, SummaryAktTree.df$WellC, sep = "-"), adj = c(0.5,2.6))
abline(AktTree.lm, lwd = 2)

## Miután minden válogatással lefuttattam:
dev.off()

### Válogatás a regresszióhoz egy pdf fájlba
pdf("SummaryPointsDepthSeparated.pdf", width = 10)
### Alábbi sorokkal külön-külön futtatni a lenti ábrát
## Shallow
AktMain <- "Shallow"
SummaryAktTree.df <- SummaryPoints.df[SummaryPoints.df$Median <= 100,]

## Deep
AktMain <- "Deep"
SummaryAktTree.df <- SummaryPoints.df[SummaryPoints.df$Median > 100,]

## Ábra a fenti válogatásokhoz
AktTree.lm <- lm(DiffNeg ~ Median, SummaryAktTree.df)
plot(DiffNeg ~ Median, SummaryAktTree.df, xlim = c(-26, 169), ylim = c(-30,75),
     main = AktMain,
     xlab = "Median groundwater level", ylab = "Median of temporal differences")
text(SummaryAktTree.df[, c("Median", "DiffNeg")], label = paste(SummaryAktTree.df$Tree, SummaryAktTree.df$Treat), adj = c(0.5,1.4))
text(SummaryAktTree.df[, c("Median", "DiffNeg")], label = paste(SummaryAktTree.df$WellT, SummaryAktTree.df$WellC, sep = "-"), adj = c(0.5,2.6))
abline(AktTree.lm, lwd = 2)

## Miután minden válogatással lefuttattam:
dev.off()
