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
    akt.difference <- median(NoShftFull.df[NoShftFull.df$Code == paste0("A", akt.num), "Diff"]) -
        median(NoShftFull.df[NoShftFull.df$Code == paste0("B", akt.num), "Diff"])
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

pdf("SummaryPoints.pdf", width = 10)
plot(Diff ~ Median, SummaryPoints.df, type = "n")
text(SummaryPoints.df[, c("Median", "Diff")], label = paste0(SummaryPoints.df$Tree, SummaryPoints.df$Treat))
dev.off()
