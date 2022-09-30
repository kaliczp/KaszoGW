## Full database = NoShftFull.df
NoShftFull.df$Mode <- factor(NoShftFull.df$Mode, levels = c("B", "A"), labels = c("Before", "After"))

## Treatment wells
Diff.df$Treat

ForTable.df <- Diff.df
ForTable.df$meanBfr  <-  numeric(14)
ForTable.df$meanAftr  <-  numeric(14)
ForTable.df$p.value  <-  numeric(14)

for(treatwellnum in 1:nrow(Diff.df)) {
takt <- t.test(Diff ~ Mode, NoShftFull.df[NoShftFull.df$WellT == Diff.df[treatwellnum, "Treat"], ])
ForTable.df[treatwellnum, "meanBfr"] <- takt$estimate[1]
ForTable.df[treatwellnum, "meanAftr"] <- takt$estimate[2]
ForTable.df[treatwellnum, "p.value"] <- takt$p.value
}

write.table(ForTable.df, file = "ForTable.csv")
