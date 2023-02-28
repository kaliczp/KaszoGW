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
ForTable.df[treatwellnum, "meanBfr"] <- round(takt$estimate[1], 2)
ForTable.df[treatwellnum, "meanAftr"] <- round(takt$estimate[2], 2)
ForTable.df[treatwellnum, "p.value"] <- round(takt$p.value, 3)
}

write.table(ForTable.df, file = "ForTable.csv", dec = ",", row.names = FALSE)

## Vegetation period
ForTableVeget.df <- Diff.df
ForTableVeget.df$meanBfr  <-  numeric(14)
ForTableVeget.df$meanAftr  <-  numeric(14)
ForTableVeget.df$p.value  <-  numeric(14)

for(treatwellnum in 1:nrow(Diff.df)) {
    takt <- t.test(Diff ~ Mode, NoShftFull.df[NoShftFull.df$WellT == Diff.df[treatwellnum, "Treat"] &
                                            NoShftFull.df$Season == "Veget", ])
    ForTableVeget.df[treatwellnum, "meanBfr"] <- round(takt$estimate[1], 2)
    ForTableVeget.df[treatwellnum, "meanAftr"] <- round(takt$estimate[2], 2)
    ForTableVeget.df[treatwellnum, "p.value"] <- round(takt$p.value, 3)
}

write.table(ForTableVeget.df, file = "ForTableVeget.csv", dec = ",", row.names = FALSE)
