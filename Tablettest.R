## Full database = NoShftFull.df
NoShftFull.df$Mode <- factor(NoShftFull.df$Mode, levels = c("B", "A"), labels = c("Before", "After"))

## Treatment wells
Diff.df$Treat

treatwellnum <- 1
t.test(Diff ~ Mode, NoShftFull.df[NoShftFull.df$WellT == Diff.df[treatwellnum, "Treat"], ])
