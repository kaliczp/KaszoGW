## Full database = NoShftFull.df
NoShftFull.df$Mode <- factor(NoShftFull.df$Mode, levels = c("B", "A"), labels = c("Before", "After"))

## Treatment wells
Diff.df$Treat

NoShftFull.df[NoShftFull.df$WellT == Diff.df[1, "Treat"], ]
