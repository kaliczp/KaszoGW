plot(gw.xts['2018-05/'])

## Égerek
## Kontroll + távolság kategóriák
## Csak a május egytől
## Autokorrelált

Slp18 <- gw.xts['2018-05/']
SelSlp18 <- c(1,4,5,6,9,10,11,13,14,16,17,18)
Slp18.df <- data.frame(Gw = as.vector(coredata(Slp18[,SelSlp18])),
                       Time = 1:nrow(Slp18),
                       Well = as.factor(rep(SelSlp18, each = nrow(Slp18))),
                       Treat = as.factor(rep(c("T","C",rep("T",9),"C"), each = nrow(Slp18)))
                       )

Slp18 <- groupedData( Gw ~ Time | Well,
                     data = Slp18.df,
                     labels = list(x = "Time", y = "GW depth"),
                     units = list(x = "(days)", y = "(cm)") )

gsummary( Slp18, invar = TRUE )

plot( Slp18, outer = ~ Treat, aspect = 3 )

fm1 <- lme(fixed = Gw ~ Time * Treat, data = Slp18.df, random = ~ Time | Well)
summary(fm1)

resid( fm1, level = 1 )

plot( fm1,  Gw ~ fitted(.),
     id = 0.05, adj = -0.3 )

qqnorm( fm1, ~resid(.) | Treat )

plot(ACF(fm1))

## Treatment Bottom logveir or Reservoir
## Tree species?
