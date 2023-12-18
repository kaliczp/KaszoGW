GyT.GW <- function(x, ind, veg, koz) {
    ##Osztályközök generálása
    osztkoz <- seq(ind, veg, by=koz)
    ##szintek osztálybasorolása
    ##Osztályhatáron a felső osztályba sorol!
    ttszint <- cut(coredata(x),bre=osztkoz,right=F)
### Calc
    newlevels <- levels(ttszint)
    ## Osztályok csökkenő sorrendbe rendezése.
    newlevels <- newlevels[length(newlevels):1]
    szint <- factor(ttszint,levels=newlevels)
    ## Gyakoriság számítás
    ossz <- as.numeric(summary(szint))
    ossz.szaz <- round((ossz/length(x))*100,2)
### Ábra
    par(mfrow=c(1, 2), mar=c(2.1,2.1,.1,0), oma=c(0,0,0.5,0))
    ##Adattábla, amiben az osztályköz alja, teteje szerepel duplán
    ##Megfordítva, hogy a magasabbakhoz kerüljön a magasabb.
    osztkoz.plot <- rep(osztkoz[length(osztkoz):1],each=2)
    ## és ismétlődve az osztály százaléka. Az elején és a végén nullával.
    gyak.plot <- c(0,rep(ossz.szaz,each=2),0)
    gyak.df <- data.frame(gyak.plot,osztkoz.plot)
    ## Gyakoriság plot
    plot(gyak.df,xlim=c(max(ossz.szaz),0),ylim=range(osztkoz)+c(-koz,koz),xaxs="i",yaxs="i",axes=F,type="l",lwd=2)
    grid()
    ## x-tengely
    axis(1)
    ## x-tengely fusson végig
    axis(2,min(osztkoz)-koz,tck=1,lab=F)
    ## y-tengely skála középen
    axis(4, las=1)
    mtext("[%]",side=1,line=1,at=max(ossz.szaz)+0.5)
    text(15, max(osztkoz.plot) + koz/2, "Gyakoriság")
    ## Tartósság
    tart.df <- data.frame(tart=c(0,cumsum(ossz.szaz)),osztkoz=osztkoz[length(osztkoz):1])
    par(mar=c(2.1,0,.1,2.1))
    plot(tart.df,type="l",ylim=range(osztkoz)+c(-koz,koz),xaxs="i",yaxs="i",axes=F,xlab="",ylab="",lwd=2)
    grid()
    ## y-tengely osztályok
    axis(2,at=osztkoz,las=1)
    axis(1)
    axis(1, at = 0, tck=1)
    mtext("[%]",side=1,line=1,at=108)
    mtext("h [cm]",side=2,line=0.1,at=max(osztkoz.plot) + koz/2,las=1)
    text(70, max(osztkoz.plot) + koz/2, "Tartósság")
}

ell <- function(ev=c(1946,1947),ind=100,veg=660,koz=40,x=szeged,kod="s", plotgy = TRUE){
    ##Osztályközök generálása
    osztkoz=seq(ind,veg,by=koz)
    ##Adatsor kiválasztása
    ttmp <- 0
    for(targyev in ev){
        fel=which(names(x)==paste(kod,targyev,sep=""))
        names(x)[fel]
        ttmp=c(ttmp,x[[fel]])
    }
    ttmp <- ttmp[-1] # Az első nullát eltüntetem
    ## Leíró stat kiíratás
    ertek <- c(max(ttmp),min(ttmp),round(mean(ttmp),1),median(ttmp))
    ertek <- c(ertek)
    names(ertek) <- c("NV","KV","KÖV","ÁTV")
    print(ertek)
    
    ##Több évnél itt!
    ido=seq(ISOdate(ev,1,1),ISOdate(ev,12,31),by="day")
    ##Plottolás
    par(mar=c(2.1,2.1,0.1,0.1), ask=TRUE)
  plot(ido,ttmp,typ="l",xaxs="i",xlab="",ylab="")
###Kontingencia tábla generálás
    ##szintek osztálybasorolása
    ##Osztályhatáron a felső osztályba sorol!
    ttszint=cut(ttmp,bre=osztkoz,right=F)
                                        #  print(ttszint[1:31])
                                        #  print(ttmp[1:31])
#   szint=factor(ttszint,sort(unique.default(ttszint),decre=T,na.last = TRUE))
## Korábban a fentivel nem ment az üres osztály.
    newlevels <- levels(ttszint)
    ## Osztályok csökkenő sorrendbe rendezése.
    newlevels <- newlevels[length(newlevels):1]
    szint=factor(ttszint,levels=newlevels)
    ## A hónap kód előállítása.
    hon=format(ido,"%m")
    ##Eredmények kiíratása
    ttab=matrix(as.numeric(format(table(szint,hon))),nc=12)
#  ttab[ttab==0]=NA
    ##ttab=as.data.frame(ttab)
    ## browser()
    ##A végleges táblázat előállítása
    prob <- data.frame(ttab)
    ##Hónap összegzés
    hoossz=numeric()
    for(tti in 1:12)
        hoossz[tti]=sum(prob[,tti])
    ## Gyakoriság számítás
    ossz=as.numeric(summary(szint))
    ossz.szaz <- round((ossz/length(ttmp))*100,2)
    prob=cbind(prob,ossz)
  ## Tartósság szám    ## Gyakoriság számítás
    ossz=as.numeric(summary(szint))
    ossz.szaz <- round((ossz/length(ttmp))*100,2)
ítás
    prob=cbind(prob,cumsum(ossz))
    tart.szaz=round((prob[,ncol(prob)]/length(ttmp))*100,1)
    prob=cbind(prob,tart.szaz)
    ## Havi összegző sor fűzése
    prob=rbind(prob,c(hoossz,sum(hoossz),NA,NA))
  ##Oszlop és sornevek
    row.names(prob)=c(levels(szint),"Sum")
    colnames(prob)=c("J","F","M","A","M","J","J","A","S","O","N","D","Sum","Tar","%")
    prob[prob==0]=NA
    print(as.matrix(prob), na.print = " ") 
    ## Hány nap?
    row.20 <- which(tart.szaz>20)[1]
    nap <- sum(prob[1:row.20,5:10], na.rm = TRUE)
    cat("Meghalad: ", nap, "Osztköz szám: ", length(osztkoz)-1,"\n")
    ##prob=data.frame(print(ttab))
                                        #rbind(prob,rep(12,1))
                                        #print(as.numeric(szint))
                                        #print(tapply(as.numeric(szint),as.factor(hon),sum))
    ##A printet elmenteni és data.frame-t készíteni belőle.
    if(plotgy) {
        par(mfrow=c(1, 2), mar=c(2.1,2.1,.1,0), oma=c(0,0,0.5,0))
        ##Adattábla, amiben az osztályköz alja, teteje szerepel duplán
        ##Megfordítva, hogy a magasabbakhoz kerüljön a magasabb.
        osztkoz.plot <- rep(osztkoz[length(osztkoz):1],each=2)
        ## és ismétlődve az osztály százaléka. Az elején és a végén nullával.
        gyak.plot <- c(0,rep(ossz.szaz,each=2),0)
        gyak.df <- data.frame(gyak.plot,osztkoz.plot)
        ## Gyakoriság plot
        plot(gyak.df,xlim=c(max(ossz.szaz),0),ylim=range(osztkoz)+c(-koz,koz),xaxs="i",yaxs="i",axes=F,type="l",lwd=2)
        ##  plot(ossz.szaz[length(ossz.szaz):1],plotosztkoz[-1],xlim=c(max(ossz.szaz)+5,0),xaxs="i",axes=F)
        grid()
        ## x-tengely
        axis(1)
        ## x-tengely fusson végig
        axis(2,min(osztkoz)-koz,tck=1,lab=F)
        ## y-tengely skála középen
        axis(4, las=1)
        mtext("[%]",side=1,line=1,at=max(ossz.szaz)+0.5)
        text(15, max(osztkoz.plot) + koz/2, "Gyakoriság")
        ## Tartósság
        tart.df <- data.frame(tart=c(0,tart.szaz),osztkoz=osztkoz[length(osztkoz):1])
        par(mar=c(2.1,0,.1,2.1))
        plot(tart.df,type="l",ylim=range(osztkoz)+c(-koz,koz),xaxs="i",yaxs="i",axes=F,xlab="",ylab="",lwd=2)
        grid()
        ## y-tengely osztályok
        axis(2,at=osztkoz,las=1)
        axis(1)
        axis(1,0,tck=1)
        mtext("[%]",side=1,line=1,at=108)
        mtext("h [cm]",side=2,line=0.1,at=max(osztkoz.plot) + koz/2,las=1)
        text(70, max(osztkoz.plot) + koz/2, "Tartósság")
    }
    par(ask = TRUE, mfrow=c(1,1))
}
