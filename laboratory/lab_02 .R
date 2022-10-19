################################################################################
#               Laboratorio Serie Storiche Economiche
#                          Giulio Fantuzzi
#
#ABOUT DATA: serie storica delle vendite di abbigliamento da sci
################################################################################
# Stima di trend (lineari nei parametri) e stagionalità additive

## Import dati vendite Tab. 7.1 Biggeri
vend <- read.table(file="data/Tab7.1.txt", sep="\t", header=TRUE)

plot(ts(vend$vendite, start=c(2005,1), freq=12)) #dati mensili

# C'è un trend lineare evidente...proviamo a detrendizzare la serie
vend$ttrend <- 1:dim(vend)[[1]]
#NOTA: così sto aggiungendo al dataset la colonna ttrend
    
#stimo il trend
det.mod <- lm(vendite ~ ttrend, data=vend)
summary(det.mod)
#plotto per vedere serie e trend sovrapposti
plot(ts(vend$vendite), type="l")
abline(det.mod, col="red", lty=2)

#Adesso che ho il trend posso detrendizzare...
#Allora aggiungo il campo detvendite (cioè le vendite senza l'effetto trend)
#Nello scorso esercizio trovavamo il trend, facevamo il modello polymod e poi facevamo la differenza tra modelli
#Però, senza far tutte ste cose, è analogo considerare direttamente i residui del modello:
vend$detvendite <- resid(det.mod)
plot(resid(det.mod), type="l", col="blue")
#e così abbiamo ottenuto il plot della serie detrendizzata


#NOTA: abbiamo la serie detrendizzata...ma c'è evidente presenza di stagionalità
#vediamo infatti che ci sono picchi ad ogni dicembre (ricordo che son vendite di sci)

#Dalla funzione Yt= D gamma +e voglio stimare a OLS il vettore gamma
stag.mod <- lm(detvendite ~ mese, data=vend)
#NOTA: non serve che io mi costruisca le dummies...basta che specifico a R che voglio regredire verso il mese
#R si crea da solo le dummy mensili e applica i minimi quadrati per stimare le gamma
summary(stag.mod)#-->otterrò le medie dei singoli mesi!

# stima componente erratica
#E' lo stesso discorso di prima per il trend...io potrei partire dalla serie di partenza e toglierci il modello che stimava la stagionalità
#Però ciò è del tutto equivalente di valutare direttamente i residui del modello stimato per la stagionalità

vend$errvendite <- resid(stag.mod) #residui della serie
plot(vend$errvendite, type="l", col="green3")#questa sarà la serie destagionalizzata (oltre che detrendizzata)


# TRICK: si poteva fare la regressione in un solo passo (a partire dalla serie che aveva trend e stagionalità)
unimod <- lm(vendite ~ ttrend + mese, data=vend)
#NOTA: come regressori considero sia il trend che la "dummy" per considerare anche la stagionalità
summary(unimod)
unires <- resid(unimod)
lines(unires, col="red", lty=2)
#NB: potevo usare plot...ha usato lines così si vedo che il grafico coincide col precedente

## noi vedendo la serie avevamo supposto trend lineare...ma se per caso fosse stato trend polinomiale di grado 2?
polimod <- lm(vendite ~ ttrend + I(ttrend^2) + mese, data=vend)
#NOTA: abbiamo considerato sempre stima simultanea di trend e stagionalità
summary(polimod)

#QUALE MODELLO RISULTA PIU' ADATTO?
#confronto tra modello con trend lineare o con trend polinomiale (considerando anche la stagionalità)
summary(unimod)$adj.r.squared
summary(polimod)$adj.r.squared #è leggermente maggiore, quindi è meglio il polinomiale!

#BONUS: 
#è interessante notare come se considero solo il trend ottengo un R2 più basso di quando considero simultaneamente trend e stagionalità 
#(ovvio, perchè se descrivo il modello con entrambe le variabili va da sè che il modello sia più esatto!)
# perchè se non considero variabili esplicative che in realtà servono, ciò impatta sull'errore e del modello (o, analogamente, gli altri parametri risulteranno distorti)