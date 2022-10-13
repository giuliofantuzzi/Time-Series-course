################################################################################
#               Laboratorio Serie Storiche Economiche
#                          Giulio Fantuzzi
#
#ABOUT DATA: serie storica del tasso di disoccupazione in veneto (Unemployment) 
#            misura trimestrale a partire dal 4 trimestre del 1992
################################################################################

#------------------------------------------------------------------
#Importo i dati con la funzione read.table
unemployment_rate <- read.table("data/fig.2_1.dat", header=FALSE)
#------------------------------------------------------------------

#------------------------------------------------------------------
#1)Plotto la prima(e unica colonna)
plot(unemployment_rate[,1])
#NOTA: però nelle ascisse ci sono gli indici...non il tempo!Quindi?
#2)Non straggo la colonna, ma creo un oggetto di tipo TimeSeries
unemp <- ts(unemployment_rate[,1], start=c(1992,4), freq=4)
class(unemp) #unemp è di tipo/classe ts
plot(unemp)
#NOTA: vediamo che ora nelle ascisse ci sono gli anni (freq. trimestrale)
#------------------------------------------------------------------

#------------------------------------------------------------------
#Differenze prime (sono alla base di criterio per regressione)
unemp_diff1 <- diff(unemp)
#NOTA: se unemp era aveva n elementi, le differenze prime ne hanno n-1
plot(unemp_diff1)
abline(0,0, lt=2)#faccio sta linea per far vedere che le diff man mano decrescono
class(unemp_diff1) #essendo unemp di classe ts, anche le differenze prime lo sono!
#Differenza delle differenze prime (CHE E' DIVERSA DALLA DIFFERENZA SECONDA)
unemp_diff2 <- diff(diff(unemp))
#NOTA: qui avrò n-2 elementi
plot(unemp_diff2)
abline(0,0, lt=2)
#NB: al contrario di prima, le diff non stanno decrescendo. Vanno sopra e sotto
#    la retta orizzontale...ma ciò dipende dalla variabilità dei dati...
#    possiamo vedere questo andamento come costante
#    --->allora per criterio diff, il poly di regressione sarà di grado2
#------------------------------------------------------------------

#------------------------------------------------------------------
#Vediamo ora un modo più veloce per trovare il polinomio di regressione
#Costruiamo innanzitutto il vettore degli indici (è il vettore "p" della teoria)
ttrend <- 1:length(unemp)

#caso1: polinomio di grado 1
polymod1 <- lm(unemp ~ ttrend) #oggetto di tipo lm
summary(polymod1)$adj.r.squared
#NB: in questo caso avrei un indice R2 corretto pari a -0.0188 (SCHIFO!)

#caso2: polinomio di grado 2
polymod2 <- lm(unemp ~ ttrend + I(ttrend^2)) #aggiungo un regressore(cioè grado)
summary(polymod2)$adj.r.squared
#NB: in questo caso R2 corretto pari a 0.82287 (BENE!)

#caso3: polinomio di grado 3
polymod3 <- lm(unemp ~ ttrend + I(ttrend^2)+I(ttrend^3))
summary(polymod3)$adj.r.squared
#NB: in questo caso R2 corretto pari a 0.8172
# Esso è < di prima...quindi poly di regressione sarà di grado 2

#NOTA: come visto nella teoria, si usa l'R2 corretto e non l'R2:
#infatti R2 aumenta se aumenta il grado, quindi non ci sarebbe utile!

#Ora che so quale polinomio usare, posso fare 2 cose:
#(1)--> plottare la ts e sovrapporci il polinomio
plot(unemp) #la serie storica di partenza
lines(un.hat, col="red", lty=2) #il polinomio di regressione
#(2)--> costruirmi il vettore degli y teorici
unemp.hat <- ts(fitted(polymod2), start=c(1992,4), freq=4)
#e di conseguenza ricavarmi i residui
res<- unemp.hat - unemp
#------------------------------------------------------------------
#OSSERVAZIONE BONUS
#Una serie può essere scomposta: trend+ stagionalità+ciclo+ errore casuale
plot(unemp)
#E' banale vedere che non è stazionaria. Posso detrendizzarla:
plot(unemp - fitted(polymod2))
#sembra già più stazionaria...bisognerebbe anche vedere le altre componenti

