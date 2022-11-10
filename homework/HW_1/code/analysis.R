#IMPORT DATASET
GDP_NOM_CAP_Y= read.csv("../data/GDP_NOM_CAP_Y.csv")
GDP_NOM_TOT_Y= read.csv("../data/GDP_NOM_TOT_Y.csv")
GDP_REAL_base15_Q= read.csv("../data/GDP_REAL_base15_Q.csv")
IPC_base15_Q=read.csv("../data/IPC_base15_Q.csv")
IPC_base15_Y= read.csv("../data/IPC_base15_Y.csv")
#HP_base15_NOM_Q= read.csv("../data/HP_base15_NOM_Q.csv")
#HP_base15_NOM_Y= read.csv("../data/HP_base15_NOM_Y.csv")
HP_base15_REAL_Q= read.csv("../data/HP_base15_REAL_Q.csv")
HP_base15_REAL_Y= read.csv("../data/HP_base15_REAL_Y.csv")
UNEMP_Q= read.csv("../data/UNEMP_Q.csv")
#-------------------------------------------------------------------
#############################################################################
# A) analisi serie storiche pil
#############################################################################

gdp_nom_ita<- GDP_NOM_TOT_Y[GDP_NOM_TOT_Y$LOCATION == "ITA",]$Value
gdp_nom_irl<- GDP_NOM_TOT_Y[GDP_NOM_TOT_Y$LOCATION == "IRL",]$Value
gdp_nom_dnk<- GDP_NOM_TOT_Y[GDP_NOM_TOT_Y$LOCATION == "DNK",]$Value
#PIL NOMINALI ANNUI A CONFRONTO
plot(ts(gdp_nom_ita, start=c(2000,1), frequency = 1), 
     ylim=c(0,3000000), col=2, xlab="Time", ylab="NOMINAL GDP",
     main="Confronto GDP nominale")
lines(ts(gdp_nom_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_nom_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.7, c("ITA", "IRL", "DNK"), col=2:4, lty=1)
#-------------------------------------------------------------------
gdp_nom_cap_ita<- GDP_NOM_CAP_Y[GDP_NOM_CAP_Y$LOCATION == "ITA",]$Value
gdp_nom_cap_irl<- GDP_NOM_CAP_Y[GDP_NOM_CAP_Y$LOCATION == "IRL",]$Value
gdp_nom_cap_dnk<- GDP_NOM_CAP_Y[GDP_NOM_CAP_Y$LOCATION == "DNK",]$Value

#PIL NOMINALI ANNUI PRO CAPITE  A CONFRONTO
plot(ts(gdp_nom_cap_ita, start=c(2000,1), frequency = 1),
     ylim=c(20000,120000),
     col=2, xlab="Time", ylab="NOMINAL GDP pro capita",
     main="Confronto GDP nominale pro capite")
lines(ts(gdp_nom_cap_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_nom_cap_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.7, c("ITA", "IRL", "DNK"), col=2:4, lty=1)
#-------------------------------------------------------------------
#PASSIAMO AL PIL REALE ANNUALE

#Passo intermedio: cambio la base agli IPC
ipc_15_ita<- IPC_base15_Y[IPC_base15_Y$LOCATION == "ITA",]$Value
ipc_00_ita= ipc_15_ita / ipc_15_ita[1]*100
ipc_15_irl<- IPC_base15_Y[IPC_base15_Y$LOCATION == "IRL",]$Value
ipc_00_irl= ipc_15_irl / ipc_15_irl[1]*100
ipc_15_dnk<- IPC_base15_Y[IPC_base15_Y$LOCATION == "DNK",]$Value
ipc_00_dnk= ipc_15_dnk / ipc_15_dnk[1]*100
#Ottengo i pil reali
gdp_real_ita<- gdp_nom_ita / ipc_00_ita
gdp_real_irl<- gdp_nom_irl / ipc_00_irl
gdp_real_dnk<- gdp_nom_dnk / ipc_00_dnk

#Plottiamo i pil reali
plot(ts(gdp_real_ita, start=c(2000,1), frequency = 1),
     col=2, xlab="Time", ylab="REAL GDP", ylim=c(0,20000),
     main="Confronto GDP reale")
lines(ts(gdp_real_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_real_dnk, start=c(2000,1), frequency = 1), type="l",col=4)

#anche qui i termini totali non sono significativi
#Passiamo ai pro capite
gdp_real_cap_ita<- gdp_nom_cap_ita / ipc_00_ita
gdp_real_cap_irl<- gdp_nom_cap_irl / ipc_00_irl
gdp_real_cap_dnk<- gdp_nom_cap_dnk / ipc_00_dnk

plot(ts(gdp_real_cap_ita, start=c(2000,1), frequency = 1),
     col=2, xlab="Time", ylab="REAL GDP pro capita", ylim=c(200,800),
     main="Confronto GDP reale pro capite")
lines(ts(gdp_real_cap_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_real_cap_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.7, c("ITA", "IRL", "DNK"), col=2:4, lty=1)

#STIMA DEL TREND SUL PIL REALE PRO CAPITE DEI VARI PAESI
#HP: sembra che ita e dnk abbiano trend lineare...irl polinomiale/exp
gdp_ttrend<- 1:length(gdp_real_cap_ita) #stesso n° anche per irl e dnk

#ITALIA
LM_gdp_real_cap_ita<-lm(gdp_real_cap_ita ~gdp_ttrend)
PM_gdp_real_cap_ita<-lm(gdp_real_cap_ita ~gdp_ttrend + I(gdp_ttrend^2)+ I(gdp_ttrend^3))
summary(LM_gdp_real_cap_ita)$adj.r.squared   
summary(PM_gdp_real_cap_ita)$adj.r.squared 
#---->per l'italia è opportuno un polinomio di 3 grado
plot(gdp_real_cap_ita,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND GDP REALE PRO CAPITE ITA")
lines(PM_gdp_real_cap_ita$fitted.values, lty=2)
#analisi dei residui
plot(2000:2021,residuals(PM_gdp_real_cap_ita), xlab="", ylab="", main="Residui ITA")
abline(0,0)
#sembra esserci un andamento sistematico...non va bene!
#forse c'è ciclicità nel pil, ma non sappiamo stimarla
#IRLANDA
LM_gdp_real_cap_irl<-lm(gdp_real_cap_irl ~gdp_ttrend)
PM_gdp_real_cap_irl<-lm(gdp_real_cap_irl ~gdp_ttrend + I(gdp_ttrend^2))
summary(LM_gdp_real_cap_ita)$adj.r.squared     
summary(PM_gdp_real_cap_ita)$adj.r.squared 
#NB: da 2 grado in poi viene un r adj sempre =
#Proviamo col modello esponenziale
EXP_gdp_real_cap_irl <- nls(gdp_real_cap_irl ~ I(a*exp(b*gdp_ttrend)),
                            start=list(a=100, b=1), trace=TRUE) 
#--->dà errore di gradiente singolare, quindi il modello exp non è adatto

plot(gdp_real_cap_irl,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND GDP REALE PRO CAPITE IRL")
lines(PM_gdp_real_cap_irl$fitted.values, lty=2)
plot(2000:2021,residuals(PM_gdp_real_cap_irl), xlab="", ylab="", main="Residui IRL")
abline(0,0)
#anche qui non benissimo: direi che c'è ciclicità, ma non la sappiamo calcolare
#DANIMARCA
LM_gdp_real_cap_dnk<-lm(gdp_real_cap_dnk ~gdp_ttrend)
PM_gdp_real_cap_dnk<-lm(gdp_real_cap_dnk ~gdp_ttrend + I(gdp_ttrend^2)+I(gdp_ttrend^3))
summary(LM_gdp_real_cap_dnk)$adj.r.squared     
summary(PM_gdp_real_cap_dnk)$adj.r.squared 
#--->confrontando gli R^2 adj il grado del miglior reg è grado 3
plot(gdp_real_cap_dnk,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND GDP REALE PRO CAPITE DNK")
lines(PM_gdp_real_cap_dnk$fitted.values, lty=2)
#Residui
plot(2000:2021,residuals(PM_gdp_real_cap_dnk), xlab="", ylab="", main="Residui DNK")
abline(0,0)
#Qui sembra bene!



#############################################################################
#B) ANALISI SERIE STORICHE DEI PREZZI DELLE CASE
#############################################################################

#-------------------------------------------------------------------------------
#1)dati reali annuali base 15
hp_base15_real_y_ita<- HP_base15_REAL_Y[HP_base15_REAL_Y$LOCATION == "ITA",]$Value
hp_base15_real_y_irl<- HP_base15_REAL_Y[HP_base15_REAL_Y$LOCATION == "IRL",]$Value
hp_base15_real_y_dnk<- HP_base15_REAL_Y[HP_base15_REAL_Y$LOCATION == "DNK",]$Value
#Passando a base 2000
hp_base00_real_y_ita<-hp_base15_real_y_ita/hp_base15_real_y_ita[1]*100
hp_base00_real_y_irl<-hp_base15_real_y_irl/hp_base15_real_y_irl[1]*100
hp_base00_real_y_dnk<-hp_base15_real_y_dnk/hp_base15_real_y_dnk[1]*100
#-------------------------------------------------------------------------------
#2)dati reali trimestrali base 15
hp_base15_real_q_ita<- HP_base15_REAL_Q[HP_base15_REAL_Q$LOCATION == "ITA",]$Value
hp_base15_real_q_irl<- HP_base15_REAL_Q[HP_base15_REAL_Q$LOCATION == "IRL",]$Value
hp_base15_real_q_dnk<- HP_base15_REAL_Q[HP_base15_REAL_Q$LOCATION == "DNK",]$Value
#Passando a base 2000
hp_base00_real_q_ita<-hp_base15_real_q_ita/hp_base15_real_q_ita[1]*100
hp_base00_real_q_irl<-hp_base15_real_q_irl/hp_base15_real_q_irl[1]*100
hp_base00_real_q_dnk<-hp_base15_real_q_dnk/hp_base15_real_q_dnk[1]*100
#-------------------------------------------------------------------------------
#Grafico del prezzo delle case reale annuale
plot(ts(hp_base00_real_y_ita, start=c(2000,1), frequency = 1), ylim=c(80,180),
     col=2, xlab="Time", ylab="REAL HP",
     main="Confronto HP reale")
lines(ts(hp_base00_real_y_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(hp_base00_real_y_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.5, c("ITA", "IRL", "DNK"), col=2:4, lty=1)

#Affordability: divido il pil pro capite reale(ho solo i dati annui) per hp reale
affordability_ita<-gdp_real_cap_ita[-length(gdp_real_cap_ita)]/hp_base00_real_y_ita
affordability_irl<-gdp_real_cap_irl[-length(gdp_real_cap_irl)]/hp_base00_real_y_irl
affordability_dnk<-gdp_real_cap_dnk[-length(gdp_real_cap_dnk)]/hp_base00_real_y_dnk
plot(ts(affordability_ita, start=c(2000,1), frequency = 1), ylim=c(1.9,5.2),
     col=2, xlab="Time", ylab="REAL GDP CAPITA/REAL HP",
     main="Confronto AFFORDABILITY")
lines(ts(affordability_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(affordability_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.6, c("ITA", "IRL", "DNK"), col=2:4, lty=1)


#Intanto facciamo il trend
hp_ttrend<- 1:length(hp_base00_real_q_ita)

#1) ITALIA
#trend dell'ita
LM_hp_q_ita<-lm(hp_base00_real_q_ita ~ hp_ttrend)
PM_hp_q_ita<-lm(hp_base00_real_q_ita ~ hp_ttrend + I(hp_ttrend^2)+ I(hp_ttrend^3)+ I(hp_ttrend^4)+ I(hp_ttrend^5))
summary(LM_hp_q_ita)$adj.r.squared 
summary(PM_hp_q_ita)$adj.r.squared #il migliore è di grado 5
plot(hp_base00_real_q_ita,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND Hp trimestrale")
lines(PM_hp_q_ita$fitted.values, lty=2)
plot(seq(from=2000.25,to=2022.5,by=0.25),residuals(PM_hp_q_ita), xlab="", ylab="", main="Residui ITA")
abline(0,0)
#residui non al top....magari c'è componente stagionale??
#NB: In realtà dal grafico non si vede nessuna apparente stagionalità
#facciamo un'analisi di  per scoprirlo
#creo il vettore dummies
trimestri<- c(rep(c("Q1","Q2","Q3","Q4"),22 ), "Q1","Q2")
stag_mod_ita<- lm(residuals(PM_hp_q_ita)~trimestri-1)
summary(stag_mod_ita)
plot(residuals(PM_hp_q_ita), main="residui ITA")
lines(fitted.values(stag_mod_ita), col="red")
#linea piatta...dunque verificato che non c'è stagionalità
#NB: se ora facessi i residui dello stag mod sarebbero ~= ai res del PM_mod

#Abbiamo dimostrato che non c'è stagionalità
#In realtà sul sito oecd "indices are seasonally adjusted", quindi tutto torna.
#Quindi come giustifico sti residui schifosi? o c'è ciclo, o servono metodi di ottimizzazione non lineari

#2) IRLANDA
#trend dell'irl
LM_hp_q_irl<-lm(hp_base00_real_q_irl ~ hp_ttrend)
PM_hp_q_irl<-lm(hp_base00_real_q_irl ~ hp_ttrend + I(hp_ttrend^2)+ I(hp_ttrend^3)+ I(hp_ttrend^4)++ I(hp_ttrend^5)++ I(hp_ttrend^6)++ I(hp_ttrend^7)+ I(hp_ttrend^8)+ I(hp_ttrend^9)+ I(hp_ttrend^10))
summary(LM_hp_q_irl)$adj.r.squared 
summary(PM_hp_q_irl)$adj.r.squared #il migliore è di grado 10 :(

plot(hp_base00_real_q_irl,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND Hp trimestrale")
lines(PM_hp_q_irl$fitted.values, lty=2)
plot(seq(from=2000.25,to=2022.5,by=0.25),residuals(PM_hp_q_irl), xlab="", ylab="", main="Residui IRL")
abline(0,0)
trimestri<- c(rep(c("Q1","Q2","Q3","Q4"),22 ), "Q1","Q2")
stag_mod_irl<- lm(residuals(PM_hp_q_irl)~trimestri-1)
summary(stag_mod_irl)
plot(residuals(PM_hp_q_irl))
lines(fitted.values(stag_mod_ita), col="red")
#linea piatta...dunque verificato che non c'è stagionalità
#NB: se ora facessi i residui dello stag mod sarebbero ~= ai res del PM_mod

#3) DANIMARCA
#trend della danimarca
LM_hp_q_dnk<-lm(hp_base00_real_q_dnk ~ hp_ttrend)
PM_hp_q_dnk<-lm(hp_base00_real_q_dnk ~ hp_ttrend + I(hp_ttrend^2)+ I(hp_ttrend^3))
summary(LM_hp_q_dnk)$adj.r.squared 
summary(PM_hp_q_dnk)$adj.r.squared #il migliore è di grado 3

plot(seq(from=2000.25,to=2022.5,by=0.25),hp_base00_real_q_dnk,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND Hp trimestrale")
lines(seq(from=2000.25,to=2022.5,by=0.25),PM_hp_q_dnk$fitted.values, lty=2)

plot(seq(from=2000.25,to=2022.5,by=0.25),residuals(PM_hp_q_irl), xlab="", ylab="", main="Residui IRL")
abline(0,0)
trimestri<- c(rep(c("Q1","Q2","Q3","Q4"),22 ), "Q1","Q2")
stag_mod_dnk<- lm(residuals(PM_hp_q_irl)~trimestri-1)
summary(stag_mod_dnk)
plot(residuals(PM_hp_q_dnk))
lines(fitted.values(stag_mod_ita), col="red")
#linea piatta...dunque verificato che non c'è stagionalità
#NB: se ora facessi i residui dello stag mod sarebbero ~= ai res del PM_mod



#############################################################################
# C) TASSO DI DISOCCUPAZIONE
#############################################################################
unemp_ita<- UNEMP_Q[UNEMP_Q$LOCATION == "ITA",]$Value
unemp_irl<- UNEMP_Q[UNEMP_Q$LOCATION == "IRL",]$Value
unemp_dnk<- UNEMP_Q[UNEMP_Q$LOCATION == "DNK",]$Value

plot(ts(unemp_ita, start=c(2000,1), frequency = 4), 
     main="Tasso disoccupazione ITA", col=2, ylim=c(3,16),
     xlab="Time", ylab="Unemployment rate")
lines(ts(unemp_irl, start=c(2000,1), frequency = 4), type="l",col=3)
lines(ts(unemp_dnk, start=c(2000,1), frequency = 4), type="l",col=4)
legend("topleft",cex=0.55, c("ITA", "IRL", "DNK"), col=2:4, lty=1)
