################################################################################
#               Laboratorio Serie Storiche Economiche
#                          Giulio Fantuzzi
#
#ABOUT DATA: Fit di curve deterministiche (su esempio pantaloni sportivi)
#            Nel libro dicono dal 1971 a frequenza annuale
################################################################################

pant <- read.table("data/fig.2_18.dat", h=F)
plot(ts(pant, start=1971))
#NB: guardando il grafico sembrerebbe appropriato usare o curva logistica o gompertz

#-------------------------------------------------------------------------------
#Però prima provo a stimare un trend lineare per vedere quanto il modello farebbe schifo
pant <- pant[,1] #riduco la tabella ad un semplice vettore
ttrend <- 1:length(pant)
#Creo un dataframe con dati vendite e ttrend
dati <- data.frame(pant=pant, ttrend=ttrend)
linmod<- lm(pant~ttrend, dati)
plot(dati$pant, type="l")
abline(linmod, col="red")
#Plottiamo anche i residui
plot(resid(linmod))#--->andamento sistematico, quindi è evidente che il modello non va!
#-------------------------------------------------------------------------------

#Appurato che il modello lineare non va, provo con le funzioni non lineari:
#-------------------------------------------------------------------------------
#1) CURVA ESPONENZIALE, criterio dei minimi quadrati non lineari (NLS)
expmod <- nls(pant ~ I(a*exp(b*ttrend)), dati,
              start=list(a=1, b=0), trace=TRUE) 
#NB: start è la initial guess per a e b
#NB: trace significa che mi printa i risultati ad ogni iterazione
#Se compilo sta roba mi da un "errore"--->ciò significa che il modello exp non va bene!!!
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#2) CURVA LOGISTICA
#(i)
logismod <- nls(pant ~ I(a/(1+b*exp(-k*ttrend))), dati,
                start=list(a=2000, b=12, k=0.3), trace=TRUE)
#anche qui tengo traccia di ogni iterazione dell'algoritmo. Output del tipo:
# somma dei minimi quadrati  -  vettore parametri(a,b,k)
#NOTA: per visualizzare meglio tutto posso usare la solita funzione summary(modello)
#NB: in sto caso ci sta 8 iterazioni
# Plottiamo
plot(ts(pant, start=1971))
lines(ts(fitted(logismod), start=1971), col="red", lty=2)
#Sembra andare tutto bene!

#(ii)
# Proviamo a costruire il modello a partire da starting values peggiori:
logismod <- nls(pant ~ I(a/(1+b*exp(-k*ttrend))), dati,
                start=list(a=10000, b=10, k=0.5), trace=TRUE)
#Anche qui bene, ci ha messi 9 iterazioni

#(iii)
# "really bad" starting values:
logismod <- nls(pant ~ I(a/(1+b*exp(-k*ttrend))), dati,
                start=list(a=5000, b=1, k=0.5), trace=TRUE)
# In questo caso dà un errore, perchè la initial guess è troppo brutta
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#3) CURVA DI GOMPERTZ
gompmod <- nls(pant ~ I(a*exp(-b*exp(-k*ttrend))), dati,
               start=list(a=35000, b=1, k=0.5), trace=TRUE)
#Questo era il modo "pre-cotto" che ci dà R
#NB: dà errore, quindi posso dedurre che la curva di gompertz non va bene
#-------------------------------------------------------------------------------





#############################COSA FACOLTATIVA###################################
#Tutte queste sono le funzioni già pronte di R...come funziona tutto ciò a livello interno?
#Ora non vedremo l'algoritmo, ma implementeremo la funzione di costo per i vari modelli
#criterio dei MQO (con cui minimizzeremo la funzione): write "sum of squares" function

#1) LOGISTICA A MANO

mylogis <- function(parms, y, ttrend) {
    a <- parms[1]
    b <- parms[2]
    k <- parms[3]
    yhat <- a/(1+b*exp(-k*ttrend)) #calcolo le y stimate
    uhat <- y - yhat #residui
    return(sum(uhat^2)) #restituisco la somma dei quadrati dei residui
}
# Valori di partenza ragionevoli: un metodo del gradiente trova la soluzione
# reasonable starting values, Nelder-Mead
optim(par=c(30000, 4, 0.5),
      fn=mylogis, #la funzione che abbiamo definito prima
      y=pant, #la var risposta
      ttrend=ttrend)

# Valori di partenza mal scelti: l'ottimizzazione vincolata aiuta
## bad starting values, box constraints
optim(par=c(5000, 0, 0.5),
      fn=mylogis,
      method="L-BFGS-B",  #l'algoritmo con cui la funzione verrà ottimizzata
      lower=c(5000, 0, 0), #costraints che metto sulla base di mie valutazioni sensate
      upper=c(100000, 100, 10),
      y=pant, 
      ttrend=ttrend)

#2) GOMPERTZ A MANO
mygomp <- function(parms, y, ttrend) {
    a <- parms[1]
    b <- parms[2]  
    k <- parms[2]
    yhat <- a*exp(-b*exp(-k*ttrend))
    uhat <- y - yhat
    return(sum(uhat^2))
}

myopt <- optim(par=c(35000, 1, 0.5),
               fn=mygomp,
               method="L-BFGS-B", #l'algoritmo con cui la funzione verrà ottimizzata
               lower=c(25000, 0, 0),
               upper=c(50000, 100, 10),
               y=pant, 
               ttrend=ttrend)
#myopt sarà un oggetto con varie info
## fitted values:
mypar <- myopt$par
my.yhat <- mypar[1]*exp(-mypar[2]*exp(-mypar[3]*ttrend))

plot(pant, type="l")
lines(my.yhat, col="darkgreen", lty=2)
#Nota: vediamo anche graficamente che la curva di Gompertz non è un buon modello!
#      lo potevamo già dedurre dall'errore della funzione nls!



#3) ESPONENZIALE A MANO
mymexp <- function(parms, y, ttrend) {
    b0 <- parms[1]
    b1 <- parms[2]  
    b2 <- parms[2]
    yhat <- b0 + b1*exp(b2*ttrend)
    uhat <- y - yhat
    return(sum(uhat^2))
}

myopt <- optim(par=c(5000, 0, 0.5),
               fn=mymexp,
               method="L-BFGS-B",
               lower=c(5000, -Inf, -100),
               upper=c(35000, 0, 100),
               y=pant, ttrend=ttrend)

## converges! To what??
## fitted values:
mypar <- myopt$par
my.yhat <- mypar[1] + mypar[2]*exp(mypar[3]*ttrend)

plot(pant, type="l")
lines(my.yhat, col="green3", lty=2)



#3) LINEARE A MANO
## Proviamo ad applicare il criterio dei MQ al modello lineare
## (esiste una soluzione ottimale in forma chiusa, OLS, ma
## vediamo se la ritroviamo)

## numerical estimate of linear OLS:

## least squares function
mylin <- function(parms, y, ttrend) {
    b0 <- parms[1]
    b1 <- parms[2]  
    yhat <- b0 + b1*ttrend
    uhat <- y - yhat
    return(sum(uhat^2))
}

myopt <- optim(par=c(0, 0),
               fn=mylin,
               y=pant, ttrend=ttrend)

## fitted values:
my.yhat.l <- myopt$par[1] + myopt$par[2]*ttrend

plot(pant, type="l")
lines(my.yhat.l, col="green3", lty=2)

## OLS fit (closed-form solution)
abline(lm(pant~ttrend), col="purple", lty=3)

## compare parameters:
myopt$par
coef(lm(pant~ttrend))

##################################
