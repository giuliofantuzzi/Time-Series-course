## Medie mobili:
## lisciamento, conservazione dei trend, destagionalizzazione

library(forecast)

## dati ex fig. 3.1
#Consideriamo 2 serie storiche
e1 <- c(0.4,-0.7,0.8,0,-0.9,-0.5,-0.6,0.4,-0.9,0.1,-0.2,-0.9,0.8,-0.1,0.4,-0.7,0.9,-0.5,0.5,-0.9,0.1,-0.3,0.3,0.9,0.8,0.8,0,-0.1,0.2,0.5)
e2 <- c(-0.1,0.7,0.5,-0.3,0.4,-0.1,-0.2,0,0.1,-0.2,0.8,0.6,-0.9,-0.3,-0.1,0.9,-0.1,0.4,-0.5,0.6,0.7,0.9,-0.9,-0.9,0,0.6,0.5,-0.5,0.9,-0.7)

plot(e1, type="l")
abline(h=0, lty=2)
lines(ma(e1,5), col="red")#ma è funzione perfare medie mobili...questa è media mobile di ordine 5
plot(ma(e1, 5), col="red", type="l")

ma5.e1 <- ma(e1, 5)[-c(1,2,length(e1)-1, length(e1))]
acf(ma5.e1, na.rm=TRUE)


plot(e2, type="l")
abline(h=0, lty=2)
lines(ma(e2,5), col="blue")

plot(ma(e2, 5), col="blue", type="l")

## Tab. 3.1

## conservazione del trend:

## trend lineare
ttrend <- 1:20
y <- 1+0.7*ttrend[-1]

plot(y, type="l")
y
ma(y, 5)


## costruisco una serie del tipo trend lineare+disturbo white noise
y <- 1+0.7*ttrend[-1] + rnorm(length(y))
lines(y, col="blue") #la serie
lines(ma(y, 5), col="red") #media mobile della serie


## Vediamo ora una serie periodica
x <- rep(c(3.5, 5, 4, 6), 5)

plot(x, type="l")
lines(ma(x, 4), col="blue") #media della serie

#------------------------------------------
## Tab. 3.5
#--->Hp: si tratta di serie moltiplicativa (Y(t)= T*S*e)
dati <- read.table(file="data/fig.3_7.dat")

dati$trim <- rep(1:4, 5)
plot(dati[,1], type="l", xlim=c(0, 24))
#NB: sta serie storica presenta sia trend che stagionalità
dimnames(dati)[[2]][1] <- "y"

#Liscio il trend mediante medie mobili
dati$y.2s <- ma(dati$y, 4)
lines(dati$y.2s, col="red") #linea rossa è la stima del trend con medie mobili
## Ora valutiamo la stagionalità
## indici di stagionalità grezza
dati$IS <- dati$y/dati$y.2s
## stima coefficienti grezzi
dati$S.star <- tapply(dati$IS, dati$trim,
                      mean, na.rm=T) #na.rm = T perche usando mm alcuni dati sono NA
## normalizzo stag grezza a 1
coefst <- prod(dati$S.star)^(1/20)
dati$S <- dati$S.star/coefst
## calcolo serie destagionalizzata (faccio il rapporto poichè la serie era moltiplicativa)
dati$y.d <- dati$y/dati$S
lines(dati$y.d, col="blue")#sta linea blu è la serie destagionalizzata (ma comprensiva di trend giusto?)
###################################

## Effetto di Slutsky-Yule
## es. lisciamento con medie mobili

## simulo white noise, 100 osservazioni
y <- rnorm(100)
#load("y100.rda") # dati della figura nelle slides
plot(y, type="l")
abline(h=0, lty=3)


## scriviamo una funzione "media mobile (semplice)"

myma <- function(x, order=5) {#ordine 5 di default
    semismm <- ((order-1)/2)
    inizio <- 1 + semismm
    fine <- length(x) - semismm
    mmx <- rep(NA, length(x))
    for(i in inizio:fine) {
        mmx[i] <- mean(x[(i-semismm):(i+semismm)])
    }
    return(mmx)
}

lines(myma(y), col="red") #ma di ordine 5
lines(myma(y, 9), col="blue")#ma di ordine 9

var(y)
var(myma(y, 5), na.rm=T)
var(myma(y,9), na.rm=T) #usando ma di ordine 9 la varianza è minore rispetto a ordine 5


## funzione che calcola il rapporto di riduzione della varianza residua:
## in generale (per medie semplici!) vale
rrv <- function(order) sum(rep(1/order, order)^2)

## Confronto tra ordine 5 e 9
rrv(5)
rrv(9)
#NB: entrambi<1: BENE
#    rrv(9)< rrv(5)--->usando la ma di ordine 9 la varianza si riduce di più rispetto ad usare ordine 5
#    e avevamo già notato poco fa che var(myma(y, 5), na.rm=T) > var(myma(y, 9), na.rm=T)
