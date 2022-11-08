## Medie mobili:
## lisciamento, conservazione dei trend, destagionalizzazione

library(forecast)

## dati ex fig. 3.1
#Consideriamo 2 serie storiche
e1 <- c(0.4,-0.7,0.8,0,-0.9,-0.5,-0.6,0.4,-0.9,0.1,-0.2,-0.9,0.8,-0.1,0.4,-0.7,0.9,-0.5,0.5,-0.9,0.1,-0.3,0.3,0.9,0.8,0.8,0,-0.1,0.2,0.5)
e2 <- c(-0.1,0.7,0.5,-0.3,0.4,-0.1,-0.2,0,0.1,-0.2,0.8,0.6,-0.9,-0.3,-0.1,0.9,-0.1,0.4,-0.5,0.6,0.7,0.9,-0.9,-0.9,0,0.6,0.5,-0.5,0.9,-0.7)

plot(e1, type="l")
abline(h=0, lty=2)
lines(ma(e1,5), col="red")
#ma è funzione perfare medie mobili...questa è media mobile di ordine 5
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


## trend lineare pi? disturbo
y <- 1+0.7*ttrend[-1] + rnorm(length(y))
lines(y, col="blue")
lines(ma(y, 5), col="red")


## serie periodica
x <- rep(c(3.5, 5, 4, 6), 5)

plot(x, type="l")
lines(ma(x, 4), col="blue")

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
## calcolo serie destagionalizzata
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

myma <- function(x, order=5) {
    semismm <- ((order-1)/2)
    inizio <- 1 + semismm
    fine <- length(x) - semismm
    mmx <- rep(NA, length(x))
    for(i in inizio:fine) {
        mmx[i] <- mean(x[(i-semismm):(i+semismm)])
    }
    return(mmx)
}

lines(myma(y), col="red")
lines(myma(y, 9), col="blue")

var(y)
var(myma(y, 5), na.rm=T)
var(myma(y,9), na.rm=T)

lines(myma(y), col="red")
lines(myma(y, 9), col="blue")
var(y)
var(myma(y, 5)) # bisogna rimuovere gli NA
var(myma(y, 5), na.rm=T)
var(myma(y, 9), na.rm=T)

## rapporto di riduzione della varianza residua:
## 5
sum(rep(1/5, 5)^2)

## in generale (per medie semplici!)
rrv <- function(order) sum(rep(1/order, order)^2)

## 9
rrv(9)

