#working directory home
setwd("C:\\Users\\TEMP.TOBIS-PC.000\\Desktop\\Analyse Finanzdaten mit R\\Hausübung")
getwd()

#Instalieren und laden der benötigten Pakete
install.packages("tseries")
install.packages("zoo")
install.packages("forecast")
install.packages("fGarch")
install.packages("car")
library(tseries)
library(zoo)
library(MASS)
library(forecast)
library(fGarch)
library(car)
###################
#####Aufgabe 1#####
###################
#Laden zweier Aktienkurszeitreihen
kuka_data <- get.hist.quote(instrument = "KU2.DE", quote = "Close")
bayer_data <- get.hist.quote(instrument = "BAYN.DE", quote = "Close")
#Renditen berechnen
#Renditen Kuka
rendite_kuka <- diff(kuka_data)/kuka_data[-length(kuka_data)]
rendite_kuka <- coredata(rendite_kuka)
P_kuka <- coredata(kuka_data)
#Renditen Bayer
rendite_bayer <- diff(bayer_data)/bayer_data[-length(bayer_data)]
rendite_bayer <- coredata(rendite_bayer)
P_bayer <- coredata(bayer_data)

#####Aufgabe 1a#####
#Funktionen für empirische Schiefe und Kurtosis 
skewness <- function(x){
  m <- mean(x)
  sigma <- sd(x)
  mean(((x - m)/sigma)^3)
}

kurtosis <- function(x){
  m <- mean(x)
  sigma <- sd(x)
  mean(((x - m)/sigma)^4)
}
###Eigenschaften der univariaten unbedingten Verteilungen
#Rendite plotten
plot(rendite_kuka)
abline(0, 0, col = "red") #stationär, Varianz schwankt im Zeitverlauf
mean(rendite_kuka) #Mittelwert nahe Null
#Histogram für Rendite mit NV eingezeichnet
truehist(rendite_kuka) #In der Mitte deutlich mehr Masse, dafür rechts und links der Mitte weniger, fat tails
xgrid <- seq(-.15, +.15, by=.001)
lines(xgrid, dnorm(xgrid, mean=mean(rendite_kuka), sd=sd(rendite_kuka)), col=2, lwd=2)
#Skewness und Kurtosis berechen
skewness(rendite_kuka) #nahe Null ähnlich wie NV (=0) => tendenziell sym.
kurtosis(rendite_kuka) #weiter enfernt von NV(=3) => Vtlg. ist leptokurtisch
jarque.bera.test(rendite_kuka) #Nullhypthese, dass NV vorliegt wird abgelehnt
#Quantile empirisch vs. NV
prob <- seq(.005, .995, by=.005)
q.emp <- quantile(rendite_kuka, prob=prob)
q.theo <- qnorm(prob, mean=mean(rendite_kuka), sd=sd(rendite_kuka))
plot(q.emp, q.theo)
abline(0,1) #deutliche Abweichung in den Tails

#Rendite plotten
plot(rendite_bayer)
abline(0, 0, col = "red")
mean(rendite_bayer) 
#Histogram für Rendite mit NV eingezeichnet
truehist(rendite_bayer) 
xgrid <- seq(-.15, +.15, by=.001)
lines(xgrid, dnorm(xgrid, mean=mean(rendite_bayer), sd=sd(rendite_bayer)), col=2, lwd=2)
#Skewness und Kurtosis berechen
skewness(rendite_bayer) 
kurtosis(rendite_bayer)
jarque.bera.test(rendite_bayer) #Nullhypthese, dass NV vorliegt wird abgelehnt
#Quantile empirisch vs. NV
prob <- seq(.005, .995, by=.005)
q.emp <- quantile(rendite_bayer, prob=prob)
q.theo <- qnorm(prob, mean=mean(rendite_bayer), sd=sd(rendite_bayer))
plot(q.emp, q.theo)
abline(0,1)


#####Aufgabe 1b#####
###Zeitliche Abhängigkeitsstruktur der Renditen und von Transformationen der Renditen
#Autokorrelationsfunktionen von Preis&Renditen&transformierten Renditen
acf(P_kuka, lag.max = 252) #Starke Autokorrelation der Preise
acf(rendite_kuka, lag.max = 252, na.action = na.pass) #Autokorrelation der Renditen nahe Null
acf(abs(rendite_kuka), lag.max = 252, na.action = na.pass) #persistent korreliert
acf(abs(rendite_kuka)^2, lag.max = 252, na.action = na.pass) #"---"
#Preis stark korreliert, Renditen nicht korreliert, Quadratische&absolute Renditen persistent korreliert
#Autokorrelationsfunktionen von Preis&Renditen
acf(P_bayer, lag.max = 252) 
acf(rendite_bayer, lag.max = 252, na.action = na.pass)
acf(abs(rendite_bayer), lag.max = 252, na.action = na.pass) 
acf(abs(rendite_bayer)^2, lag.max = 252, na.action = na.pass)


#####Aufgabe 1c#####
###Abhängigkeiten zwischen den Renditen der beiden Aktien
#Renditenzeitreihen "gleich lang" machen
Schnittmenge <- intersect(index(bayer_data), index(kuka_data))
length(Schnittmenge)
kuka_data <- kuka_data[index(kuka_data) %in% Schnittmenge]
bayer_data <- bayer_data[index(bayer_data) %in%  Schnittmenge]
length(kuka_data)
length(bayer_data)
#Renditen nach anpassen der Länge
rendite_kuka <- diff(kuka_data)/kuka_data[-length(kuka_data)]
rendite_bayer <- diff(bayer_data)/bayer_data[-length(bayer_data)]
plot(rendite_kuka, rendite_bayer, pch=16, col=rgb(0,0,1,.4))
# Regression: Kuka vs. Bayer
lmod <- lm( rendite_kuka~ rendite_bayer)
summary(lmod)
abline(coef(lmod)[1], coef(lmod)[2], lwd=2, col=2)
cor(as.numeric(rendite_kuka), as.numeric(rendite_bayer))#Positive Korrelation


#####Aufgabe 1d######
max_diff_kuka <- which.max(diff(kuka_data))
index(diff(kuka_data))[max_diff_kuka]
kuka_data[c(max_diff_kuka-1, max_diff_kuka, max_diff_kuka+1, max_diff_kuka+2)]
par(mfrow=c(2,1))
plot(kuka_data)
points(kuka_data[max_diff_kuka], col = "red", pch = 16)
plot(rendite_kuka)
points(rendite_kuka[max_diff_kuka], col = "red", pch = 16)
#Die absoluten Werte steigen für die Kuka Aktie ab ca. 2012 deutlich aber kontinuierlich an. Zuvor sind nur geringe Änderungen der absoluten Werte zu erkennen
#Die Rendite zeigt zwischen 2008 und 2010 größere Ausschläge im Vgl. zu den restlichen Zeitpunkten. 
#Am 15.08.2015 ist der größte Ausschlag der Zeitreihe zu erkennen, da ein chinesischer Investro eim Übernahmangebot an Kuka unterbreitet hat
#Es sind keine saisonalen Schwankungen zu erkennen
max_diff_bayer <- which.max(diff(bayer_data))
index(diff(bayer_data))[max_diff_bayer]
bayer_data[c(max_diff_bayer-1, max_diff_bayer, max_diff_bayer+1, max_diff_bayer+2)]
par(mfrow=c(2,1))
plot(bayer_data)
points(bayer_data[max_diff_bayer], col = "red", pch = 16)
plot(rendite_bayer)
points(rendite_bayer[max_diff_bayer], col = "red", pch = 16)
#Die absoluten Werte der Bayer Aktie sinken von 200 bis ca. 2003 und steigen bis ca 2009 leicht an. Nachdem sie bis 200 konstant geblieben sind 
#folgt  éin stärkerer Anstieg bis 2015, auf welchen ein leichter Abschwung folgte
#Eine saisonale Schwankung ist nicht nicht zu erkennen.
#Die Renditen schwanken von 200 bis 2003 stärker als beim Rest der Zeitreihe.
par(mfrow=c(1,1))

###################
#####Aufgabe 2#####
###################

#####Aufgabe 2a#####
?auto.arima
m_kuka <- auto.arima(rendite_kuka, ic="bic") #ARIMA(4,0,0) ist das beste Modell
m_bayer <- auto.arima(rendite_bayer, ic="bic") #ARIMA (2,0,0) ist das beste Modell


#####Aufgabe 2b#####
#Extraktion Residuen
e_hat_kuka <- m_kuka$residuals
plot(e_hat_kuka)
e_hat_bayer <- m_bayer$residuals
plot(e_hat_bayer)

#GARCH(1,1)
rendite_kuka <- as.numeric(rendite_kuka*100)
m_garch_kuka <- garchFit(formula = ~ garch(1, 1), data=rendite_kuka)
m_garch_bayer <- garchFit(formula = ~garch(1, 1), data=rendite_bayer)
#Innovationsverteilungen und Spezifikationen der bedingten Varianz
m_snorm_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "snorm", data=rendite_kuka)
#m_ged_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "ged", data=rendite_kuka)
#m_sged_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "sged", data=rendite_kuka)
m_std_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "std", data=rendite_kuka)
m_sstd_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "sstd", data=rendite_kuka)
#m_snig_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "snig", data=rendite_kuka)
m_qmle_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "QMLE", data=rendite_kuka)

#Goodness of fit anhand Residualanalyse (für cond.dist = norm)
#Residuen
e_hat_garch_kuka <- m_garch_kuka@residuals
ts.plot(e_hat_garch_kuka)
#Korreliertheit?
acf(e_hat_garch_kuka, na.action = na.pass)
Box.test(e_hat_garch_kuka, lag=10, type="Ljung-Box")
#Residuen sind unkorreliert
acf(e_hat_garch_kuka^2, na.action = na.pass)
Box.test(e_hat_garch_kuka^2, lag=10, type="Ljung-Box")
#Quadratisch Residuen sind korreliert
#Varianz 
var(e_hat_garch_kuka, na.rm = TRUE)
#es scheint ein Vrainzcluster in der Mitte der Daten zu existieren
#Die quadratischen Residuen sind korreliert, was nicht für eine konstanze Varianz spricht
# Normalverteiltheit
hist(e_hat_garch_kuka, breaks=100, probability = TRUE)
xgrid <- seq(-20, 20, by=.01) 
lines(xgrid, dnorm(xgrid, mean(e_hat_garch_kuka), sd(e_hat_garch_kuka)), col=2, lwd=3)
# QQ-Plot 
qqnorm(e_hat_garch_kuka)
#qqline 
abline(0,1, col=2)
jarque.bera.test(e_hat_garch_kuka)
#Die Residuen sind nicht normalverteilt, Zu viel W'keitsmasse in der Mitte und den Rändern 


#Graphische Veranschaulichung von Value at Risk Coverage (0.01) für norm_GARCH(1,1)
alpha <- .01
intv <- coef(m_garch_kuka)[1] + data.frame(lower=qnorm(alpha/2)*m_garch_kuka@sigma.t, upper=qnorm(1-alpha/2)*m_garch_kuka@sigma.t)
ts.plot(rendite_kuka)
lines(intv$lower, col=2)
lines(intv$upper, col=2)
#Überdecken die Intervalle die realisierten Renditen tatsächlich mit der eingestellten W'keit?
which.upper <- which(rendite_kuka > intv$upper)
which.lower <- which(rendite_kuka < intv$lower)
# Wie viele "Verletzungen" des Intervalls treten auf?
length(which.upper) + length(which.lower)
(length(which.upper) + length(which.lower))/length(rendite_kuka)
points(which.upper, rendite_kuka[which.upper], pch=16, col=4, cex=1)
points(which.lower, rendite_kuka[which.lower], pch=16, col=4, cex=1)

#For Schleife für BIC der GArchs
garchs <- c("norm", "snorm","std",
            "sstd", "QMLE")
bic_garch <-as.numeric(vector(length = length(garchs)))
for (i in 1:length(garchs)){
  alpha <- .01
  garch <- garchFit(formula = ~ garch(1, 1), cond.dist = garchs[i], data=rendite_kuka, trace = FALSE)
  bic_garch[i] <- garch@fit$ics["BIC"]
}
bic_garch_kuka <- rbind(garchs, round((bic_garch),4))
bic_garch_kuka
bic_garch_kuka[,which.min(as.numeric(bic_garch))] #std mit niedrigstem bic


#######################2C NOCH NICHT FERTIG############################################
###############################################################
#############################################################
#####Aufgabe 2c#####
#Für Kuka Garch(1,1) mit std, da niedrigsten BIC
rendite_kuka <- rendite_kuka - mean(rendite_kuka)
orig <- 4000
m_kuka <- garchFit(formula = ~ garch(1, 1), data=rendite_kuka[1:orig], cond.dist="std")
coef(m_kuka)
tail(m_kuka@sigma.t)
plot(predict(m_kuka, 360))
ts.plot(rendite_kuka)
lines(m_kuka@sigma.t, col = "red")


# 1-3-Schritt-Prognosen für die verbleibenden Beobachtungen 
orig <- 4200
n <- length(rendite_kuka) 
k <- 0 
h <- 3
FCST.exp <- matrix(nrow=n, ncol=h) 
for(i in orig:(n-3)){ 
  m <- garchFit(formula = ~ garch(1, 1), data = rendite_kuka[1:i], cond.dist="std", trace = FALSE) # Expanding window 
  FCST.exp[i,] <- predict(m, h)$meanForecast  
  SD.exp[i,] <- predict(m, 50)$standardDeviation
  print(i-orig+1) 
  k <- k+1 
} 
# Expanding Window, 1-step
fcst1step.exp <- FCST.exp[orig:(n-h),1] # 1-Schritt-Prognosen 
ts.plot(c(rendite_kuka[1:orig], fcst1step.exp))
lines()
truth1step.exp <- rendite_kuka[(orig+1):(n-h+1)] 
error.exp <- truth1step.exp - fcst1step.exp 
plot(error.exp)
boxplot(error.exp)
#### FÜR BAYER#####
#########################################################################
############################################################################
##############################################################################

#####Aufgabe 2d#####
rendite_kuka <- rendite_kuka - mean(rendite_kuka)
orig <- 4000
m_kuka <- garchFit(formula = ~ garch(1, 1), data=rendite_kuka[1:orig], cond.dist="std")
garch11.condsim <- function(n, w, a, b, y0, sigma2.t){
  p <- length(a)
  q <- length(b)
  y <- sigma2 <- c()
  y[1] <- y0 
  sigma2[1] <- sigma2.t 
  for(i in 2:n){
    sigma2[i] <- w + a*y[(i-1)]^2 + b*sigma2[(i-1)]
    y[i] <- sqrt(sigma2[i])*rnorm(1)
  }    
  return(y)
}

garch.fcst <- function(model, h=2){
  m <- model  
  garch11.condsim(n=h, w=coef(m)[2], a=coef(m)[3], b=coef(m)[4], y0=tail(m@data, 1), sigma2.t=tail(m@sigma.t, 1)^2)[h]  
}

par(mfrow = c(2,1))
for (i in 2:10){
sim <- replicate(10000, garch.fcst(m, h=i))
hist(sim, breaks=100)
qq.plot(sim)
print(i)
}
x <- rt(1000,4)
hist(x, breaks = 100)
lines(x)
par(mfrow = c(1,1))

###################
#####Aufgabe 3#####
###################
#Aufgabe 3a

     
