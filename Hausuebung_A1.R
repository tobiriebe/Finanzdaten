#working directory home
setwd("C:\\Users\\TEMP.TOBIS-PC.000\\Desktop\\Analyse Finanzdaten mit R\\Hausübung")
getwd()

#Instalieren und laden der benötigten Pakete
install.packages("tseries")
install.packages("zoo")
install.packages("forecast")
install.packages("fGarch")
library(tseries)
library(zoo)
library(MASS)
library(forecast)
library(fGarch)
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
skewness(rendite_kuka) 
kurtosis(rendite_kuka)
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
m_garch_kuka <- garchFit(formula = ~ garch(1, 1), data=rendite_kuka)
m_grach_bayer <- garchFit(formula = ~garch(1,1), data=rendite_bayer)
#Innovationsverteilungen und Spezifikationen der bedingten Varianz
m_snorm_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "snorm", data=rendite_kuka)
  #m_ged_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "ged", data=rendite_kuka)
  #m_sged_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "sged", data=rendite_kuka)
m_std_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "std", data=rendite_kuka)
m_sstd_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "sstd", data=rendite_kuka)
  #m_snig_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "snig", data=rendite_kuka)
m_qmle_garch_kuka <- garchFit(formula = ~ garch(1, 1), cond.dist = "QMLE", data=rendite_kuka)

######FÜR BAYER######
# Vergleich der neg. Log-Likelihoods der GARCH-Modelle
m_garch_kuka@fit$objective #6027
m_snorm_garch_kuka@fit$objective #6027
m_std_garch_kuka@fit$objective #5740
m_sstd_garch_kuka@fit$objective #5739
m_qmle_garch_kuka@fit$objective #6027
#t-Vtlg und standartisieret t-Vtlg bieten besten fit

#Graphische Veranschaulichung von Value at Risk Coverage (0.01) für norm_GARCH(1,1)
alpha <- .001
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

#For Schleife für Risk-Cover und BIC der GArchs
garchs <- c("norm", "snorm","std",
            "sstd", "QMLE")
val_at_risk_cov <- bic_garch <-as.numeric(vector(length = length(garchs)))
for (i in 1:length(garchs)){
alpha <- .001
garch <- garchFit(formula = ~ garch(1, 1), cond.dist = garchs[i], data=rendite_kuka, trace = FALSE)
intv <- coef(garch)[1] + data.frame(lower=qnorm(alpha/2)*garch@sigma.t, upper=qnorm(1-alpha/2)*garch@sigma.t)
which.upper <- which(rendite_kuka > intv$upper)
which.lower <- which(rendite_kuka < intv$lower)
length(which.upper) + length(which.lower)
val_at_risk_cov[i] <-(length(which.upper) + length(which.lower))/length(rendite_kuka)
bic_garch[i] <- garch@fit$ics["BIC"]
}
rm(val_at_risk_cov_kuka)
rm(bic_garch_kuka)
val_at_risk_cov_kuka <- rbind(garchs, round((val_at_risk_cov),4))
which.min(val_at_risk_cov)
val_at_risk_cov_kuka #std&sstd mit geringstem risk
bic_garch_kuka <- rbind(garchs, round((bic_garch),4))
which.max(bic_garch)
bic_garch_kuka #snorm mit niedrigstem bic

#####Aufgabe 2c#####
#Für Kuka Garch(1,1) mit snorm, da niedrigsten BIC
rendite_kuka <- rendite_kuka - mean(rendite_kuka)
orig <- 4000
m_kuka <- garchFit(formula = ~ garch(1, 1), data=rendite_kuka[1:orig], cond.dist="snorm")

coef(m_kuka)
predict(m_kuka)
#### FÜR BAYER#####

