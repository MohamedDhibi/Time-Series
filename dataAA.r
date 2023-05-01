##Libraries
library(dplyr)
library(xts)
library(tseries)
library(stats)
library(forecast)
library(fredr)

##importation des donnees 

fredr_set_key("d12020aa815dd3c82d419e68256020c4")
dataA<-fredr(series_id = "TRFVOLUSM227NFWA",
		observation_start = as.Date("1972-01-01"),
		observation_end = as.Date("2008-12-31"))
dataA


dataAA=data.frame(Date=dataA$date,valeur=dataA$value)
dataAA
str(dataAA)

View(dataAA)



##Préparation des données

# détectation et suppression des données manquantes
print(which(dataAA$date==""))
print(which(dataAA$value==NA))

na.fail(dataAA) # pas de valeur manquantes

##Les tranformations des variables
class(dataAA$Date)
#"Date" est de classe date
class(dataAA$valeur)
#"valeur" est de classe numeric



#détection de répetition de date pour des valeurs différentes:
duplicated_dates <- duplicated(dataAA$Date) | duplicated(dataAA$Date, fromLast = TRUE)
duplicated_rows <- dataAA[duplicated_dates,]

if (nrow(duplicated_rows) > 0) {
  cat("There are", nrow(duplicated_rows), "rows with duplicate dates but different values:")
  print(duplicated_rows)
} else {
  cat("No rows with duplicate dates and different values were found.")
}
#on a le message suivant affiché "No rows with duplicate dates and differe




#creation de notre serie temporelle

#on va utiliser xts ou zoo car on a déja une variable Date
# création xts
install.packages("xts")
library(xts)
STA=xts(dataAA$valeur,order.by=dataAA$Date)


##Verifying the dataset ST
is.regular(STA)
#"ST" is regularly spaced, which means that the time intervals between observations are equal.
anyNA(STA)
#there are no missing values in "STA".


# Visualiser la série temporelle
# Plot the raw data using the base plot function
plot(STA,xlab="Date", ylab = "Millions of Miles ",main="Millions of Miles travelled between 1972 and 2008")

class(STA)
#de classe "xts" ou "zoo"

#Agregation
STA.year<-apply.yearly(STA,FUN="mean")
plot(STA.year,type="o",col="blue")

STA.month<-apply.monthly(STA,FUN="mean")
plot(STA.month,type="o",col="red")

# extraction d'une partie
# entre 01/01/2000 --> 31/12/2005
Partdata=window(TSd,start="2000-01-01",end="2005-12-31")
plot(Partdata)
par(mfrow=c(2,1))
plot(STA)
plot(Partdata)

ind.ts=ts(dataAA$valeur,start=c(1972,1),end=c(2008,12),freq=12)
plot(ind.ts)

ind.ts



# boites à moustache par mois 
CM=cycle(ind.ts)
boxplot(ind.ts~CM)


###décomposition avec modele additif
data_AD=decompose(ind.ts,"additive")
plot(data_AD)
#Composante tendance
tend_data=data_AD$trend
#composante saisonniere
sais_data=data_AD$seasonal
#composante résiduelle
random_data=data_AD$random
#prévision
data_pred=tend_data+sais_data

###décomposition avec modele multiplicatif
data_MU=decompose(ind.ts,"multiplicative")
plot(data_MU)
#Composante tendance
tend1_data=data_MU$trend
#composante saisonniere
sais1_data=data_MU$seasonal
#composante résiduelle
random1_data=data_MU$random
#prévision
data_pred1=tend1_data*sais1_data



###presentation graphique des deux modeles ensemble
par(mfrow=c(2,1))
plot(ind.ts, main="Modele additif")
lines(data_pred,col="red")
plot(ind.ts , main="Modele multiplicatif")
lines(data_pred1,col="green")

#La prévision est presque confondue avec la série réelle dans les deux courbes
#mais on peut dire que le modele additif décrit mieux notre série

#Exploration graphique des données

plot(ind.ts,xlab="Date", ylab = "Millions of Miles ",
main="Millions of Miles travelled between 1972 and 2008",col="purple4",lwd=3)
#A partir de la représentation graphique des données à étudier, on remarque la 
#présence d'une tendance croissante et d'une saisonnalité. Pour être sûr de ce qui a
 #été observé, on doit examiner la fonction d'autocorrélation acf .

acf(ind.ts,,lag=150,main="ACF of Miles travelled ",lwd=2)

#L'acf a une décroissance lente ce qui confirme notre hypothèse de présence d'une tendance
 #ainsi qu'un comportement répétitif amorti qui nous renseigne sur la présence d'une
 #saisonnalité.



########################################################################################
########################################################################################

#II-Ajustement de la série temporelle

#Modélisation des composants déterministes : la composant tendance et la saisonnalité 
#La composante tendance
#La question qui se pose à ce niveau est : quel modèle choisir pour la tendance ?
plot(decompose(ind.ts,type=c("additive")))

#Le resultat de la fonction de décomposition par rapport à la tendance (via une moyenne mobile) 
#nous conduit à choisir un modèle polynomial d'ordre 1 ou 2

####Lissage par moyenne mobile
#Le lissage par moyenne mobile d'ordre 1 est un moyen simple de réduire le bruit dans une 
#série temporelle en remplaçant chaque valeur de la série par la moyenne de cette valeur
#et de sa valeur voisine précédente ou suivante. Dans ce cas, la moyenne 
#mobile d'ordre 1 calcule une moyenne des deux valeurs voisines de chaque point de la série.

#order 1

M1 <- stats::filter(ind.ts, rep(1,1), method="convolution")
M1


#order 2
M2<- stats::filter(ind.ts, rep(1/2,1), method="convolution")

M2

plot(ind.ts)
lines(M1,col="red")
lines(M2,col="blue")
# on chousit l'ordre 1


#### Ajustement linéaire de la tendance
# Y=at+b
t=1:length(ind.ts)
M1=lm(ind.ts~t)
coef(M1) #coef de regression estime
summary(M1)

# Ajustement ployn
# Y=a*t^2+b*t+c
M2=lm(ind.ts~t+I(t^2))

summary(M2)
# Y=d*t^3+a*t^2+b*t+c
M3=lm(ind.ts~t+I(t^2)+I(t^3))
summary(M3)

# Amélioration de l'ajustement du modèle
plot(ind.ts)
points(time(ind.ts),fitted(M1),type="l",col="red")
points(time(ind.ts),fitted(M2),type="l",col="green")
points(time(ind.ts),fitted(M3),type="l",col="blue")


#La composante saisonnière

# Détermination de la variable descriptive liée à la composante saisonnière


#on remarque une saisonalite, on pense a ajuster la composante saisonniere , on utilise la variable dummy
# library(forecast)
# # une matrice composée de toutes les variables dummy
# DV<-seasonaldummy(ind.ts) 


# S=sum_(T/2)(beta_i*cos(2*pi*i*t/T)+sum_(T/2)(alpha_i*sin(2*pi*i*t/T)
CS<-matrix(0,length(ind.ts),6)
SN<-matrix(0,length(ind.ts),6)
for (i in 1:6) CS[,i]<-cos(2*pi*i*t/12)
for (i in 1:6) SN[,i]<-sin(2*pi*i*t/12) 

# The code defines a variable t as a sequence of integers from 1 to the length of a time series ind.ts. 
# The purpose of this variable is to represent time or the index of observations in the time series.
t<-c(1:length(ind.ts))
#standardized version of t called s.t
s.t<-(t-mean(t))/sd(t) # # cette variable est introduite afin de guarantir un 
                       #même ordre de grandeur des différentes variables descritptives


#Premier modèle 
#The code fits a linear regression model to the time series ind.ts with s.t, CS, and SN as predictors.
Model1<-lm(ind.ts~s.t+CS+SN)
summary(Model1)

##deuxieme modèle
# Y=a*t^2+b*t+c
Model2<-lm(ind.ts~s.t+I(s.t^2)+CS+SN)
summary(Model2)

##troisieme modèle
# Y=d*t^3+a*t^2+b*t+c
Model3<-lm(ind.ts~s.t+I(s.t^2)+I(s.t^3)+CS+SN)
summary(Model3)

#Comparaison des modèles

AIC(Model1)
AIC(Model2)
AIC(Model3)
# The lowest value is given by model 3
BIC(Model1)
BIC(Model2)
BIC(Model3)
# The lowest value is given by model 3


#Représentation graphique du modèle ajusté

plot(ind.ts,xlab="Date", ylab = "Millions of Miles ",main="Millions of Miles travelled between 1972 and 2008",
 col="purple4",lwd=2)


points(time(ind.ts),fitted(Model3),lwd=2,type="l",col="red")
legend("topleft", c("Original TS","Estimated TS"),inset = .02,
 bg = "gray90",lwd=c(2,2),col=c("purple","red"))


#################################""
#III-Analyse et modélisation des résidus issus de l’ajustement
#Représentation graphique de la composante résiduelle

ind.res<-Model3$residuals
plot(time(ind.ts),ind.res, xlab="Time", ylab="",
 main="Residual component",
 col="blue4",lwd=2,type="l")



#H0: seérie non stationnaire
#H1: série stationnaire

adf.test(ind.res)
 #p-value = 0.01<0.05 donc stationnaire

#Il faut maintenant modéliser les données stationnaires résultantes par un modèle ARMA.
#Pour avoir une idée sur l'ordre du modèle AR et l'ordre du modèle MA, nous tracons l'acf 
#et le pacf des données résiduelles différencié

par(mfrow=c(1,2))
acf(ind.res)
pacf(ind.res)

# on Compte le nombre de pics significatifs dans la fonction ACF pour avoir une indication
# approximative du nombre de termes autorégressifs à inclure dans le modèle.

# on Utilise également la fonction PACF pour confirmer l’ordre optimal du modèle AR(p).
#on a 10 pics ,l'ordre d'AR sera 15 donc on peut tester les 15
  #p=1
  M1=arima(ind.res,order=c(1,0,0))
  AIC(M1)
  #p=2
  #AR(2)
  M2=arima(ind.res,order=c(2,0,0))
  AIC(M2)
  #p=3
  #AR(3)
  M3=arima(ind.res,order=c(3,0,0))
  AIC(M3)  
  
  #p=4
  #AR(4)
  M4=arima(ind.res,order=c(4,0,0))
  AIC(M4) 
 #p=5
  #AR(5)
  M5=arima(ind.res,order=c(5,0,0))
  AIC(M5) 
 #p=6
  #AR(6)
  M6=arima(ind.res,order=c(6,0,0))
  AIC(M6) 
 #p=7
  #AR(7)
  M7=arima(ind.res,order=c(7,0,0))
 AIC(M7) 
 #p=7
  #AR(8)
  M8=arima(ind.res,order=c(8,0,0))
  AIC(M8) 
 #p=9
  #AR(9)
  M9=arima(ind.res,order=c(9,0,0))
  AIC(M9) 
 #p=10
  #AR(10)
  M10=arima(ind.res,order=c(10,0,0))
  AIC(M10) 
  #AR(11)
  M11=arima(ind.res,order=c(11,0,0))
  AIC(M11) 
  #AR(12)
  M12=arima(ind.res,order=c(12,0,0))
  AIC(M12) 
  #AR(13)
  M13=arima(ind.res,order=c(13,0,0))
  AIC(M13) 
  #AR(14)
  M14=arima(ind.res,order=c(14,0,0))
  AIC(M14) 
  #AR(15)
  M15=arima(ind.res,order=c(15,0,0))
  AIC(M15) 

#M115 est le mailleur , (AIC plus faible)

#L’ordre du modèle MA est le nombre de retards associé au coude de la PACF. Autrement dit, 
l’ordre est le nombre de termes MA nécessaires pour que la PACF retombe à zéro.
 
#diminution rapide pour le 2eme décalage
#on a 2 pics ,l'ordre d'AR sera 2, on peut tester les 2
#q=1
  AM1=arima(ind.res,order=c(0,0,1))
  AIC(AM1)
  #q=2
  #AR(2)
  AM2=arima(ind.res,order=c(0,0,2))
  AIC(AM2)
 
 #AM2 est le mailleur , (AIC plus faible)


#affiche le resultat du meilleur modele entre AR et MA
#model <- arima(data, order=c(p,d,q))

#ARMA(2,15):
M22<-arima(ind.res,order=c(2,0,15))

M22<-arima(ind.res,order=c(2,0,15))
AIC(M22)


# Afficher les paramètres du modèle
M22$coef



library(forecast)
#vérifier si les résidus du modèle satisfont aux conditions de stationnarité. 
#En particulier,vérifier si les résidus sont indépendants, s’ils ont une distribution normale et s’ils ont une variance constante.
checkresiduals(M22)

##Modèle ARIMA (AR Integrated MA)

adf.test(ind.ts)
#La pvalue <0,05 donc on accepte l'hypothèse alternative : serie Stationnaire.

diff_ind <- diff(ind.ts)
M_arima <- arima(diff_ind, order = c(2,1,15))

# Résumé du modèle ARIMA ajusté
summary(M_arima)

# Tracer les résidus du modèle ARIMA ajusté
plot(M_arima$residuals, main = "Résidus ARIMA")


# "ME" : erreur moyenne, c'est-à-dire la moyenne des écarts entre les prévisions et les valeurs réelles
# "RMSE" : racine carrée de l'erreur quadratique moyenne (MSE), qui mesure la précision des prévisions
# "MAE" : erreur absolue moyenne, c'est-à-dire la moyenne des valeurs absolues des écarts entre les prévisions et les valeurs réelles
# "MAPE" : erreur de pourcentage absolue moyenne, qui mesure l'erreur moyenne en pourcentage des prévisions par rapport aux valeurs réelles
# "MASE" : erreur d'échelle moyenne moyenne, qui mesure l'erreur des prévisions par rapport à une méthode de prévision naïve basée sur la moyenne mobile
# "ACF1" : premier coefficient d'autocorrélation de la série des résidus
# "RMSE^2" : carré de la racine carrée de l'erreur quadratique moyenne (MSE), qui est équivalent à la variance des résidus



library(forecast)
accuracy(M_arima)

M_arima
plot(ind.ts, main="Série temporelle et série ajustée")
lines(fitted(M_arima), col="red")
legend("topright", legend=c("Série temporelle", "Série ajustée"), col=c("black", "red"), lty=c(1,1))




#finalement, nous pouvons utiliser le modèle ARIMA pour faire des prévisions futures :

# prévisions futures
fc <- forecast(M_arima, h = 3)

# visualisation des prévisions
plot(fc)





