#install.packages("fredr")
library(fredr)
library(TSstudio)
library(xts)

#importation des donnees 
fredr_set_key("d12020aa815dd3c82d419e68256020c4")

datab<-fredr(series_id = "BOST625URN",
		observation_start = as.Date("1992-01-01"),
		observation_end = as.Date("2019-12-31"))

datab

STB<- data.frame(Date=datab$date,valeur=datab$value)
str(STB)
View(STB)

# détectation et suppression des données manquantes
print(which(STB$Date==""))
print(which(STB$valeur==NA))

#rien

na.fail(STB) # pas de valeur manquantes


#TRANSFORMATION:


class(STB$Date)
#"Date" est de classe date
class(STB$valeur)
#"valeur" est de classe numeric


#détection de répetition de date pour des valeurs différentes:
duplicated_dates <- duplicated(STB$Date) | duplicated(STB$Date, fromLast = TRUE)
duplicated_rows <- STB[duplicated_dates,]

if (nrow(duplicated_rows) > 0) {
  cat("There are", nrow(duplicated_rows), "rows with duplicate dates but different values:")
  print(duplicated_rows)
} else {
  cat("No rows with duplicate dates and different values were found.")
}
#on a le message suivant affiché "No rows with duplicate dates and differe



#CREATION SERIE TEMPORELLE!

#on va utiliser xts ou zoo car on a déja une variable Date
# création xts

library(xts)
TSd=xts(STB$valeur,order.by=STB$Date)

ts_info(TSd)
anyNA(TSd)
boxplot(TSd,
  ylab = "unemployement rate",
  main = "Boxplot of unemployement rate "
)

#\n D'apres le boxplot, aucun outliers n'est detecte.
class(TSd)

plot(TSd)



##AGREGATION

TSd.year<-apply.yearly(TSd,FUN="mean")

plot(TSd.year,type="o",col="blue")

TSd.month<-apply.monthly(TSd,FUN="mean")

plot(TSd.month,type="o",col="red")

# extraction d'une partie
# entre 01/01/2014 --> 31/12/2019
Partdata=window(TSd,start="2014-01-01",end="2019-12-31")
plot(Partdata)
par(mfrow=c(2,1))
plot(TSd)
plot(Partdata)

#creation d une serie temporelle
ind.ts=ts(STB$valeur, start=c(1972,1), end=c(2008,12), frequency = 12)
plot(ind.ts)


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



par(mfrow=c(2,1))
plot(ind.ts)
lines(data_pred,col="red")
plot(ind.ts)
lines(data_pred1,col="green")


#La prévision est presque confondue avec la série réelle dans les deux courbes
#mais on peut dire que le modele additif décrit mieux notre série


#######"Exploration graphique des données:

plot(ind.ts, xlab="Time", ylab="Unemployment Rate in Boston-Cambridge-Nashua",
     main="",
     col="purple4",lwd=3)
plot(decompose(ind.ts))


#A partir de la représentation graphique des données à étudier, on remarque manque de tendance et d'une saisonnalité.
 Pour être sûr de ce qui a été observé, on doit examiner la fonction d'autocorrélation acf .

# détecter les points de repture
install.packages("strucchange")
library(strucchange)
tps=1:length(ind.ts)
# indexation des points de repture
PR.break=breakpoints(log(ind.ts)~tps)
# estimation linéaire
MPR=lm(log(ind.ts)~breakfactor(PR.break)*tps)
summary(MPR)
plot(log(ind.ts))
points(time(log(ind.ts)),fitted(MPR),type="l",col="red")
PR.break$breakpoints
install.packages("forecast")
library(forecast)
myts_adj <- tsoutliers(ind.ts, types="AO", idx=PR.break$breakpoints)

#cette comande ne marche pas 



acf(ind.ts,,lag=150,main="ACF of Unemployement Rate",lwd=2)

#L'acf a une décroissance lente ce qui confirme notre hypothèse de présence d'une tendance ainsi qu'un comportement
 répétitif amorti qui nous renseigne sur la présence d'une saisonnalité.


## Lissage par moyenne mobile
#order 1
M1=filter(ind.ts,rep(1,1),method="convolution")
M1
#order 2
M2 <- filter(ind.ts, rep(1/2, 2), method = "convolution")
M2
plot(ind.ts)
lines(M1,col="red")
lines(M2,col="blue")



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

###############################################################
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

adf.test(ind.res)
 #p-value = 0.01<0.05 donc stationnaire

#Il faut maintenant modéliser les données stationnaires résultantes par un modèle ARMA.
#Pour avoir une idée sur l'ordre du modèle AR et l'ordre du modèle MA, nous tracons l'acf 
#et le pacf des données résiduelles différencié

par(mfrow=c(1,2))
acf(ind.ts, 50)
pacf(ind.ts)


# 30 batton dans ACF et 1 dans PACF =>

#affiche le resultat du meilleur modele entre AR et MA
#model <- arima(data, order=c(p,d,q))

#ARMA(1,30):
M22<-arima(ind.ts,order=c(1,0,30))
AIC(M22)

#En particulier,vérifier si les résidus sont indépendants, s’ils ont une distribution normale et s’ils ont une variance constante.
checkresiduals(M22)

##Modèle ARIMA (AR Integrated MA)

adf.test(ind.ts)
#La pvalue >0,05 donc on accepte l'hypothèse alternative : serie non Stationnaire.

diff_ind <- diff(ind.ts)
M_arima <- arima(diff_ind, order = c(1,2,30))

# Résumé du modèle ARIMA ajusté
summary(M_arima)

# Tracer les résidus du modèle ARIMA ajusté
plot(M_arima$residuals, main = "Résidus ARIMA")


accuracy(M_arima)

M_arima
plot(ind.ts, main="Série temporelle et série ajustée")
lines(fitted(M_arima), col="red")
legend("topright", legend=c("Série temporelle", "Série ajustée"), col=c("black", "red"), lty=c(1,1))

