
install.packages("fredr")
##Libraries
library(dplyr)
library(xts)
library(tseries)
library(stats)
library(forecast)
library(fredr)

##importation des donnees 

fredr_set_key("d12020aa815dd3c82d419e68256020c4")
dat1<-fredr(series_id = "TRFVOLUSM227NFWA",
		observation_start = as.Date("1972-01-01"),
		observation_end = as.Date("2008-12-31"))
ST<-dat1$value
ST



dat2<-fredr(series_id = "BOST625URN",
		observation_start = as.Date("1992-01-01"),
		observation_end = as.Date("2019-12-31"))
ST2<-dat2$value
ST2


summary(ST)
summary(ST_b)
str(ST)
##Verifying the dataset ST
is.ts(ST)
ST_a <- ts(ST, start = c(1972, 1), frequency =12)
is.regular(ST_a)
##"ST" is regularly spaced, which means that the time intervals between observations are equal.
anyNA(ST_a)
##there are no missing values in "ST2".
ST_a
plot(decompose(ST_a))
##Verifying the dataset ST2
is.ts(ST2)
ST_b<- ts(ST2, start = c(1992, 1), frequency = 12)
is.regular(ST_b)
anyNA(ST_b)




class(ST_a)
class(ST_b)

frequency(ST_a)
frequency(ST_b)

##extract the monthly seasonal cycle of the time series 
cycle(ST_a)
cycle(ST_b)


summary(ST_a)
summary(ST_b)

plot(decompose(ST_a))
plot(decompose(ST_b))





# Visualiser la série temporelle
# Plot the raw data using the base plot function
plot(ST_a,xlab="Date", ylab = "Millions of Miles ",main="Millions of Miles travelled between 1972 and 2008")
plot(ST_b,xlab="Date", ylab = "Unemployment Rate ",main="Unemployment Rate in Boston-Cambridge-Nashua between 1992 and 2019")


# Boxplot
boxplot(ST_a~cycle(ST_a),xlab="Date", ylab = "Millions of Miles " ,main ="Millions of Miles travelled between 1972 and 2008")
boxplot(ST_b~cycle(ST_b),xlab="Date", ylab = "Unemployment Rate " ,main ="Unemployment Rate in Boston-Cambridge-Nashua between 1992 and 2019")


##Some information about the dataset
View(ST_a)
summary(ST_a)
frequency(ST_a)

View(ST_b)
summary(ST_b)
frequency(ST_b)


##Stationarity test
adf_test=adf.test(ST_a)
adf_test
#p-value = 0.01 <0.05 donc stationnaire

adf_test=adf.test(ST_b)
adf_test
#p-value = 0.6265> 0.05 donc non stationnaire


##Correlation de ST_a
acf_test=acf(ST_a[0:100], lag.max = 6, plot = FALSE)
acf_test
plot(acf_test)

pacf_test=pacf(ST_a[0:100], lag.max = 100)
pacf_test
plot(pacf_test)



acf_test=acf(ST_b, lag.max = 100, plot = FALSE)
acf_test
plot(acf_test)

pacf_test=pacf(ST_b, lag.max = 100)
pacf_test
plot(pacf_test)





#Etude de marche aléatoire
x <- cumsum(ST_a) #somme cumulative des bruits blancs
plot.ts(x)



#########################AR Model################################
############"        data1   #################
#Fitting the Model
AR=arima(ST_a, order = c(1,0,0))
#Results
AR
#Assassement
Box.test(AR$resid, lag=5, type="Ljung-Box")
#Plotting
AR_fit=ST_a- residuals(AR)
plot.ts(ST_a)
points(AR_fit, type = "l", col = "green", lty = 2)
#Accuracy with Test
AR_Pred=predict(AR,n.ahead=29)
AR_Pred$pred

accuracy(AR_Pred$pred,ST_a)

############"        data2   #################
#Fitting the Model
AR=arima(ST_b, order = c(1,0,0))
#Results
AR
#Assassement
Box.test(AR$resid, lag=5, type="Ljung-Box")
#Plotting
AR_fit=ST_b- residuals(AR)
plot.ts(ST_b)
points(AR_fit, type = "l", col = "green", lty = 2)
#Accuracy with Test
AR_Pred=predict(AR,n.ahead=29)
AR_Pred$pred

##accuracy(AR_Pred$pred,ST_a)



########################MA Model#######################################
###########        data1   ###############
#Fitting the model
MA=arima(ST_a, order=c(0,0,1))
#Results
MA
#Assassement
Box.test(MA$resid, lag=5, type="Ljung-Box")
#Plotting
MA_fit=ST_a - residuals(MA)

plot.ts(ST_a)
points(MA_fit, type = "l", col = "blue", lty = 2)
#Accuracy with Test
MA_Pred=predict(MA,n.ahead=29)
MA_Pred$pred
Test
accuracy(MA_Pred$pred,Test)

###########        data2   ###############
#Fitting the model
MA=arima(ST_b, order=c(0,0,1))
#Results
MA
#Assassement
Box.test(MA$resid, lag=5, type="Ljung-Box")
#Plotting
MA_fit=ST_b - residuals(MA)

plot.ts(ST_b)
points(MA_fit, type = "l", col = "blue", lty = 2)
#Accuracy with Test
MA_Pred=predict(MA,n.ahead=29)
MA_Pred$pred
#Test
#accuracy(MA_Pred$pred,Test)



##########################ARMA Model######################
#############     data1      ##############
#Fitting the model
ARMA=arima(ST_a,order=c(1,0,1))
#Results
ARMA
#Assassement
Box.test(ARMA$resid, lag=5, type="Ljung-Box")
#Plotting
ARMA_fit=ST_a-residuals(ARMA)
plot.ts(ST_a)
points(ARMA_fit, type = "l", col = "orange", lty = 2)
#Accuracy with Test
ARMA_Pred=predict(ARMA,n.ahead=29)
ARMA_Pred$pred
###Test
##accuracy(ARMA_Pred$pred,Test)

#############     data2      ##############
#Fitting the model
ARMA=arima(ST_b,order=c(1,0,1))
#Results
ARMA
#Assassement
Box.test(ARMA$resid, lag=5, type="Ljung-Box")
#Plotting
ARMA_fit=ST_b-residuals(ARMA)
plot.ts(ST_b)
points(ARMA_fit, type = "l", col = "orange", lty = 2)
#Accuracy with Test
ARMA_Pred=predict(ARMA,n.ahead=29)
ARMA_Pred$pred
###Test
##accuracy(ARMA_Pred$pred,Test)


#######################ARIMA Model#########################
############     data1        #############
#Fitting the model
ARIMA=arima(ST_a,order=c(1,1,1))
#Results
ARIMA
#Assassement
Box.test(ARIMA$resid, lag=5, type="Ljung-Box")
#Plotting
ARIMA_fit=ST_a-residuals(ARIMA)

plot.ts(ST_a)
points(ARIMA_fit, type = "l", col = "red", lty = 2)
#Accuracy with Test
ARIMA_Pred=predict(ARIMA,n.ahead=29)
ARIMA_Pred$pred


############     data2         #############
#Fitting the model
ARIMA=arima(ST_b,order=c(1,1,1))
#Results
ARIMA
#Assassement
Box.test(ARIMA$resid, lag=5, type="Ljung-Box")
#Plotting
ARIMA_fit=ST_b-residuals(ARIMA)

plot.ts(ST_b)
points(ARIMA_fit, type = "l", col = "red", lty = 2)
#Accuracy with Test
ARIMA_Pred=predict(ARIMA,n.ahead=29)
ARIMA_Pred$pred


