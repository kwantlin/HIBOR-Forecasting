#Assignment 2 - HIBOR Statistical Analysis

install.packages("readxl")
install.packages("xlsx")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("janitor")
install.packages("lmtest")
install.packages("forecast")
install.packages('forecast', dependencies = TRUE)
library("xlsx")
library("readxl")
library("gdata")
library("openxlsx")
library("ggplot2")
library("janitor")
library("lmtest")
library("forecast")

# Code Sources: https://digitalcommons.murraystate.edu/honorstheses/26/
# https://otexts.com/fpp2/regression-intro.html?fbclid=IwAR0UQTOc0z9zjbo_PQj6PMilMwF-qT7BkFTQkeneBw65Gh8WRJ-NOf4I4WI

###Overall time series - no predictors###

Hibor.data <- read.xlsx("Hibor Analysis.xlsx", sheet = 1); head(Hibor.data)
Hibor.data$DateH <- excel_numeric_to_date(Hibor.data$DateH); head(Hibor.data)
timeseries<-
  ts(Hibor.data$HIBOR, frequency = 1, start (1,1))
plot(timeseries) #time plot
acf(timeseries) #ACF plot - linear predictability of the series at time using only 
#values from previous times. Used to narrow down what process could be used to fit the data. 
pacf(timeseries) #PACF plot - partial ACF. Used to determine the "order" of the process.


## DIFFERENCING (order 1) to remove non-stationarity from the series
diffarima<-diff(timeseries,difference=1)
par(mfrow=c(2,1))
ts.plot(timeseries,type="o");ts.plot(diffarima,type="o")
abline(h=0)
acf(diffarima) #ACF of the differenced data
# PARAMETER ESTIMATION: Fit AR(1) model (with difference of 1) using ...
fit1<-arima(timeseries,order=c(1,1,0),method='ML') # maximum likelihood, preferred method
coef(fit1) #Coefficient
sqrt(diag(vcov(fit1))) #Standard Error
#Current equation is Y_t = 0.430532(Y_(t-1))+0.02788041
fit2<-arima(timeseries,order=c(1,1,0),method='CSS') #conditional least squares
fit3<-arima(timeseries,order=c(1,1,0),method='CSS-ML') #unconditional least squares
#shows if it is significant to zero
library(lmtest)
coeftest(fit1)

library(lmtest)
coeftest(fit2)

library(lmtest)
coeftest(fit3)

#MODEL DIAGNOSTICS
fit<-arima(timeseries, order=c(1,1,0),method="ML",include.mean=F)
#------------Residual diagnostic
tsdiag(fit)# ACF and Ljung-Box plot of the residuals
shapiro.test(resid(fit)) # Shapiro Wilk test for normality
qqnorm(resid(fit)) # QQ plot
qqline(resid(fit), col="dodgerblue")

#---- INFORMATION CRITERIA VALUES: Comparing AIC, AICCs, BIC
nobs<-length(fit)
k<-length((fit$coef))+1
aic<-fit$aic
aicc<-aic + (2*k*(k+1))/(nobs-k-1)
bic <-aic + k*(log(nobs)-2)
aic;aicc;bic
fit2<-arima(timeseries, order=c(1,0,0),method="ML",include.mean=F)
nobs<-length(fit2)
k<-length((fit2$coef))+1
aic<-fit2$aic
aicc<-aic + (2*k*(k+1))/(nobs-k-1)
bic <-aic + k*(log(nobs)-2)
aic;aicc;bic
fit3<-arima(timeseries, order=c(2,0,0),method="ML",include.mean=F)
nobs<-length(fit3)
k<-length((fit3$coef))+1
aic<-fit3$aic
aicc<-aic + (2*k*(k+1))/(nobs-k-1)
bic <-aic + k*(log(nobs)-2)
aic;aicc;bic
fit4<-arima(timeseries, order=c(1,0,1),method="ML",include.mean=F)
nobs<-length(fit4)
k<-length((fit4$coef))+1
aic<-fit4$aic
aicc<-aic + (2*k*(k+1))/(nobs-k-1)
bic <-aic + k*(log(nobs)-2)
aic;aicc;bic
fit5<-arima(timeseries, order=c(1,1,1),method="ML",include.mean=F)
nobs<-length(fit5)
k<-length((fit5$coef))+1
aic<-fit5$aic
aicc<-aic + (2*k*(k+1))/(nobs-k-1)
bic <-aic + k*(log(nobs)-2)
aic;aicc;bic
#Overall, ARIMA(1,1,1) is the best fit.

#FORECASTING
Hibor.data <- read.xlsx("Hibor Analysis.xlsx", sheet = 1); head(Hibor.data)
Hibor.data$DateH <- excel_numeric_to_date(Hibor.data$DateH); head(Hibor.data)
timeseries<-
  ts(Hibor.data$HIBOR, frequency = 1, start (1,1))
ar.fit<-arima(timeseries, order=c(1,1,1),method="ML",include.mean=F) # fitting ar(1)model with ML estimator
ar.pr<-predict(ar.fit,n.ahead=10) # Forecasting TS at 10 future time points
ar.pr # Predicted value at each future time point
# Constructing 95% upper and lower error bound arond the forecast
U<-ar.pr$pred+1.96*ar.pr$se
L<-ar.pr$pred-1.96*ar.pr$se
# plotting forecast values with given confidence interval along with the data
t<-1000:1100
par(mfrow=c(1,1))
plot(t,timeseries[t],type="o",ylab=" Simulated AR(2)
     Process")
lines(ar.pr$pred,col="red",type="o") #forecasted values
lines(U,col="blue",lty="dashed") #error bounds
lines(L,col="blue",lty="dashed") #error bounds
legend("bottomright", c("Simulated data", "Forecast", "Error Bounds (95% Confidence)"),
       col=c(1,2,4), lty=c(1,1,2))

###Dynamic Forecasting###

Hibor.data <- read.xlsx("Hibor Analysis.xlsx", sheet = 1); head(Hibor.data)
Hibor.data$DateH <- excel_numeric_to_date(Hibor.data$DateH); head(Hibor.data)
timeseries<-
  ts(Hibor.data$HIBOR, frequency = 1, start (1,1))
plot(timeseries) 
xreg <- cbind(ExchangeRate = Hibor.data$ExchangeRate,
              FedFunds = Hibor.data$FedFunds,
              LIBOR = Hibor.data$LIBOR,
              BalClose = Hibor.data$BalClose,
              HKM1 = Hibor.data$HKM1)
fit <- auto.arima(Hibor.data[,"HIBOR"], xreg = xreg)
checkresiduals(fit)
#####INPUT VALUES FOR FORECASTING PERIOD, THEN A COMMA, THEN THE NUMBER OF DAYS YOU WANT FOR YOUR FORECAST###
fcast <- forecast(fit, xreg = cbind(ExchangeRate=rep(7.8104,30), FedFunds=rep(2.38,30), LIBOR = rep(2.40413,30), BalClose = rep(54303,30), HKM1 = rep(1591.6,30)))
autoplot(fcast) + ylab("HIBOR Rate")

###Overall Multiple Linear Regression###
plot(Hibor.data) #Each column shows the relationships between the forecast variable and each of the predictors.
Hibor.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Hibor.data)
summary(Hibor.fit)
plot(Hibor.fit)
termplot(Hibor.fit)

cor(Hibor.data$HIBOR,Hibor.data$ExchangeRate)
cor(Hibor.data$HIBOR,Hibor.data$FedFunds)
cor(Hibor.data$HIBOR,Hibor.data$LIBOR)
cor(Hibor.data$HIBOR,Hibor.data$BalClose)
cor(Hibor.data$HIBOR,Hibor.data$HKM1)

#January

Jan.data <- read.xlsx("Hibor analysis.xlsx", sheet = 3); head(Jan.data)
Jan.data$DateH <- excel_numeric_to_date(Jan.data$DateH); head(Jan.data)

ggplot(Jan.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Jan.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Jan.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Jan.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Jan.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Jan.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Jan.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Jan.data)
summary(Jan.fit)

cor(Jan.data$HIBOR,Jan.data$ExchangeRate)
cor(Jan.data$HIBOR,Jan.data$FedFunds)
cor(Jan.data$HIBOR,Jan.data$LIBOR)
cor(Jan.data$HIBOR,Jan.data$BalClose)
cor(Jan.data$HIBOR,Jan.data$HKM1)

#February
Feb.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 4); head(Feb.data)
Feb.data$DateH <- excel_numeric_to_date(Feb.data$DateH); head(Feb.data)

ggplot(Feb.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Feb.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Feb.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Feb.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Feb.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Feb.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Feb.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Feb.data)
summary(Feb.fit)

cor(Feb.data$HIBOR,Feb.data$ExchangeRate)
cor(Feb.data$HIBOR,Feb.data$FedFunds)
cor(Feb.data$HIBOR,Feb.data$LIBOR)
cor(Feb.data$HIBOR,Feb.data$BalClose)
cor(Feb.data$HIBOR,Feb.data$HKM1)

#March
Mar.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 5); head(Mar.data)
Mar.data$DateH <- excel_numeric_to_date(Mar.data$DateH); head(Mar.data)

ggplot(Mar.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Mar.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Mar.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Mar.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Mar.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Mar.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Mar.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Mar.data)
summary(Mar.fit)

cor(Mar.data$HIBOR,Mar.data$ExchangeRate)
cor(Mar.data$HIBOR,Mar.data$FedFunds)
cor(Mar.data$HIBOR,Mar.data$LIBOR)
cor(Mar.data$HIBOR,Mar.data$BalClose)
cor(Mar.data$HIBOR,Mar.data$HKM1)

#April
Apr.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 6); head(Apr.data)
Apr.data$DateH <- excel_numeric_to_date(Apr.data$DateH); head(Apr.data)

ggplot(Apr.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Apr.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Apr.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Apr.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Apr.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Apr.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Apr.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Apr.data)
summary(Apr.fit)

cor(Apr.data$HIBOR,Apr.data$ExchangeRate)
cor(Apr.data$HIBOR,Apr.data$FedFunds)
cor(Apr.data$HIBOR,Apr.data$LIBOR)
cor(Apr.data$HIBOR,Apr.data$BalClose)
cor(Apr.data$HIBOR,Apr.data$HKM1)

#May

May.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 7); head(May.data)
May.data$DateH <- excel_numeric_to_date(May.data$DateH); head(May.data)

ggplot(May.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(May.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(May.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(May.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(May.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(May.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

May.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=May.data)
summary(May.fit)

cor(May.data$HIBOR,May.data$ExchangeRate)
cor(May.data$HIBOR,May.data$FedFunds)
cor(May.data$HIBOR,May.data$LIBOR)
cor(May.data$HIBOR,May.data$BalClose)
cor(May.data$HIBOR,May.data$HKM1)

#June

Jun.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 8); head(Jun.data)
Jun.data$DateH <- excel_numeric_to_date(Jun.data$DateH); head(Jun.data)

ggplot(Jun.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Jun.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Jun.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Jun.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Jun.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Jun.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Jun.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Jun.data)
summary(Jun.fit)

cor(Jun.data$HIBOR,Jun.data$ExchangeRate)
cor(Jun.data$HIBOR,Jun.data$FedFunds)
cor(Jun.data$HIBOR,Jun.data$LIBOR)
cor(Jun.data$HIBOR,Jun.data$BalClose)
cor(Jun.data$HIBOR,Jun.data$HKM1)

#July

Jul.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 9); head(Jul.data)
Jul.data$DateH <- excel_numeric_to_date(Jul.data$DateH); head(Jul.data)

ggplot(Jul.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Jul.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Jul.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Jul.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Jul.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Jul.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Jul.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Jul.data)
summary(Jul.fit)

cor(Jul.data$HIBOR,Jul.data$ExchangeRate)
cor(Jul.data$HIBOR,Jul.data$FedFunds)
cor(Jul.data$HIBOR,Jul.data$LIBOR)
cor(Jul.data$HIBOR,Jul.data$BalClose)
cor(Jul.data$HIBOR,Jul.data$HKM1)

#August

Aug.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 10); head(Aug.data)
Aug.data$DateH <- excel_numeric_to_date(Aug.data$DateH); head(Aug.data)

ggplot(Aug.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Aug.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Aug.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Aug.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Aug.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Aug.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Aug.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Aug.data)
summary(Aug.fit)

cor(Aug.data$HIBOR,Aug.data$ExchangeRate)
cor(Aug.data$HIBOR,Aug.data$FedFunds)
cor(Aug.data$HIBOR,Aug.data$LIBOR)
cor(Aug.data$HIBOR,Aug.data$BalClose)
cor(Aug.data$HIBOR,Aug.data$HKM1)

#September

Sep.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 11); head(Sep.data)
Sep.data$DateH <- excel_numeric_to_date(Sep.data$DateH); head(Sep.data)

ggplot(Sep.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Sep.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Sep.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Sep.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Sep.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Sep.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Sep.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Sep.data)
summary(Sep.fit)

cor(Sep.data$HIBOR,Sep.data$ExchangeRate)
cor(Sep.data$HIBOR,Sep.data$FedFunds)
cor(Sep.data$HIBOR,Sep.data$LIBOR)
cor(Sep.data$HIBOR,Sep.data$BalClose)
cor(Sep.data$HIBOR,Sep.data$HKM1)

#October

Oct.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 12); head(Oct.data)
Oct.data$DateH <- excel_numeric_to_date(Oct.data$DateH); head(Oct.data)

ggplot(Oct.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Oct.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Oct.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Oct.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Oct.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Oct.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Oct.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Oct.data)
summary(Oct.fit)

cor(Oct.data$HIBOR,Oct.data$ExchangeRate)
cor(Oct.data$HIBOR,Oct.data$FedFunds)
cor(Oct.data$HIBOR,Oct.data$LIBOR)
cor(Oct.data$HIBOR,Oct.data$BalClose)
cor(Oct.data$HIBOR,Oct.data$HKM1)

#November

Nov.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 13); head(Nov.data)
Nov.data$DateH <- excel_numeric_to_date(Nov.data$DateH); head(Nov.data)

ggplot(Nov.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Nov.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Nov.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Nov.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Nov.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Nov.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Nov.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Nov.data)
summary(Nov.fit)

cor(Nov.data$HIBOR,Nov.data$ExchangeRate)
cor(Nov.data$HIBOR,Nov.data$FedFunds)
cor(Nov.data$HIBOR,Nov.data$LIBOR)
cor(Nov.data$HIBOR,Nov.data$BalClose)
cor(Nov.data$HIBOR,Nov.data$HKM1)

#December

Dec.data <- read.xlsx("HIBOR analysis.xlsx", sheet = 14); head(Dec.data)
Dec.data$DateH <- excel_numeric_to_date(Dec.data$DateH); head(Dec.data)

ggplot(Dec.data, aes(DateH, HIBOR)) + geom_line() + xlab("") + ylab("Daily Hibor")
ggplot(Dec.data, aes(DateH, ExchangeRate)) + geom_line() + xlab("") + ylab("Exchange Rate")
ggplot(Dec.data, aes(DateH, FedFunds)) + geom_line() + xlab("") + ylab("Federal Funds Rate")
ggplot(Dec.data, aes(DateH, LIBOR)) + geom_line() + xlab("") + ylab("LIBOR")
ggplot(Dec.data, aes(DateH, BalClose)) + geom_line() + xlab("") + ylab("Aggregate Balance")
ggplot(Dec.data, aes(DateH, HKM1)) + geom_line() + xlab("") + ylab("HKM1")

Dec.fit = lm(HIBOR~ExchangeRate+FedFunds+LIBOR+BalClose+HKM1, data=Dec.data)
summary(Dec.fit)

cor(Dec.data$HIBOR,Dec.data$ExchangeRate)
cor(Dec.data$HIBOR,Dec.data$FedFunds)
cor(Dec.data$HIBOR,Dec.data$LIBOR)
cor(Dec.data$HIBOR,Dec.data$BalClose)
cor(Dec.data$HIBOR,Dec.data$HKM1)


#####################################



