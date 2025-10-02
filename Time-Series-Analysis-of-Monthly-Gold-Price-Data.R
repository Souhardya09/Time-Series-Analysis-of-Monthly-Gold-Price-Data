rm(list=ls())
library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)
library(randtests)

data=read.csv("C:/Users/SOUHARDYA/Documents/gold_monthly_csv.csv")
data
names(data)
head(data)

gold=ts(data$Price,start=1950,end=c(2020,7),frequency=12)
gold
attach(data)

plot(gold,ylab="Price of Gold(in USD)",xlab="time",main="Time series plot of Gold price",col="Red")

##Let us divide the dataset into training dataset and testing dataset

n=nrow(data)

train_size=floor(n*0.9)

##training dataset

train_data=data[1:train_size,]
gold_train=ts(train_data$Price,start=c(1950,1),end=c(2013,5),frequency=12)
plot(gold_train,ylab="Price of Gold(in USD)",xlab="time",main="Time series plot of Gold price",col="Black")

##testing dataset

test_data=data[(train_size+1):n,]
gold_test=ts(test_data$Price,start=c(2013,6),end=c(2020,7),frequency=12)
plot(gold_test,ylab="Price of Gold(in USD)",xlab="time",main="Time series plot of Gold price",col="Blue")

##checking stationarity of training dataset

adf.test(gold_train)##stationarity not present here
acf(gold_train)
pacf(gold_train)

##differencing the data

gold_train_diff1=diff(gold_train,differences = 1)
adf.test(gold_train_diff1)

##checking whether stationarity present or not

decomp=stl(gold_train,s.window="periodic")
plot(decomp)##plot suggesting seasonality is present

##Fitting ARIMA

fit_arima=auto.arima(gold_train,seasonal = FALSE)
fit_arima

##Fitting SARIMA

fit_sarima=auto.arima(gold_train,seasonal = TRUE)
fit_sarima

##Appropriate model choosen is ARIMA(0,1,2)(0,0,2)

model=auto.arima(gold_train,seasonal=TRUE)
summary(model)
fitted_values=fitted(model)
fitted_values

##plotting fitted values and  the actual values same at a time

plot(gold_train,col="Blue",xlab="Time",ylab="Gold price (in USD)",main="Actual gold price with fitted values")
lines(fitted_values,col="Red")
legend('topleft',legend=c("Actual","Fitted"),col=c("Blue","Red"),lty=1)

model.residual=residuals(model)
plot(model.residual,main='Plot of Model Residuals',xlab='Time',ylab='Model Residuals')

plot(x=fitted_values,y=model.residual,main='Residual vs Fitted values',xlab='Fitted Values',ylab='Model Residuals')
acf(model.residual,main='ACF plot for Model Residuals')
qqnorm(model.residual)
qqline(model.residual)

##Checking randomness of the residuals

runs.test(model.residual)##So the residuals are random

##Forecasting

forecast_data=forecast(model,h=length(gold_test))
forecast_data

predicted_value=ts(data.frame(forecast_data)[,1],start=c(2013,6),end=c(2020,7),frequency=12)
upperconfidence_90=ts(data.frame(forecast_data)[,5],start=c(2013,6),end=c(2020,7),frequency=12)
lowerconfidence_90=ts(data.frame(forecast_data)[,4],start=c(2013,6),end=c(2020,7),frequency=12)

plot(gold_test,col="orange",,ylim=c(0,2500),main='Plot of test data and forecasted values',xlab='Time',ylab='Gold Price(in USD)')
lines(predicted_value,col='green')
lines(upperconfidence_90,col='violet')
lines(lowerconfidence_90,col='blue')
legend('topright',legend=c('Original','Predicted','Upper Confidence Limit','Lower Confidence Limit'),col=c('orange','green','violet','blue'),lty=1)

##Error measure(MAPE)

et=gold_test-predicted_value
mape=sum(abs(et/gold_test)*100)/(length(gold_test))
mape

accuracy(forecast_data, gold_test)

