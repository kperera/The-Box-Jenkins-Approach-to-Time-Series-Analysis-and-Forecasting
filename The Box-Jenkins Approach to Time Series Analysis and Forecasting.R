#Kenneth Perera
#MATH 194
#02/16/2020
#Forecasting Experiment

#Importing libraries
library(tseries)
library(forecast)
library(ggfortify)

#Getting data 
data <- ts(read.csv(file = 'data.csv')) #reading data from CSV

#Exploratory Data Analysis
par(mfrow=c(1,1))
ts.plot(data, main = 'Time Series Plot of Data', 
        xlab = 'Index') #Plotting the data
par(mfrow=c(1,2))
hist(data, freq=FALSE, main = "Histogram of Data") #histogram of data
lines(density(data), col="blue", lwd = 2) #density of data
qqnorm(data, pch = 1, frame = FALSE, main = "Normal Q-Q Plot of Data")
qqline(data, col = "blue", lwd = 2)

#Model Identification
par(mfrow=c(1,2))
acf(data, main = 'ACF Plot of Data') #acf of data
pacf(data, main = 'PACF Plot of Data') #pacf of data

differenced_data <- diff(data) #differencing to make data stationary

par(mfrow=c(1,1))
ts.plot(differenced_data, main = 'Time Series Plot of Differenced Data', 
        xlab = 'Index') #Plotting the differenced data

par(mfrow=c(1,2))
acf(differenced_data,
    main = 'ACF Plot of Differenced Data') #acf of differenced data
pacf(differenced_data,
     main = 'PACF Plot of Differenced Data') #pacf of differenced data
#dev.off()

#Model Estimation
arima_data <- auto.arima(data) #Fitting ARIMA Model
arima_data #ARIMA(0,1,0) Model

#Model Checking
arima_residuals <- residuals(arima_data) #Getting residuals
ggtsdiag(arima_data) #ploting residuals, acf of residuals ljung box statistic
par(mfrow=c(1,2))
hist(arima_residuals, freq = FALSE, main = "Histogram of Residuals") #histogram of residuals
lines(density(arima_residuals), col="blue", lwd = 2) #density of residuals
qqnorm(arima_residuals, pch = 1, frame = FALSE, main = "Normal Q-Q Plot of Residuals")
qqline(arima_residuals, col = "blue", lwd = 2)

#Forecasting
forecasted_data <- forecast(arima_data, h = 20, level = c(95)) #forecasting data
par(mfrow=c(1,1))
plot(forecasted_data, main = "Forecast of ARIMA(0,1,0)", 
       xlab = "Index", ylab = "Data") #plotting forecast