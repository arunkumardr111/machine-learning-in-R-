
# create and Print a Time Series  of monthly sales from Jan 2020

sales <- c(79,11,74, 86, 16,13, 34,63,49,18,68,45,99,86,78,42,98,58, 82, 71, 34, 56,  76, 39)

#create a time series object

sales.timeseries <- ts(sales, start = c(2012, 1), frequency = 12)
print (sales.timeseries)

#display start, end, and frequency of TS

start(sales.timeseries)

end(sales.timeseries)

start(sales.timeseries)

#plot TS 

plot(sales.timeseries, type = "o", pch = 19, col ="red")

#Creating a subset

sales.subset <- window(sales.timeseries, start = c(2012, 3), end = c(2012, 7))

plot(sales.subset, type = "o", pch = 19, col ="red", main = "Chart for subset of a Time Series",xlab="times")



### Multiple TS 

sales1 <- c(75, 18, 86, 13, 34, 63, 91, 85, 68, 55, 99, 86, 78, 42, 98, 58, 82, 81, 71, 56, 78, 90, 25, 48)
sales2 <- c(79, 11, 74, 86, 16, 13, 34, 63, 49, 18, 68, 45, 99, 86, 78, 42, 98, 58, 82, 71, 34, 56, 76, 50)

# combing multiple TS into matric
combined.sales <- matrix(c(sales1, sales2), nrow = 24)

#creatin TS object for the matrix
sales.timeseries <- ts(combined.sales, start = c(2012, 1), frequency = 12)

#plotting a chart from multiple TS

plot(sales.timeseries, main = "Multiple TS")

#SMOOTHING
#install and load forecast library


library(forecast)

#create a numerical vector of 48 values
newsales <- c( 79,11,74,86, 16, 13, 34, 63, 49, 18,68, 45, 99, 86, 78, 42, 98, 58, 82, 71, 34, 56, 76, 50, 75, 18, 86, 
               13, 34, 
              63, 91, 85, 68, 56, 99, 86, 73, 41, 98, 58, 82, 81, 71, 56,78 , 90, 25,42 )

# create a TS obj
newsales.timeseries <- ts(newsales, start = c(2010, 1), frequency = 6)

#Using par function to plot multiple images of one image
par(mfrow = c(3, 1))
plot(ma(newsales.timeseries, 1), type = "o", 
     pch = 19, col = "red", main = "Time Series with 
     k = 1")

plot(ma(newsales.timeseries, 3), type = "o", pch = 19, col = "blue", main = "Time Series with k = 1")

plot(ma(newsales.timeseries, 5), type = "o", pch = 19, col = "green", main = "Time Series with k = 1")

#Exponential Smoothing is used when we want a weighted average of existing
#TS values to make a short term prediction of future values
# ETS model(Exponential TS)
#syntax: ets(x , model = )
#x = name of dataset being UseMethod
# model has the model name.. we use AAA(Triple exponential smoothing model)
#Holt-Winters Exponential Smoothing, Accounts for  seasonality

#Use dataset AIrpassengers

AirPassengers
class(AirPassengers)
#determine frequency of air passengers dataset
frequency(AirPassengers)


# Using Cycle function
cycle(AirPassengers)
      
# Use aggregate() function to group data on a year basis
aggregate(AirPassengers)

#plotting a chart for airpassengers dataset
plot(AirPassengers)

hist(AirPassengers)
 
air <- log(AirPassengers)  #alog transformation
hist(air)
plot(air, main = "Log Transformed Chart for Air Passengers Data set" ,   col = "brown")

library(forecast)
plot(ma(AirPassengers,4))
#Model - The possible inputs are "N" for none, "A" for additive, 
#"M" for multiplicative, or "Z" for automatic selection. 
# The default value of ZZZ 
#ensures that all components are selected using the information criterion.
#- error , trend and seasonal

AirModel <- ets(air, model = "ZZZ")  

#plot AirModel
plot(AirModel)

# SEARSONAL DECOMPOSITION
## TS has seasonal componment,
#(such as monthly, quaterly or yearly), 
#cyclic component, trend and irregular
#single trend-cycle component, 
#and a remainder component 

#STL - Seasonal Decomposition of Time Series by Loess. 
#Loess is a method for estimating nonlinear relationships. 
#stl(ts, s.window = , t.window = )
#ts = ts object
#s.window = controls how fast seasonal effects can change over time, should be odd(7)
#t.windows = controls how fast trend   can change over time.
#Smaller values allow more rapid changes



fitair <- stl(air, s.window = "period" )
plot(fitair)
head(fitair$time.series, n = 4)


#using mothplot and seasonplot
monthplot(AirPassengers, month.labels = "TRUE", main = "Month wise chart")

#Using Arima model(Auto Regressive Integrated Moving Average )

fit <- auto.arima(air) 

# Next 5 forecasted values 
forecast(fit, 5) 

plot(forecast(fit, 5))

#plot on seasonbase
monthplot(AirPassengers, year.labels = "TRUE", choice = "seasonal", main = "Yearwise wise chart")

monthplot(AirPassengers, year.labels = "TRUE", choice = "trend", main = "Yearwise wise chart")


monthplot(AirPassengers, year.labels = "TRUE", choice = "remainder", main = "Yearwise wise chart")


#Use dataset JohnsonJohnson
library(forecast)

JohnsonJohnson
class(JohnsonJohnson)

start(JohnsonJohnson)
end(JohnsonJohnson)

#determine frequency of dataset
frequency(JohnsonJohnson)

# Using Cycle function
cycle(JohnsonJohnson)

par(mfrow = c(4,1))
plot(JohnsonJohnson,main = "time sireis without moving averege")

plot(ma(JohnsonJohnson,2),main = "time sireiswith moving averege k = 2")
plot(ma(JohnsonJohnson,4),main = "time sireiswith moving averege k = 4")
plot(ma(JohnsonJohnson,6),main = "time sireiswith moving averege k = 6")


#Plot the chart through seasonal decomposition
fitair_jj <- stl(JohnsonJohnson, s.window = "period" )
plot(fitair_jj, main = "seasonal decomposition")


#problem 2
library(zoo)

nottem
class(nottem)
frequency(nottem)
cycle(nottem)
start(nottem)
end(nottem)
aggregate(nottem)
# Create a plot for nottem dataset
plot(nottem)

#Create a time series object by subsetting the data for time rangestarting from Jan 1925 to Dec 1930 considering data of all 12months

new.timeseriesdata<-ts(nottem,start = c(1925,1),end = c(1930,12),frequency = 12)
new.timeseriesdata

par(mfrow = c(2,1))
plot(new.timeseriesdata , main = "time sireiswithout moving averege", col = "blue")
plot(ma(new.timeseriesdata,4),main = "time sireis moving averege k = 4", col = "blue")


#ARIMA model on this data and predict monthly temp for the next two years
library(forecast)
fit1 <- auto.arima(new.timeseriesdata) 
#forcasting for next 2 years
forecast(fit1,24)
plot(forecast(fit1,24))

#Predict the next 12 figures using ARIMA for nottem datase
fit_nottem<-auto.arima(nottem)
forecast(fit_nottem,12)
plot(forecast(fit_nottem,12))











