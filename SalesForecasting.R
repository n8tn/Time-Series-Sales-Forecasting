rm(list=ls())
library(ggplot2)
library(tseries)
library(forecast)
library(zoo)
library(strucchange)
library(rsconnect)
library(readr)
setwd("C:/Users/User/Documents/GitHub/Friesland")
Sales <- read_csv("pseudo epos UAE.csv")

Summary <- Sales
Summary$Date <- as.yearmon(paste(Summary$Month, Summary$Year), "%m %Y")
Sales$Date <- as.yearmon(paste(Sales$Month, Sales$Year), "%m %Y")
Sales <- Sales[-c(1:2)]

Sales <- ts(Sales[,1:3], start = c(2015,1), frequency = 12)
plot(Sales, xlab = "Years", ylab = "Total Sales in UAE")


#################################################################################
#################################################################################
##############                Model with Rainbow PPV              ###############
#################################################################################

## Testing
adf.test(Sales[,1], alternative = "stationary") #qty
adf.test(Sales[,3], alternative = "stationary") #ppv

adf.test(log(Sales[,1]), alternative = "stationary") #qty
adf.test(log(Sales[,3]), alternative = "stationary") #ppv
  
adf.test(diff(log(Sales[,1])), alternative = "stationary") #qty
adf.test(diff(log(Sales[,3])), alternative = "stationary") #ppv

adf.test(diff(diff(log(Sales[,1]))), alternative = "stationary") #qty
adf.test(diff(diff(log(Sales[,3]))), alternative = "stationary") #ppv

## ARIMA model: Rainbow Qty vs Rainbow PPV
ARIMAautofit <- auto.arima(log(Sales[,1]), d = 2, xreg = log(Sales[,3])) 
summary(ARIMAautofit)

## 1% increase in Rainbow PPV => 2.2% decrease in Rainbow Sales 

#Forecast
ppv <- log(rep.int(2.03, 5))
fc <- forecast(ARIMAautofit, n = 5, xreg = ppv, lambda = 0)
fc$x <- exp(fc$x)
plot(fc)
fc$mean

Forecast <- as.numeric(fc$mean)
Date <- rownames(as.data.frame(fc))
Forecasts <- data.frame(Date, Forecast)

rsconnect::deployApp("C:/Users/User/Documents/GitHub/Friesland")
#################################################################################
#################################################################################
##############              Model with All Covariates             ###############
#################################################################################

## Matrix of all covariates
cov = Salesx[,c(1,3,4)] 

ARIMAautofitxreg <- auto.arima(log(Salesx[,2]), d = 1, xreg = log(cov))
summary(ARIMAautofitxreg)                          

## 10% increase in Carnation Qty => 0.18% fall in Rainbow Sales 
## 10% increase in Rainbow PPV => 4.8% fall in Rainbow Sales 
## 10% increase in Carnation PPV => 3.8% fall in Rainbow Sales 




#RainbowCarnation$l <- lag(RainbowCarnation$rainbow_sales, n=1)
#plot(RainbowCarnation$rainbow_qty, RainbowCarnation$l, xlab = "Sales Quantity", ylab = "Total Sales Value")

##################################################################################
######################## Summary Stats 

Summary <- RainbowCarnation
Summary$rppv <- (RainbowCarnation$rainbow_sales/RainbowCarnation$rainbow_qty)
Summary$cppv <- (RainbowCarnation$carnation_sales/RainbowCarnation$carnation_qty)
Summary$date <- as.yearmon(paste(RainbowCarnation$Month, RainbowCarnation$Year), "%m %Y")
Summary <- Summary[-c(1,2,3)]

## Plots
#Sales Per Volume vs. Quantity Scatterplot (RAINBOW)
par(mfrow=c(1,2))
plot(Summary$rainbow_qty, Summary$rppv, main = "Sales Per Volume vs. Quantity 
     RAINBOW Scatterplot", 
     xlab = "Quantity (kgs)", ylab = "Price (per vol.)", pch=19)
abline(lm(Summary$rppv ~ Summary$rainbow_qty), col="red") # regression line (y~x) 
lines(lowess(Summary$rainbow_qty, Summary$rppv), col="blue") # lowess line (x,y)

#Sales vs. Quantity Scatterplot
plot(Summary$rainbow_qty, Summary$rainbow_sales, main = "Sales vs. Quantity 
     RAINBOW Scatterplot", 
     xlab = "Quantity (kgs)", ylab = "Sales (AED)", pch=19)
abline(lm(Summary$rainbow_sales ~ Summary$rainbow_qty), col="red") # regression line (y~x) 
lines(lowess(Summary$rainbow_qty, Summary$rainbow_sales), col="blue") # lowess line (x,y)

#Sales Per Volume vs. Quantity Scatterplot (CARNATION)
par(mfrow=c(1,2))
plot(Summary$carnation_qty, Summary$cppv, main = "Sales Per Volume vs. Quantity 
     CARNATION Scatterplot", 
     xlab = "Quantity (kgs)", ylab = "Price (per vol.)", pch=19)
abline(lm(Summary$cppv ~ Summary$carnation_qty), col="red") # regression line (y~x) 
lines(lowess(Summary$carnation_qty, Summary$cppv), col="blue") # lowess line (x,y)

#Sales vs. Quantity Scatterplot
plot(Summary$carnation_qty, Summary$carnation_sales, main = "Sales vs. Quantity 
     CARNATION Scatterplot", 
     xlab = "Quantity (kgs)", ylab = "Sales (AED)", pch=19)
abline(lm(Summary$carnation_sales ~ Summary$carnation_qty), col="red") # regression line (y~x) 
lines(lowess(Summary$carnation_qty, Summary$carnation_sales), col="blue") # lowess line (x,y)

#Carnation Sales Per Volume vs. Rainbow Quantity Scatterplot 
par(mfrow=c(1,2))
plot(Summary$rainbow_qty, Summary$cppv, main = "CARNATION Sales Per Volume vs. RAINBOW Quantity 
     Scatterplot", 
     xlab = "RAINBOW Quantity (kgs)", ylab = "CARNATION Price (per vol.)", pch=19)
abline(lm(Summary$cppv ~ Summary$rainbow_qty), col="red") # regression line (y~x) 
lines(lowess(Summary$rainbow_qty, Summary$cppv), col="blue") # lowess line (x,y)

#Carnation Sales vs. Rainbow Quantity Scatterplot
plot(Summary$rainbow_qty, Summary$carnation_sales, main = "CARNATION Sales vs. RAINBOW Quantity 
     Scatterplot", 
     xlab = "RAINBOW Quantity (kgs)", ylab = "CARNATION Sales (AED)", pch=19)
abline(lm(Summary$carnation_sales ~ Summary$rainbow_qty), col="red") # regression line (y~x) 
lines(lowess(Summary$rainbow_qty, Summary$carnation_sales), col="blue") # lowess line (x,y)

plot(Salesx, main = "Time-series for Sales and PPV (2015 - 2018)", las = 0, yaxt = 'n', col = "red")

#### Forecasts
dev.off()
rppv <- rep.int(3.94, 1)
cppv <- rep.int(4.18, 1)
carnation_qty <- rep.int(2446, 1)

fcov <- data.frame(carnation_qty, rppv, cppv)

pred <- predict(ARIMAautofitxreg, n.ahead = 1, newxreg = log(fcov))
pred$pred
fcast = exp(pred$pred)
fcast
plot(forecast(ARIMAautofitxreg, h = 1, xreg = log(fcov)))
  
####################################################################################
####################################################################################
####################################################################################

## Test and train
Sales.train <- window(Sales, end = c(2017,7)) #assign the first 80% of the data to the train set
Sales.test <- window(Sales, start = c(2017,8)) #assign the most recent 20% to the test set

## Testing
adf.test(Sales.train[,1], alternative = "stationary") #qty
adf.test(Sales.train[,3], alternative = "stationary") #ppv

adf.test(log(Sales.train[,1]), alternative = "stationary") #qty
adf.test(log(Sales.train[,3]), alternative = "stationary") #ppv

adf.test(diff(log(Sales.train[,1])), alternative = "stationary") #qty
adf.test(diff(log(Sales.train[,3])), alternative = "stationary") #ppv

adf.test(diff(diff(log(Sales.train[,1]))), alternative = "stationary") #qty
adf.test(diff(diff(log(Sales.train[,3]))), alternative = "stationary") #ppv

adf.test(diff(diff(Sales.train[,1])), alternative = "stationary") #qty
adf.test(diff(diff(Sales.train[,3])), alternative = "stationary") #ppv

ARIMAautofit.train <- auto.arima(log(Sales.train[,1]), d = 2, xreg = log(Sales.train[,3])) 
summary(ARIMAautofit.train)
fc <- forecast(ARIMAautofit.train, n = 6, xreg = log(Sales.test[,3]))

(-2688237.8)*mean(Sales.train[,3])/mean(Sales.train[,1])

#Compare Test-Train forecasts
accuracy(fc, Sales.test[,1])

plot(fc)
lines(Sales.test[,1])


######################################################################3
bp_ts <- breakpoints(Sales[,3] ~ 1)
summary(bp_ts)
ci_ts <- confint(bp_ts)
plot(Sales[,3])
lines(bp_ts)
lines(ci_ts)
lines(fitted(bp_ts, breaks = 2), col = 4)
lines(confint(bp_ts, breaks = 2))
fitted(bp_ts)[1]
fitted(bp_ts[breakpoints])


bp_ts <- breakpoints(Sales[,1] ~ 1)
summary(bp_ts)
ci_ts <- confint(bp_ts)
plot(Sales[,1])
lines(bp_ts)
lines(ci_ts)
