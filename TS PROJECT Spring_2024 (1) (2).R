
################################################################################
###Required packages
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")
install.packages("seastests")

library(tseries)
library(forecast)
library(lmtest)
library(seastests)

################################################################################

### Reloading Data

library(readxl)
sunspots <- read_excel("~/Desktop/sunspots.xlsx")
View(sunspots)

attach(sunspots)
dataa <- as.vector(meansunspots)

### Time Series Plot

dataa.to.plot <- ts(dataa,frequency = 12,start = c(2000,1))

ts.plot(dataa.to.plot, main= "TS of Sunspots", ylab = "Monthly Mean Total Sunspot Number", col="blue")
plot.ts(dataa.to.plot, main= "TS of ....",ylab = "Measure")

### Checking Seasonality

kw(dataa, freq = 12)# < 0.01 p-value suggest seasonality


### Checking Stationarity

acf(dataa, lag.max = 50)

pacf(dataa, lag.max = 30)  

### Transforming to Stationarity

trans_dataa <- sqrt(dataa)  #to make it stationary in variance
st_diff = diff(trans_dataa, differences=1) #in mean

ts.plot(trans_dataa,main=  "square root transformation")
ts.plot(st_diff,main=  "First difference")


acf(st_diff, main='ACF after stationarity satisfied')∑ # cuts off after 2nd lag
pacf(st_diff, main='PACF after stationarity satisfied') # cuts after 3st lag



#arima 112
# Specifying the Initial Model


Model1 = arima(trans_dataa,order = c(3,1,2), method = c("ML"))
summary (Model1)


Model2 = arima(trans_dataa,order = c(3,1,0), method = c("ML"))
summary (Model2)

Model3 = arima(trans_dataa,order = c(2,1,0), method = c("ML"))
summary (Model3)


# Diagnosis Tests

##1## the significance of the coefficients
coeftest(Model1)
coeftest(Model2)
coeftest(Model3)

##2## The randomness of the noise
Residuals = residuals(Model1)
acf(Residuals, main ="ACF for ARiMA (3,1,0)")
pacf(Residuals, main ="PACF for ARiMA (3,1,0)" )
Box.test(Residuals,type="Ljung",lag=10,fitdf=3)
Box.test(Residuals,type="Box-Pierce",lag=10,fitdf=3)  # fitdf = p+q
#insignificant (p-value<0.05)

Residuals = residuals(Model2)
acf(Residuals, main ="ACF for ARiMA (3,1,2)")
pacf(Residuals, main ="PACF for ARiMA (3,1,2)" )
Box.test(Residuals,type="Ljung",lag=10,fitdf=5)
Box.test(Residuals,type="Box-Pierce",lag=10,fitdf=5)


Residuals = residuals(Model3)
acf(Residuals, main ="Residuals ACF for ARiMA (2,1,0)")
pacf(Residuals, main ="Residuals PACF for ARiMA (2,1,0)" )
Box.test(Residuals,type="Ljung",lag=10,fitdf=2)
Box.test(Residuals,type="Box-Pierce",lag=10,fitdf=2)

##3## the accuracy measures
accuracy(Model1)      ## the accuracy measures: MAE, RMSE, MAPE
accuracy(Model2)  
accuracy(Model3)  
##4## the stationarity and invertibility.  chick ma condition
print(-0.3277+-0.3012) #-0.6289
print(-0.3012--0.3277). #0.0265
#-1<-0.3012<1
# Forecasting
forecast(Model3, h=20)
f=forecast(Model3, h = 20)
plot(f)

auto.arima(dataa,trace = T)

save.image("TS Project")
quit()
