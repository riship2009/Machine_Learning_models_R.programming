temperature = nottem

head(temperature,12)
tail(temperature,12)
str(temperature)
class(temperature)

# generating time-series with 12 observations per unit of time
time_series <- ts(temperature, start=c(1920,1), end=c(1939,12), frequency = 12) 
time_series
plot(time_series)

# decomposition of time-series into level, trend, seasonality, and noise components
temp_decom <- decompose(time_series) 
temp_decom_seasonal <- time_series - temp_decom$seasonal
plot(temp_decom)
plot(temp_decom_seasonal)

#DW Test
install.packages("lmtest")
library(lmtest)
model <- lm(time_series ~ temp_decom$seasonal)
summary(model)
dwtest(model)


# for ARIMA modelling
install.packages("forecast")
library(forecast)

# generatine ARIMA model
auto.arima(time_series) # to determine the order of differencing
reg <- arima(time_series,order=c(1,0,2),seasonal=c(1,1,2))	
reg

temp_forecast <- forecast(time_series, model=reg, h=8) # forecasting next 8 months	
temp_forecast

