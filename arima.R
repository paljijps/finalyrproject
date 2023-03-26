rm(list = ls())
library(forecast)
library(readxl)

df = read_excel("Trenddata.xlsx")
df

df.ts = ts(df$Creg, frequency = 1, start = c(2002))
df.ts

arima_model = auto.arima(df.ts)
arima_model

arima_forecast = forecast(arima_model, 6)
arima_forecast

arima_accuracy = accuracy(arima_forecast)
arima_accuracy

plot(arima_forecast)

# H0: Residuals are white noise
Box.test(arima_model$residuals, type = "Ljung-Box")
