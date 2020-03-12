#### 1. 데이터 불러오기 ####
library(corona19); library(dplyr)
data <- getdata("state") %>% arrange(date)

#### 2. 데이터 확인하기 ####
plot(data$confirmed)

#### 3. 비정상성, ARIMA ####
library(forecast)
auto.arima(data$test)
plot(forecast(arima(ts(data$test), order=c(2,2,0))))

auto.arima(data$confirmed)
forecast(arima(ts(data$confirmed), order=c(1,2,1)))
plot(forecast(arima(ts(data$confirmed), order=c(1,2,1))))

auto.arima(data$deceased)
forecast(arima(ts(data$deceased), order=c(1,2,1)))
plot(forecast(arima(ts(data$deceased), order=c(1,2,1))))


