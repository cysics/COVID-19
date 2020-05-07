#### 1. 데이터 전처리 ####
library(tidyverse); library(scales); library(forecast); library(stringr);
rdata <- list()

#### 가. 전세계 데이터(영국) ####
rdata$urlW <- c("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
rdata$World <- read_csv(rdata$urlW) %>% select(c(2:16)) %>% 
  rename("Country"=1, "Date"=2, "Confirmed"=3, "DailyC"=4, "Deaths"=5, "DailyD"=6,
         "ConfirmedperM"=7, "DailyCperM"=8, "DeathsperM"=9, "DailyDperM"=10,
         "TotalTests"=11, "DailyT"=12, "TestsperT"=13, "DailyTperT"=14, 
         "TestsU"=15)

max(rdata$World$Date)
unique(rdata$World$Country)
rdata$World$Country <- gsub("South Korea", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("United States", "US", rdata$World$Country)
rdata$World$Country <- gsub("United Kingdom", "UK", rdata$World$Country)


#### 2. 미국 예측 ####
library(lubridate)
print(data <- rdata$World %>% filter(Country=="US", Date>="2020-03-01") %>% select("DailyC") %>% ts())
autoplot(train <- window(data, end=c(nrow(data)-7)))
autoplot(test <- window(data, start=c(nrow(data)-6)))
print(arimax <- rdata$World %>% filter(Country=="US", Date>="2020-03-01") %>% select("DailyT") %>% ts())


#### 가. 미국 모델 성능평가 ####
h=7
Models <- list(
  ARIMA = auto.arima(train),
  ETS = ets(train),
  NAIVE = naive(train, h=h),
  NNETAR = nnetar(train),
  BATS = bats(train),
  TBATS = tbats(train)
)
Forecasts <- lapply(Models, forecast, h)

par(mfrow=c(2, 3))
for(f in names(Forecasts)){
  plot(Forecasts[[f]], main=f, ylim=c(0,50000))
  lines(test, col="red")
}
par(mfrow=c(1, 1))

acc <- Reduce(rbind, lapply(Forecasts, function(f){accuracy(f, test)[2, , drop=F]}))
row.names(acc) <- names(Forecasts)
round(acc[order(acc[,'RMSE']),], 1)


#### 나. 미국 최적모형 ####
#### _1) NNETAR (30번 평균) ####
NNETAR <- forecast(nnetar(train), h=h) %>% data.frame()
for(i in 1:29){NNETAR <- cbind(NNETAR, forecast(nnetar(train), h=h) %>% data.frame())}
NNETAR <- rowMeans(NNETAR) %>% ts(end=nrow(data))
autoplot(data)+autolayer(NNETAR, size=1) + ggtitle("Forecasts from NNETAR(2,2)") + theme(legend.position="none")

#### _2) NAIVE ####
autoplot(NAIVE <- forecast(naive(train, h=h)))
autoplot(data)+autolayer(forecast(naive(train, h=h)), PI=T, alpha=0.4, col="red") +
  ggtitle("Forecasts from Naive method") + theme(legend.position="none")

#### _3) 모형조합 ####
Combination <- (NNETAR+NAIVE[["mean"]])/2
autoplot(data)+ autolayer(Combination, size=1.5)+ ggtitle("Combination") + 
  theme(legend.position="none") + scale_y_continuous(labels=comma)+
  ylab("1일 확진자 수")

round(accuracy(Combination, data),1)
colMeans(data.frame(Combination))

#### 다. 미국 미래예측 ####
#### _1) NNETAR (30번 평균) ####
NNETAR <- forecast(nnetar(data), h=h) %>% data.frame()
for(i in 1:29){NNETAR <- cbind(NNETAR, forecast(nnetar(data), h=h) %>% data.frame())}
NNETAR <- rowMeans(NNETAR) %>% ts(end=nrow(data)+7)
autoplot(data)+autolayer(NNETAR, size=1) + ggtitle("Forecasts from NNETAR(2,2)") + theme(legend.position="none")

#### _2) NAIVE ####
autoplot(NAIVE <- forecast(naive(data, h=h)))
autoplot(data)+autolayer(forecast(naive(data, h=h)), PI=T, alpha=0.4, col="red") +
  ggtitle("Forecasts from Naive method") + theme(legend.position="none")

#### _3) 모형조합 ####
Combination <- (NNETAR+NAIVE[["mean"]])/2
autoplot(data)+ autolayer(Combination, size=1.5)+ ggtitle("Combination") + 
  theme(legend.position="none") + scale_y_continuous(labels=comma)+
  ylab("1일 확진자 수")

colMeans(data.frame(Combination))



#### 3. 일본 예측 ####
print(data <- rdata$World %>% filter(Country=="Japan", Date>="2020-03-01") %>% select("DailyC") %>% ts())
autoplot(train <- window(data, end=c(nrow(data)-7)))
autoplot(test <- window(data, start=c(nrow(data)-6)))


#### 가. 일본 모델 성능평가 ####
h=7
Models <- list(
  ARIMA = auto.arima(train),
  ETS = ets(train),
  NAIVE = naive(train, h=h),
  NNETAR = nnetar(train),
  BATS = bats(train),
  TBATS = tbats(train)
)
Forecasts <- lapply(Models, forecast, h)

par(mfrow=c(2, 3))
for(f in names(Forecasts)){
  plot(Forecasts[[f]], main=f, ylim=c(0,1200))
  lines(test, col="red")
}
par(mfrow=c(1, 1))

acc <- Reduce(rbind, lapply(Forecasts, function(f){accuracy(f, test)[2, , drop=F]}))
row.names(acc) <- names(Forecasts)
round(acc[order(acc[,'RMSE']),], 1)



#### 나. 일본 최적모형 ####
autoplot(ETS <- forecast(ets(train), h=h))
autoplot(data)+autolayer(forecast(ets(train), h=h), PI=T, alpha=0.4, col="red") +
  ggtitle("Forecasts from ETS(A,N,N)") + theme(legend.position="none") + scale_y_continuous(labels=comma)+
  ylab("1일 확진자 수")

colMeans(data.frame(ETS))

#### 다. 일본 미래예측 ####
autoplot(ETS <- forecast(ets(data), h=h))
autoplot(data)+autolayer(forecast(ets(data), h=h), PI=T, alpha=0.4, col="red") +
  ggtitle("Forecasts from ETS(A,N,N)") + theme(legend.position="none") + 
  scale_y_continuous(labels=comma) + ylab("1일 확진자 수")

colMeans(data.frame(ETS))


