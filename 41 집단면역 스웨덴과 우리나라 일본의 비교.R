#### 1. 데이터 가져오기 ####
library(pins); library(tidyverse); library(reshape2)
library(tabulizer); library(webshot);

rdata <- df <- fit <- list()

#### 가. 전세계 데이터 가져오기(영국) ####
rdata$urlW <- c("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
rdata$World <- read.csv(rdata$urlW) %>% select(c(3:40)) %>%
  rename("Country"=1, "Date"=2, "Confirmed"=3, "DailyC"=5, "Deaths"=6, "DailyD"=8,
         "ConfirmedperM"=9, "DailyCperM"=11, "DeathsperM"=12, "DailyDperM"=14,
         "TotalTests"=16, "DailyT"=19, "TestsperT"=17, "DailyTperT"=20, 
         "PositiveR"=22, "Population"=25, "PopulationD"=26, "AgeMedian"=27, 
         "Age65"=28, "Age70"=29, "GDP"=30, "Poverty"=31, "CardiovascDR"=32,
         "DiabetesDR"=33, "SmokerF"=34, "SmokerM"=35, "Handwasing"=36,
         "BedperT"=37, "LifeE"=38) %>% 
  mutate(DeathRate=100*Deaths/Confirmed) %>% 
  mutate(DCperDT=100*DailyC/DailyT) %>% 
  mutate(DDperDC=100*DailyD/DailyC) %>% 
  mutate(Date=as.Date(Date))

max(rdata$World$Date)
unique(rdata$World$Country)
rdata$World$Country <- gsub("South Korea", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("United States", "US", rdata$World$Country)
rdata$World$Country <- gsub("United Kingdom", "UK", rdata$World$Country)

rdata$data <- rdata$World



#### 2. 데이터 비교 ####
library(gganimate); library(ggpubr); library(scales)

#### 가. 스웨덴 ####
data <- rdata$World %>% filter(Date>="2020-02-20") %>% 
  filter(Country=="Sweden" | Country=="Finland"| Country=="Norway"| 
           Country=="Denmark" | Country=="Japan" | Country=="Korea")
data$Country <- factor(data$Country, levels=c("Sweden", "Norway", "Denmark", 
                                              "Finland", "Japan", "Korea"))

# 누적확진자 수
ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="ConfirmedperM", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DailyC", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

# 일별 검사자 수
ggline(data, x="Date", y="DailyT", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.25, 0.7)) +
  scale_y_continuous(labels=comma)

# 천명당 일별 검사자 수
ggline(data, x="Date", y="DailyTperT", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.25, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DCperDT", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.5, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Deaths", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.25, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DeathsperM", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.25, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DeathRate", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.25, 0.63)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DailyD", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.75, 0.7)) +
  scale_y_continuous(labels=comma)



# 누적 검사자 수
ggline(data, x="Date", y="TotalTests", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.25, 0.7)) +
  scale_y_continuous(labels=comma)

# 천명당 누적 검사자 수
ggline(data, x="Date", y="TestsperT", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)




#### 나. 누적확진자 대비 1일 확진자 ####
head(data <- rdata$World %>% arrange(Country, Date) %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
              Daily=Confirmed-lag(Confirmed)) %>% filter(Date=="2020-04-02") %>% 
       mutate(Group=ifelse(Country=="Finland", "Finland", 
                    ifelse(Country=="Norway", "Norway",
                    ifelse(Country=="Japan", "Japan", 
                    ifelse(Country=="Denmark", "Denmark", 
                    ifelse(Country=="Sweden", "Sweden", 
                    ifelse(Country=="Korea", "Korea", "Others"))))))))
data$Group <- factor(data$Group, levels=c("Norway", "Denmark", "Japan", 
                                          "Sweden", "Finland", "Korea", "Others"))
ggscatter(data, x="Confirmed", y="Daily", color="Group", 
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "black", "grey"),
          label="Country", repel=T, 
          label.select=c("Norway", "Denmark", "Japan", "Sweden", "Finland", 
                         "Korea", "China", "US", "Italy", "Spain"))+
  scale_x_continuous(labels=comma, trans="log10", limits=c(100,300000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(10,50000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.005, 0.04, 0.3)), size=1,
              col=c("Black", "Black", "Black"), alpha=0.5,
              show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=200000, y=400, label="a=0.005", size=5, color="grey")+
  annotate("text", x=60000, y=1000, label="a=0.050", size=5, color="grey")+
  annotate("text", x=35000, y=30000, label="a=0.300", size=5, color="grey")

head(data <- rdata$World %>% arrange(Country, Date) %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
              Daily=Confirmed-lag(Confirmed)) %>% filter(Date==Sys.Date()-1) %>% 
       mutate(Group=ifelse(Country=="Finland", "Finland", 
                    ifelse(Country=="Norway", "Norway",
                    ifelse(Country=="Japan", "Japan", 
                    ifelse(Country=="Denmark", "Denmark", 
                    ifelse(Country=="Sweden", "Sweden",
                    ifelse(Country=="Korea", "Korea", "Others"))))))))
data$Group <- factor(data$Group, levels=c("Norway", "Denmark", "Japan", 
                                          "Sweden", "Finland", "Korea", "Others"))
ggscatter(data, x="Confirmed", y="Daily", color="Group", 
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "black", "grey"),
          label="Country", repel=T, 
          label.select=c("Norway", "Denmark", "Japan", "Sweden", "Finland", 
                         "Korea", "China", "US", "Italy", "Spain"))+
  scale_x_continuous(labels=comma, trans="log10", limits=c(100,300000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(10,50000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.005, 0.04, 0.3)), size=1,
              col=c("Black", "Black", "Black"), alpha=0.5,
              show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=200000, y=400, label="a=0.005", size=5, color="grey")+
  annotate("text", x=60000, y=1000, label="a=0.050", size=5, color="grey")+
  annotate("text", x=10000, y=10000, label="a=0.300", size=5, color="grey")



#### 3. 아리마 모델 (스웨덴) ####
library(forecast)
autoplot(ts(data %>% filter(Country=="Sweden") %>% select(DailyC)), 
         ylab="1일 확진자 수", title="스웨덴")
plot(forecast(auto.arima(ts(data %>% filter(Country=="Sweden") %>% select(Confirmed)))))
plot(forecast(auto.arima(ts(data %>% filter(Country=="Sweden") %>% select(DailyC)))))

plot(forecast(auto.arima(ts(data %>% filter(Country=="Korea") %>% select(Confirmed)))))
plot(forecast(auto.arima(ts(data %>% filter(Country=="Korea") %>% select(DailyC)))))


rdata$data <- rdata$World

Diff <- function(country){
  for(Days in c(9, 7, 5, 3)){
    data <- rdata$data %>% 
      filter(Country==country & Confirmed>10 & Date<=Sys.Date()-Days) %>% 
      arrange(Date)
    
    ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
      str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
    
    ARIMAF <- forecast(arima(ts(data$Confirmed), 
                             order=c(ARIMA[1],ARIMA[2],ARIMA[3])))
    if(Days==9){
      ARIMAF1 <- data.frame(ARIMAF$mean) %>% rename(FC9=1) %>% round(0)
      ARIMAF1 <- ARIMAF1[1:8,]
    }else if(Days==7){
      ARIMAF2 <- data.frame(ARIMAF$mean) %>% rename(FC7=1) %>% round(0)
      ARIMAF2[3:10,] <- ARIMAF2[1:8,]
      ARIMAF2[1:2,] <- NA
      ARIMAF2 <- ARIMAF2[1:8,]
    }else if(Days==5){
      ARIMAF3 <- data.frame(ARIMAF$mean) %>% rename(FC5=1) %>% round(0)
      ARIMAF3[5:10,] <- ARIMAF3[1:6,]
      ARIMAF3[1:4,] <- NA
      ARIMAF3 <- ARIMAF3[1:8,]
    }else{
      ARIMAF4 <- data.frame(ARIMAF$mean) %>% rename(FC3=1) %>% round(0)
      ARIMAF4[7:10,] <- ARIMAF4[1:4,]
      ARIMAF4[1:6,] <- NA
      ARIMAF4 <- ARIMAF4[1:8,]
    }
  }
  ARIMAR <- rdata$data %>% filter(Country==country) %>% 
    select("Date", "Confirmed") %>% arrange(Date) %>% 
    slice((nrow(.)-7):(nrow(.)))
  result <- cbind(ARIMAR, ARIMAF1, ARIMAF2, ARIMAF3, ARIMAF4) %>% 
    rename(Date=1, Confirmed=2, FC9=3, FC7=4, FC5=5, FC3=6) %>% 
    melt(id.var="Date") %>% na.omit
  return(result)
}
print(rdata$Sweden <- Diff("Sweden"))
print(rdata$Korea <- Diff("Korea"))
print(rdata$Japan <- Diff("Japan"))
print(rdata$US <- Diff("US"))
print(rdata$Spain <- Diff("Spain"))
print(rdata$France <- Diff("France"))
print(rdata$Germany <- Diff("Germany"))
print(rdata$Italy <- Diff("Italy"))


ggline(rdata$Sweden, x="Date", y="value", color="variable", size=1.5,
       title="Sweden", legend="right")+ scale_y_continuous(labels=comma)
ggline(rdata$Korea, x="Date", y="value", color="variable", size=1.5,
       title="Korea", legend="right")+ scale_y_continuous(labels=comma)
ggline(rdata$Japan, x="Date", y="value", color="variable", size=1.5,
       title="Japan", legend="right")+ scale_y_continuous(labels=comma)
ggline(rdata$US, x="Date", y="value", color="variable", size=1.5,
       title="US", legend="right")+ scale_y_continuous(labels=comma)
ggline(rdata$Spain, x="Date", y="value", color="variable", size=1.5,
       title="Spain", legend="right")+ scale_y_continuous(labels=comma)
ggline(rdata$France, x="Date", y="value", color="variable", size=1.5,
       title="France", legend="right")+ scale_y_continuous(labels=comma)
ggline(rdata$Germany, x="Date", y="value", color="variable", size=1.5,
       title="Germany", legend="right")+ scale_y_continuous(labels=comma)
ggline(rdata$Italy, x="Date", y="value", color="variable", size=1.5,
       title="Italy", legend="right")+ scale_y_continuous(labels=comma)


#### 4. 시계열 모델 비교 ####
print(data <- rdata$World %>% filter(Country=="Sweden" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="Korea" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="Japan" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="Italy" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="Germany" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="France" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="Spain" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="US" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="New Zealand" & Date>="2020-02-01"))
print(data <- rdata$World %>% filter(Country=="Taiwan" & Date>="2020-02-01"))

autoplot(ts(data$DailyC)) 

print(data <- ts(data$DailyC, start=c(2020, 2), frequency=365.25))

df$h=20
autoplot(train <- head(data, round(length(data)-df$h)))
autoplot(test <- tail(data, df$h))

df$Models <- list(
  ETS = ets(train),
  NAIVE = naive(train, h=df$h),
  ARIMA = auto.arima(train),
  NNETAR = nnetar(train),
  TBATS = tbats(train)
)
fit$Forecasts <- lapply(df$Models, forecast, df$h)

df$acc <- Reduce(rbind, lapply(fit$Forecasts, 
                               function(f){forecast::accuracy(f, test)[2, , drop=F]}))
rownames(df$acc) <- names(fit$Forecasts)
round(df$acc[order(df$acc[,'MAE']),], 1)

plot(fit$Forecasts[[1]]); lines(test, col="red")  # ETS
plot(fit$Forecasts[[2]]); lines(test, col="red")  # Naive
plot(fit$Forecasts[[3]]); lines(test, col="red")  # ARIMA
plot(fit$Forecasts[[4]]); lines(test, col="red")  # NNAR
plot(fit$Forecasts[[5]]); lines(test, col="red")  # TBATS

plot(forecast(ets(data), h=df$h))           # ETS
plot(forecast(naive(data, h=df$h), h=df$h)) # Naive
plot(forecast(auto.arima(data), h=df$h))    # ARIMA
plot(forecast(nnetar(data), h=df$h))        # NNETAR
plot(forecast(tbats(data), h=df$h))         # TBATS


library(keras); library(Metrics)
print(data <- rdata$World %>% filter(Country=="Korea" & Date>="2020-01-28"))
N.train <- 180
N.test <- 20
step <- 3
tail(data <- tail(data, step*2+N.test+N.train))

#정규화
msd <- c(mean(data[N.train+step+seq(N.test+step), ]$DailyC), 
         sd(data[N.train+step+seq(N.test+step), ]$DailyC))
tail(train <- data[seq(N.train+step), ] %>% mutate(DailyC=scale(DailyC)))
print(test <- data[N.train+step+seq(N.test+step), ] %>% mutate(DailyC=scale(DailyC)))
test$DailyC

tail(train.x <- array(data=lag(cbind(train$DailyC), step)[-(1:step), ], 
                      dim=c(nrow(train)-step, step, 1)))
tail(train.y <- array(data=train$DailyC[-(1:step)], dim=c(nrow(train)-step, 1)))

tail(test.x <- array(data=lag(cbind(test$DailyC), step)[-(1:step), ], 
                     dim=c(nrow(test)-step, step, 1)))
tail(test.y <- array(data=test$DailyC[-(1:step)], dim=c(nrow(test)-step, 1)))

model <- keras_model_sequential() %>% 
  layer_lstm(units=100, input_shape=c(step, 1), batch_size=20, 
             return_sequences=T, stateful=T) %>%
  layer_dropout(rate=0.2) %>%
  layer_lstm(units=50, return_sequences=F, stateful=T) %>%
  layer_dense(units=25) %>% 
  layer_dense(units=1) %>% 
  compile(loss='mse', optimizer='adam', metrics=list("mean_absolute_error"))

model %>% summary()

for(i in 1:N.train){
  model %>% fit(x=train.x, y=train.y, batch_size=20, epochs=5, shuffle=F, verbose=0)
  model %>% reset_states()
}

print(pred_out <- model %>% predict(test.x, batch_size=20) %>% as.tibble %>% 
        select(1) %>% as.matrix())
print(pred_out <- t(t(pred_out)*msd[2] + msd[1]))

mae(test.y, pred_out)

data %>% mutate(predy=c(rep(NA, nrow(data)-length(pred_out)), pred_out)) %>% 
  tail(.,(N.train+N.test+2)) %>% 
  ggplot(aes(x=Date, y=DailyC)) + geom_line() + 
  geom_line(aes(y=predy), color = "red")

library(timetk)
idx <- data %>% tk_index() %>% tk_make_future_timeseries(length_out=N.test)

tail(test.x1 <- array(data=tail(test$DailyC,20), dim=c(N.test, step, 1)))

print(pred_out1 <- model %>% predict(test.x1, batch_size=20) %>% as.tibble %>% 
        select(1) %>% as.matrix())
print(pred_out1 <- t(t(pred_out1)*msd[2] + msd[1]))
