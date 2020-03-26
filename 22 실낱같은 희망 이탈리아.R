#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr)
rdata <- list()

#### 가. 전세계 확진자와 사망자 등 ####
# 코로나19 데이터 주소가 바뀌었습니다.
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# 날짜 및 나라별 확진자 수
rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  group_by(Country, Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

# 날짜 및 나라별 사망자 수
rdata$DeathCases <- read_csv(rdata$url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  group_by(Country, Variable) %>% summarise(Confirmed=sum(Deaths)) %>% 
  rename("Country"=1,"Date"=2,"Deaths"=3)


# 확진자, 사망자 합치기
rdata$World <- merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")) %>% mutate(Date=as.Date(.$Date, "%m/%d/%y"))

unique(rdata$World$Country)

# 타이완에 *표 없애기, 우리나라 Korea로 표현하기, 국가 이름 일치시키기
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("United Kingdom", "UK", rdata$World$Country)
rdata$World$Country <- gsub("United Arab Emirates", "A.Emirates", 
                            rdata$World$Country)

# 사망률 계산하기
head(rdata$data <- rdata$World %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  arrange(Country, Date))

max(rdata$data$Date)
rdata$data %>% filter(Country=="US" & Date<=Sys.Date()-1)

#### 2. 아리마모델 ####
library(forecast); library(ggpubr)
Diff <- function(country){
  for(Days in c(9, 7, 5, 3)){
    data <- rdata$data %>% 
            filter(Country==country & Confirmed>10 & Date<=Sys.Date()-Days) %>% 
            arrange(Date)
    
    ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
      str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
    
    rdata$data %>% filter(Country==country & Date==Sys.Date()-1) %>% 
      select(Confirmed) 
    forecast(arima(ts(data$Confirmed), 
      order=c(ARIMA[1],ARIMA[2],ARIMA[3])))$mean[Days-1]
    
    ARIMAF <- forecast(arima(ts(data$Confirmed), 
                             order=c(ARIMA[1],ARIMA[2],ARIMA[3])))
    if(Days==9){
      ARIMAF1 <- data.frame(ARIMAF$mean) %>% rename(FC9=1) %>% round(0)
    }else if(Days==7){
      ARIMAF2 <- data.frame(ARIMAF$mean) %>% rename(FC7=1) %>% round(0)
      ARIMAF2[3:10,] <- ARIMAF2[1:8,]
      ARIMAF2[1:2,] <- NA
    }else if(Days==5){
      ARIMAF3 <- data.frame(ARIMAF$mean) %>% rename(FC5=1) %>% round(0)
      ARIMAF3[5:10,] <- ARIMAF3[1:6,]
      ARIMAF3[1:4,] <- NA
    }else{
      ARIMAF4 <- data.frame(ARIMAF$mean) %>% rename(FC3=1) %>% round(0)
      ARIMAF4[7:10,] <- ARIMAF4[1:4,]
      ARIMAF4[1:6,] <- NA
    }
  }
  return(cbind(ARIMAF1, ARIMAF2, ARIMAF3, ARIMAF4))
}
print(rdata$US <- Diff("US"))
print(rdata$Spain <- Diff("Spain"))
print(rdata$France <- Diff("France"))
print(rdata$Germany <- Diff("Germany"))
print(rdata$Italy <- Diff("Italy"))
print(rdata$Korea <- Diff("Korea"))
print(rdata$Japan <- Diff("Japan"))

#### 가. US ####
#### _1) 데이터 합치기 ####
print(rdata$RUS <- rdata$data %>% filter(Country=="US") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FUS <- cbind(rdata$RUS, rdata$US[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) ARIMA 예측 ####
print(data <- rdata$data %>% filter(Country=="US" & Confirmed>100) %>% 
        select(c("Country", "Date", "Confirmed")) %>% arrange(Date))
ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
  str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
plot(forecast(arima(ts(data$Confirmed), order=c(ARIMA[1],ARIMA[2],ARIMA[3]))),
     main="Forecasting US")

##### _3) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GUS <- rdata$FUS %>% select(c(1:6)) %>% melt(id.var="Date") %>% 
        na.omit)
ggline(rdata$GUS, x="Date", y="value", color="variable", size=1.5,
       title="US", legend="right")



#### 나. Spain ####
#### _1) 데이터 합치기 ####
print(rdata$RSpain <- rdata$data %>% filter(Country=="Spain") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FSpain <- cbind(rdata$RSpain, rdata$Spain[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) ARIMA 예측 ####
print(data <- rdata$data %>% filter(Country=="Spain" & Confirmed>100) %>% 
        select(c("Country", "Date", "Confirmed")) %>% arrange(Date))
ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
  str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
plot(forecast(arima(ts(data$Confirmed), order=c(ARIMA[1],ARIMA[2],ARIMA[3]))),
     main="Forecasting Spain")

##### _3) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GSpain <- rdata$FSpain %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GSpain, x="Date", y="value", color="variable", size=1.5,
       title="Spain", legend="right")



#### 다. France ####
#### _1) 데이터 합치기 ####
print(rdata$RFrance <- rdata$data %>% filter(Country=="France") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FFrance <- cbind(rdata$RFrance, rdata$France[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) ARIMA 예측 ####
print(data <- rdata$data %>% filter(Country=="France" & Confirmed>100) %>% 
        select(c("Country", "Date", "Confirmed")) %>% arrange(Date))
ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
  str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
plot(forecast(arima(ts(data$Confirmed), order=c(ARIMA[1],ARIMA[2],ARIMA[3]))),
     main="Forecasting France")

##### _3) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GFrance <- rdata$FFrance %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GFrance, x="Date", y="value", color="variable", size=1.5,
       title="France", legend="right")



##### 라. Germany ####
#### _1) 데이터 합치기 ####
print(rdata$RGermany <- rdata$data %>% filter(Country=="Germany") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FGermany <- cbind(rdata$RGermany, rdata$Germany[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) ARIMA 예측 ####
print(data <- rdata$data %>% filter(Country=="Germany" & Confirmed>100) %>% 
        select(c("Country", "Date", "Confirmed")) %>% arrange(Date))
ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
  str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
plot(forecast(arima(ts(data$Confirmed), order=c(ARIMA[1],ARIMA[2],ARIMA[3]))),
     main="Forecasting Germany")

##### _3) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GGermany <- rdata$FGermany %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GGermany, x="Date", y="value", color="variable", size=1.5,
       title="Germany", legend="right")



##### 마. Italy ####
#### _1) 데이터 합치기 ####
print(rdata$RItaly <- rdata$data %>% filter(Country=="Italy") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FItaly <- cbind(rdata$RItaly, rdata$Italy[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) ARIMA 예측 ####
print(data <- rdata$data %>% filter(Country=="Italy" & Confirmed>100) %>% 
        select(c("Country", "Date", "Confirmed")) %>% arrange(Date))
ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
  str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
plot(forecast(arima(ts(data$Confirmed), order=c(ARIMA[1],ARIMA[2],ARIMA[3]))),
     main="Forecasting Italy")

##### _3) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GItaly <- rdata$FItaly %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GItaly, x="Date", y="value", color="variable", size=1.5,
       title="Italy", legend="right")


##### 바. Korea ####
#### _1) 데이터 합치기 ####
print(rdata$RKorea <- rdata$data %>% filter(Country=="Korea") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FKorea <- cbind(rdata$RKorea, rdata$Korea[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) ARIMA 예측 ####
print(data <- rdata$data %>% filter(Country=="Korea" & Confirmed>100) %>% 
        select(c("Country", "Date", "Confirmed")) %>% arrange(Date))
ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
  str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
plot(forecast(arima(ts(data$Confirmed), order=c(ARIMA[1],ARIMA[2],ARIMA[3]))),
     main="Forecasting Korea")



##### _3) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GKorea <- rdata$FKorea %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GKorea, x="Date", y="value", color="variable", size=1.5,
       title="Korea", legend="right")


#### 사. Japan ####
#### _1) 데이터 합치기 ####
print(rdata$RJapan <- rdata$data %>% filter(Country=="Japan") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FJapan <- cbind(rdata$RJapan, rdata$Japan[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) ARIMA 예측 ####
print(data <- rdata$data %>% filter(Country=="Japan" & Confirmed>100) %>% 
        select(c("Country", "Date", "Confirmed")) %>% arrange(Date))
ARIMA <- capture.output(auto.arima(data$Confirmed))[2] %>% 
  str_extract_all("\\d+") %>% unlist() %>% sapply(as.numeric)
plot(forecast(arima(ts(data$Confirmed), order=c(ARIMA[1],ARIMA[2],ARIMA[3]))),
     main="Forecasting Japan")

##### _3) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GJapan <- rdata$FJapan %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GJapan, x="Date", y="value", color="variable", size=1.5,
       title="Japan", legend="right")



