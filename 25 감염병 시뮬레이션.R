#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr)
rdata <- list()

#### 가. 전세계 확진자와 사망자 등 ####
# 코로나19 데이터 주소가 바뀌었습니다.
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

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

# 날짜 및 나라별 완치자 수
rdata$recoveredCases <- read_csv(rdata$url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  group_by(Country, Variable) %>% summarise(Confirmed=sum(Recovered)) %>% 
  rename("Country"=1,"Date"=2,"Recovered"=3)


# 확진자, 사망자 합치기
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y"))

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
rdata$data %>% filter(Country=="Korea" & Date<=Sys.Date()-1)

#### 나. 나라별 인구수와 국가를 신뢰하는 정도 ####
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2019") %>% select(c(1,3))   # 인구는 2019년 기준
rdata$Trust <- read_csv("data/Trust.csv") # 인구는 2018년 기준

# 국가 이름 확인
setdiff(rdata$Trust$Country, rdata$Population$Country)

head(rdata$TrustP <- merge(rdata$Trust, rdata$Population, by='Country'))

setdiff(rdata$TrustP$Country, rdata$World$Country)
unique(rdata$World$Country)
rdata$Population$Country
rdata$Population %>% filter(Country=="Timor")

# 이름 일치시키기(페로, 홍콩, 팔레스타인 제외)
rdata$TrustP$Country <- gsub("South Korea", "Korea", rdata$TrustP$Country)
rdata$TrustP$Country <- gsub("United States", "US", rdata$TrustP$Country)
rdata$TrustP$Country <- gsub("Czech Republic", "Czechia", 
                             rdata$TrustP$Country)
rdata$TrustP$Country <- gsub("United Kingdom", "UK", rdata$TrustP$Country)
rdata$World$Country <- gsub("Bahamas, The", "Bahamas", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("Gambia, The", "Gambia", rdata$World$Country)
rdata$World$Country <- gsub("East Timor", "Timor", rdata$World$Country)


#### 다. 다 합치기 ####
# 검사자 수, 인구, 확진자, 사망자, 완치자 데이터 모두 합치기
head(rdata$data <- merge(rdata$TrustP, rdata$World, by='Country') %>% 
       arrange(Country, Date))

# 백만명당 확진자, 사망자 구하기
rdata$data <- rdata$data %>% mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeathsperM=Deaths*1000000/Population)

rdata$data %>% filter(Country=="Korea") %>% mutate(max=max(Confirmed))



#### 2. 우리나라 1일 확진자 수 ####
library(ggpubr)
data <- rdata$data %>% filter(Country=="Korea" & Date>"2020-02-17") %>% 
  mutate(Daily=Confirmed-Recovered)
ggline(data, x="Date", y="Daily", color="red", ylab="Number")

# 평균 15일 입원하면 퇴원한다고 가정한 경우 감염자 수
for(i in 2:(nrow(data))){
  if(i>15){
    data[i,10] <- data[i, 5]-data[i-15,5]
  }else{
    if(i==2) data[i-1,10] <- 0
    data[i,10] <- data[i, 5]
  }
}
plot(data$Date, data$Daily, xlab="Date", ylab="Number", type="l", lwd=2, col="red",
     main="감염자 수")



#### 3. SIR 모델 ####
library(EpiModel)
#### 가. 기본 ####
init <- init.dcm(s.num=1000, i.num=1, r.num=0)
# s.num : 감염에 민감한 사람수(9000명)
# i.num : 감염된 사람 수(1명)
# r.num : 회복된 사람 수(0명)

control <- control.dcm(type="SIR", nsteps=200)
# type : SIR(Susceptible-Infectious-Recovered)
# nsteps : 시뮬레이션 기간(100일)
# 사스는 면역력 8~10년, 메르스는 1~2년

param <- param.dcm(inf.prob=0.05, act.rate=10, rec.rate=1/20,
                   a.rate=0, ds.rate=0, di.rate=0, dr.rate=0)
# inf.prob(감염력 : 5%), act.rate(하루에 감염을 유발하는 행동의 횟수 : 10회)
# rec.rate(회복률 : 20일 후 회복), a.rate(인구의 유입: 5,200만명에 30만명 출생), 
# ds.rate(다른 이유로 사망 : 0%), di.rate(감염자의 격리), dr(완치자의 이탈)

mod <- dcm(param, init, control)
plot(mod, legend=F)
legend("topright", legend=c("취약자", "감염자", "회복자"), 
       col=c("blue", "red", "green"), lty=1,cex=1.0)



#### 나. 사회적 거리두기 ####
param <- param.dcm(inf.prob=0.05, act.rate=seq(10, 2, -2), rec.rate=1/20,
                   a.rate=0, ds.rate=0, di.rate=0, dr.rate=0)
mod <- dcm(param, init, control)

plot(mod, legend=F, ylim=c(0,800), main="사회적 거리두기", )
legend("topright", legend=c("act=10", "act=8", "act=6", 
                            "act=4", "act=2"), 
  col=c("red", "blue", "green", "violet", "orange"), lty=1,cex=1.0)


#### 다. 개인 위생 정도 ####
param <- param.dcm(inf.prob=seq(0.05, 0.01, -0.01), act.rate=10, rec.rate=1/20,
                   a.rate=0, ds.rate=0, di.rate=0, dr.rate=0)
mod <- dcm(param, init, control)

plot(mod, legend=F, ylim=c(0,800), main="마스크, 손씻기 등 개인 위생 정도")
legend("topright", legend=c("prob=0.05", "prob=0.04", "prob=0.03", 
                            "prob=0.02", "prob=0.01"), 
       col=c("red", "blue", "green", "violet", "orange"), lty=1,cex=1.0)


#### 라. 취약한 사람들의 유입 ####
param <- param.dcm(inf.prob=0.05, act.rate=10, rec.rate=1/20,
                   a.rate=seq(0.000, 0.008, 0.002), ds.rate=0, di.rate=0, dr.rate=0)
mod <- dcm(param, init, control)

plot(mod, legend=F, ylim=c(0,800), main="취약한 사람들의 유입")
legend("topright", legend=c("a.rate=0.000", "a.rate=0.002", "a.rate=0.004", 
                            "a.rate=0.006", "a.rate=0.008"), 
       col=c("red", "blue", "green", "violet", "orange"), lty=1,cex=1.0)

#### 마. 취약한 사람들의 유출 ####
param <- param.dcm(inf.prob=0.05, act.rate=10, rec.rate=1/20,
                   a.rate=0, ds.rate=seq(0.000, 0.04, 0.01), di.rate=0, dr.rate=0)
mod <- dcm(param, init, control)

plot(mod, legend=F, ylim=c(0,800), main="취약한 사람들의 유출")
legend("topright", legend=c("a.rate=0.00", "a.rate=0.01", "a.rate=0.02", 
                            "a.rate=0.03", "a.rate=0.04"), 
       col=c("red", "blue", "green", "violet", "orange" ), lty=1,cex=1.0)


#### 바. 감염자 격리 ####
param <- param.dcm(inf.prob=0.05, act.rate=10, rec.rate=1/20,
                   a.rate=0, ds.rate=0, di.rate=seq(0.000, 0.16, 0.04), dr.rate=0)
mod <- dcm(param, init, control)

plot(mod, legend=F, ylim=c(0,800), main="감염자 격리")
legend("topright", legend=c("di.rate=0.00", "di.rate=0.04", "di.rate=0.12", 
                            "di.rate=0.16", "di.rate=0.20"), 
       col=c("red", "blue", "green", "violet", "orange"), lty=1,cex=1.0)


##### 4. Italy ####
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
print(rdata$Italy <- Diff("Italy"))
#### _1) 데이터 합치기 ####
print(rdata$RItaly <- rdata$data %>% filter(Country=="Italy") %>% 
        select(c("Date", "Confirmed")) %>% 
        arrange(Date) %>% slice(c((nrow(.)-7):(nrow(.)))))
print(rdata$FItaly <- cbind(rdata$RItaly, rdata$Italy[1:8,]) %>% 
        mutate(Diff9=Confirmed-FC9,Diff7=Confirmed-FC7, Diff5=Confirmed-FC5, 
               Diff3=Confirmed-FC3))

##### _2) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GItaly <- rdata$FItaly %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GItaly, x="Date", y="value", color="variable", size=1.5,
       title="Italy", legend="right")
