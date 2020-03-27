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

unique(rdata$data$Country)
rdata$data %>% filter(Country=="US") %>% mutate(max=max(Confirmed))

# 백만명당 확진자, 사망자 구하기
rdata$data <- rdata$data %>% mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeathsperM=Deaths*1000000/Population)



#### 2. 미국, 이탈리아, 스페인 ####
library(gganimate); library(scales)

#### 가. 100명 이상 된 날부터 비교 ####
print(Italy <- rdata$data %>% filter(Country=="Italy" & Date>"2020-02-22") %>% 
        arrange(Country, Date))
print(US <-rdata$data %>% filter(Country=="US" & Date>"2020-03-02") %>% 
        arrange(Country, Date))
print(Spain <-rdata$data %>% filter(Country=="Spain" & Date>"2020-03-01") %>% 
        arrange(Country, Date))

Italy$Date <- c(1:nrow(Italy))
US$Date <- c(1:nrow(US))
Spain$Date <- c(1:nrow(Spain))

# 미국의 전날 데이터가 없으면(NA로 표시되면) 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US)-1,], US[1:nrow(US)-1,])

# 미국의 전날 데이터가 있으면 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US),], US[1:nrow(US),], Spain[1:nrow(US),])

print(result <- ggplot(Line, aes(x=Date, y=Confirmed, color=Country)) + 
        scale_y_continuous(labels=comma) + theme_classic() + 
        geom_line(size=1.2) + geom_point(size=5) + 
        geom_segment(aes(xend=max(Date)+1, yend=Confirmed), linetype=2) +
        geom_text(aes(x=max(Date)+5, label=comma(Confirmed, accuracy=1)), size=7)+
        theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
              plot.margin=margin(10, 30, 10, 10)) +
        transition_reveal(Date) + view_follow(fixed_y=T) + 
        coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ISU1.gif"))

#### 나. 1000명을 초과한 날부터 비교 ####
print(Italy <- rdata$data %>% filter(Country=="Italy" & Date>"2020-02-28") %>%
        arrange(Country, Date))
print(US <-rdata$data %>% filter(Country=="US" & Date>"2020-03-10") %>% 
        arrange(Country, Date))
print(Spain <-rdata$data %>% filter(Country=="Spain" & Date>"2020-03-09") %>% 
        arrange(Country, Date))

Italy$Date <- c(1:nrow(Italy))
US$Date <- c(1:nrow(US))
Spain$Date <- c(1:nrow(Spain))

# 미국의 전날 데이터가 없으면(NA로 표시되면) 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US)-1,], US[1:nrow(US)-1,])

# 미국의 전날 데이터가 있으면 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US),], US[1:nrow(US),], Spain[1:nrow(US),])

print(result <- ggplot(Line, aes(x=Date, y=Confirmed, color=Country)) + 
        scale_y_continuous(labels=comma) + theme_classic() + 
        geom_line(size=1.2) + geom_point(size=5) + 
        geom_segment(aes(xend=max(Date)+1, yend=Confirmed), linetype=2) +
        geom_text(aes(x=max(Date)+3, label=comma(Confirmed, accuracy=1)), size=7)+
        theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
              plot.margin=margin(10, 30, 10, 10)) +
        transition_reveal(Date) + view_follow(fixed_y=T) + 
        coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ISU2.gif"))



#### 3. 아리마모델 ####
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

##### _2) 실제 확진자 수와 ARIMA 예측 ####
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

##### _2) 실제 확진자 수와 ARIMA 예측 ####
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

##### _2) 실제 확진자 수와 ARIMA 예측 ####
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

##### _2) 실제 확진자 수와 ARIMA 예측 ####
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

##### _2) 실제 확진자 수와 ARIMA 예측 ####
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

##### _2) 실제 확진자 수와 ARIMA 예측 ####
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

##### _2) 실제 확진자 수와 ARIMA 예측 ####
print(rdata$GJapan <- rdata$FJapan %>% select(c(1:6)) %>% 
        melt(id.var="Date") %>% na.omit)
ggline(rdata$GJapan, x="Date", y="value", color="variable", size=1.5,
       title="Japan", legend="right")

