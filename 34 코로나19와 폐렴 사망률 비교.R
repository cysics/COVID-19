#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr);
rdata <- list()

#### 가. 전세계 데이터 #####
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")

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

max(rdata$World$Date)


##### 나. 나라별 인구수와 폐렴 사망자 수 합치기 ####
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2017") %>% select(c(1,3))   # 인구는 2019년 기준
rdata$Pneumonia <- read_csv("data/pneumonia.csv") %>% select(c(1,3:4)) %>% 
  rename(Country=1, Year=2, Pneumonia=3) 
setdiff(rdata$Population$Country, rdata$Pneumonia$Country)
rdata$Population <- merge(rdata$Pneumonia, rdata$Population) %>% 
  filter(Year=="2017") %>% select(c(1,3:4))

# 국가 이름 맞추기 
setdiff(rdata$World$Country, rdata$Population$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)
rdata$Population$Country <- gsub("South Korea", "Korea", rdata$Population$Country)
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$Population$Country <- gsub("United States", "US", rdata$Population$Country)

# 확진자 수를 기준으로 데이터 합치기
rdata$WorldP <- merge(rdata$World, rdata$Population) %>% arrange(Country, Date)
 

#### 다. 검사자 수 합치기 ####
# Sys.setlocale("LC_ALL", "english")
# rdata$TotalTests <- read_csv("data/full-list-total-tests-for-covid-19.csv") %>% 
#   select(c(1,3:4)) %>% rename(Country=1, Date=2, TotalTests=3) %>% 
#   mutate(Date=lubridate::mdy(Date))
# Sys.setlocale("LC_ALL", "korean")

# github 공개
rdata$TotalTests <- read_csv(rdata$url[4]) %>% 
  separate(Entity, sep=" ", into=c("Country", "X1", "X2")) %>% 
  mutate(Country=ifelse(!grepl("-", X1), paste(Country, X1), Country)) %>% 
  select(c(1, 4, 8:10)) %>% 
  rename("Country"=1, "Date"=2, "TotalTests"=3, "DailyT"=4, "TestsperT"=5)

# 나라 이름 맞추기
setdiff(rdata$TotalTests$Country, rdata$WorldP$Country)
rdata$TotalTests$Country <- gsub("South Korea", "Korea", rdata$TotalTests$Country)
rdata$TotalTests$Country <- gsub("United States", "US", rdata$TotalTests$Country)
setdiff(rdata$TotalTests$Country, rdata$WorldP$Country)

# 데이터 합치기
rdata$data <- merge(rdata$TotalTests, rdata$WorldP, all=T) %>% 
  arrange(Country, Date) 
# unique(rdata$data$Country)
rdata$data$Country <- gsub("United Kingdom", "UK", rdata$data$Country)
rdata$data$Country <- gsub("United Arab Emirates", "A.Emirates", rdata$data$Country)
rdata$data$Country <- gsub("Dominican Republic", "Dominica", rdata$data$Country)

# 각종 값 계산
rdata$data <- rdata$data %>% group_by(Country) %>% 
  mutate(DailyC=Confirmed-lag(Confirmed),
         DailyT=TotalTests-lag(TotalTests),
         DailyD=Deaths-lag(Deaths),
         DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
         ConfirmedperM=Confirmed/Population,
         ConfirmedperT=Confirmed/TotalTests,
         DeathsperM=Deaths/Population,
         DeathsperT=Deaths/TotalTests,
         DeathRateperT=DeathRate/TotalTests) %>% ungroup(Country)

rdata$data %>% filter(Country=="Japan") %>% arrange(desc(Date))
# 아직 데이터가 불완전해 보인다. 
# 일본의 4월 2일자 테스트는 전날에 비해 2건 증가했으나 확진자는 317명 증가ㅠㅠ


#### 2. 코로나19와 폐렴 사망률 비교 ####
rdata$data %>% filter(Country=="Japan") %>% arrange(desc(Date)) %>% 
  mutate(PRate=Pneumonia/100000) %>% select(c(1,6:10, ncol(.)))


Rnum <- rdata$data %>% filter(Country=="Korea") %>% arrange(desc(Date)) %>% 
  select("DailyC") %>% slice(1:7) %>% pull()
  
  mutate(PRate=Pneumonia/100000) %>% select(c(1,6:10, ncol(.)))



#### 2. 군집분석 ####
#### 가. 변수간 군집분석 ####
library(pheatmap); library(NbClust); library(factoextra)
data <- rdata$data %>% select(c("Confirmed", "Deaths", "Recovered", "DailyC", 
        "DailyD", "DeathRate", "ConfirmedperM", "DeathsperM", "TotalTests")) %>% 
  na.omit

pheatmap(cor(data, method="pearson"), cutree_rows=4, cutree_cols=4,
         display_numbers=T, fontsize=8, number_format="%.2f", number_color="grey0")

#### 나. 국가간 군집분석 ####
# 1000명 이상 확진자가 나온 국가의 수 확인
rdata$data %>% group_by(Country) %>% filter(Date<Sys.Date()) %>% 
  filter(Confirmed>=1000) %>% distinct(Country) %>% pull() %>% length()

# 국가별 검사자 데이터가 있는 날짜 수 확인
rdata$data %>% group_by(Country) %>% filter(!is.na(TotalTests)) %>% 
  summarise(Num=length(TotalTests))
# 국가별로 최근 데이터로 10개만 뽑아볼까?
head(data <- rdata$data %>% group_by(Country) %>% filter(!is.na(TotalTests)) %>% 
  filter(length(Date)>=10) %>% arrange(desc(Date)) %>% slice(1:10))
# 어떻게 사용해야할지... 나중에 써먹어 보자.

rdata$cutoff <-100
rdata$cutoff <-350
rdata$cutoff <-1000
data <- rdata$data %>% group_by(Country) %>% filter(!is.na(Confirmed)) %>% 
  filter(max(Confirmed)>=rdata$cutoff*4) %>% 
  filter(Confirmed>=rdata$cutoff & Confirmed<=rdata$cutoff*4) %>% 
  mutate(Days=max(Date)-min(Date)+1) %>% arrange(desc(Days)) %>% 
  mutate(Days=as.numeric(as.character(Days))) %>% 
  arrange(desc(Confirmed)) %>% slice(1) %>% 
  select(c("Country", "Days", "Confirmed", "Recovered", 
           "DeathRate", "ConfirmedperM")) %>% 
  ungroup() %>% column_to_rownames("Country") %>% 
  select(c("Days", "Confirmed", "Recovered", 
           "DeathRate", "ConfirmedperM")) %>% 
  na.omit %>% scale()

NbClust(scale(data), distance="euclidean", min.nc=2, max.nc=5, method="average")
NbClust(scale(data), distance="euclidean", min.nc=2, max.nc=6, method="average")
# 100(2), 500(2), 1000(5)

rdata$fit.hc <- data %>% scale() %>% dist(method="euclidean") %>% 
  hclust(method="ward.D2")

fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=2,  #cutoff=100, 500
          color_labels_by_k=T, rect=T)
fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=3,  #cutoff=100, 500
          color_labels_by_k=T, rect=T)
fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=4,  #cutoff=100, 500
          color_labels_by_k=T, rect=T)




#### 3. 개별국가 비교 ####
library(gganimate); library(ggpubr); library(scales)

data <- rdata$data %>% filter(Country=="Denmark" | Country=="Japan" |
                                Country=="Malaysia")
data$Country <- factor(data$Country, levels=c("Japan", "Malaysia", "Denmark"))

ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="TotalTests", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="TestsperT", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Deaths", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Recovered", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DeathRate", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.15, 0.7)) +
  scale_y_continuous(labels=comma)





