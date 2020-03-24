#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr)
rdata <- list()

#### 가. 전세계 확진자와 사망자 등 ####
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

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

# 확진자, 사망자, 완치자 합치기
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y"))

# 타이완에 *표 없애기, 우리나라 Korea로 표현하기, 국가 이름 일치시키기
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)

# 사망률 계산하기
head(rdata$World <- rdata$World %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  arrange(Country, Date))

max(rdata$World$Date)
rdata$World %>% filter(Country=="US" & Date==Sys.Date()-1)

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
rdata$TrustP$Country <- gsub("East Timorc", "Timorc", rdata$TrustP$Country)
rdata$World$Country <- gsub("Bahamas, The", "Bahamas", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("Gambia, The", "Gambia", rdata$World$Country)
rdata$World$Country <- gsub("East Timor", "Timor", rdata$World$Country)


#### 다. 다 합치기 ####
# 검사자 수, 인구, 확진자, 사망자, 완치자 데이터 모두 합치기
head(data <- merge(rdata$TrustP, rdata$World, by='Country') %>% 
       arrange(Country, Date))

unique(data$Country)
data %>% filter(Country=="US") %>% mutate(max=max(Confirmed))

# 백만명당 확진자, 사망자 구하기
data <- data %>% mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeathsperM=Deaths*1000000/Population) 

data$Country <- gsub("United Kingdom", "UK", data$Country)



#### 2. 100명을 기준으로 10배 늘어나는데 걸리는 시간 ####
# 1000명 이상의 확진자를 낸 국가들 ####
data %>% group_by(Country) %>% filter(Date<Sys.Date()) %>% 
  filter(Confirmed>=1000) %>% distinct(Country) %>% pull() %>% length()

library(ggpubr)
cutoff <-100
Days <- data %>% group_by(Country) %>% filter(Date<Sys.Date()-1) %>% 
  filter(max(Confirmed)>=cutoff*10) %>% 
  filter(Confirmed>=cutoff & Confirmed<=cutoff*10) %>% 
  mutate(Days=max(Date)-min(Date)) %>% arrange(desc(Days)) %>% 
  mutate(Days=as.numeric(as.character(Days))) %>% 
  select(c("Country", "Trust", "Days")) %>% distinct()

shapiro.test(resid(lm(Days~Trust, Days)))

ggscatter(Days, x="Trust", y="Days", 
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="spearman", 
              label.x=15, label.y=20, label.sep = ",", col="Red"),
          label="Country", repel=T)



#### 3. 100만명당 확진자수 ####
Confirmed <- data %>% filter(Confirmed>1000 & Date==Sys.Date()-2)

shapiro.test(resid(lm(ConfirmedperM~Trust, Confirmed)))

ggscatter(Confirmed, x="Trust", y="ConfirmedperM", 
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="spearman", 
              label.x=15, label.y=800, label.sep = ",", col="Red"),
          label="Country", repel=T)



#### 4. 최근 날짜의 사망률 ####
print(DeathRate <- data %>% group_by(Country) %>% 
        filter(Confirmed>100, Date==Sys.Date()-2) %>% 
  arrange(desc(DeathRate)))

shapiro.test(resid(lm(DeathRate~Trust, DeathRate)))

ggscatter(DeathRate, x="Trust", y="DeathRate", 
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="spearman", 
              label.x=15, label.y=7, label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Italy", "Korea", "Iraq", "Indonesia",
                         "Algeria", "Iran", "Spain", "Philippines",
                         "UK", "Hungary", "France", "Netherlands",
                         "Japan", "Morocco", "Turkey", "Germany",
                         "Belgium", "Greece"))

ggscatter(DeathRate, x="ConfirmedperM", y="DeathRate", 
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="spearman", 
              label.x=1200, label.y=7.5, label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Italy", "Korea", "Iraq", "Indonesia",
                         "Algeria", "Iran", "Spain", "Philippines",
                         "UK", "Hungary", "France", "Netherlands",
                         "Japan", "Morocco", "Turkey", "Germany",
                         "Belgium", "Greece", "Switzerland",
                         "Luxembourg", "Iceland"))

