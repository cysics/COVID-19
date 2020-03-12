#### 1. 데이터 전처리 ####
#### 가. 신종플루 데이터 ####
rdata <- list()
rdata$H1N1 <- data.frame(Country=c("Korea", "China", "Japan", "US"),
                         Confirmed=c(107939, 120940, 11636, 113690),
                         Deaths=c(250, 800, 198, 3433))

#### 나. 코로나19 데이터 ####
library(tidyverse); library(reshape2); library(tibble)

url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", 
         "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", 
         "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

confirmedCases <- read_csv(url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Confirmed"=4) %>% 
  mutate(Country=ifelse(!is.na(State) & State=="Hubei", "Hubei", Country)) %>% 
  group_by(Country,Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

DeathCases <- read_csv(url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  mutate(Country=ifelse(!is.na(State) & State=="Hubei", "Hubei", Country)) %>% 
  group_by(Country,Variable) %>% summarise(Deaths=sum(Deaths)) %>% 
  rename("Country"=1,"Date"=2,"Deaths"=3)

recoveredCases <- read_csv(url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  mutate(Country=ifelse(!is.na(State) & State=="Hubei", "Hubei", Country)) %>% 
  group_by(Country, Variable) %>% summarise(Recovered=sum(Recovered)) %>% 
  rename("Country"=1,"Date"=2,"Recovered"=3)

rdata$COVID19 <- merge(merge(confirmedCases, DeathCases, by.y=c("Country","Date")), 
                    recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y"))

rdata$COVID19$Country <- gsub("Republic of Korea", "Korea", rdata$COVID19$Country)
rdata$COVID19$Country<-gsub("Iran \\(Islamic Republic of\\)","Iran",rdata$COVID19$Country)
rdata$COVID19$Country <- gsub("Hong Kong SAR", "Hong Kong", rdata$COVID19$Country)
rdata$COVID19$Country <- gsub("Mainland China", "China", rdata$COVID19$Country)
rm("confirmedCases", "DeathCases", "recoveredCases", "url")



#### 2. 데이터 분석 ####
data