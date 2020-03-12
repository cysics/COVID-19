#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble)
rdata <- list()


#### 가. 신종플루 데이터 ####
# https://en.wikipedia.org/wiki/2009_flu_pandemic_by_country
print(rdata$H1N1 <- data.frame(Country=c("Korea", "China", "Japan", "US"),
                         Confirmed=c(107939, 120940, 11636, 113690),
                         Deaths=c(250, 800, 198, 3433)) %>% 
  mutate(DeathRate=Deaths/Confirmed))


#### 나. 코로나19 데이터 ####
# Johns Hopkins University Center for Systems Science and Engineering
# https://github.com/CSSEGISandData/COVID-19
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", 
         "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", 
         "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  mutate(Country=ifelse(Country=="US" & !grepl(",", State), NA, Country)) %>% 
  group_by(Country,Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

nrow(rdata$confirmedCases %>% filter(Country=="US"))

rdata$DeathCases <- read_csv(rdata$url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  mutate(Country=ifelse(Country=="US" & !grepl(",", State), NA, Country)) %>% 
  group_by(Country,Variable) %>% summarise(Deaths=sum(Deaths)) %>% 
  rename("Country"=1,"Date"=2,"Deaths"=3)

rdata$recoveredCases <- read_csv(rdata$url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  mutate(Country=ifelse(Country=="US" & !grepl(",", State), NA, Country)) %>% 
  group_by(Country, Variable) %>% summarise(Recovered=sum(Recovered)) %>% 
  rename("Country"=1,"Date"=2,"Recovered"=3)

rdata$COVID19 <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  mutate(DeathRate=Deaths/Confirmed)

rdata$COVID19$Country <- gsub("Korea\\, South", "Korea", rdata$COVID19$Country)
rdata$COVID19$Country <- gsub("Iran \\(Islamic Republic of\\)", "Iran",
                              rdata$COVID19$Country)

max(rdata$COVID19$Date)



#### 2. 데이터 분석 ####
#### 가. 가장 최근 날짜 사망률과 비교하기 ####
rdata$data1 <- rdata$COVID19 %>% select(c("Country", "DeathRate", "Date")) %>% 
  filter(Country=="Korea" | Country=="China" | Country=="Japan" | Country=="US")

rdata$data2 <- rdata$H1N1 %>% select(c("Country", "DeathRate"))

str(data <- rbind(data.frame(group="COVID19", rdata$data1), 
  data.frame(group="H1N1", merge(rdata$data2, rdata$data1['Date'], by=NULL))) %>%
      unique() %>% mutate(DeathRate=round(DeathRate*100, 2)) %>% 
      mutate(group=factor(group, levels=c("H1N1", "COVID19"))) %>% 
      mutate(Country=factor(Country, levels=c("Korea", "China", "Japan", "US"))))

library(ggpubr)
data[data$Date=="2020-03-11",] %>% ggbarplot(x="Country", y="DeathRate", 
  ylab="Death Rate(%)", fill="group", ylim=c(0,4.2), palette="Paired",
  position=position_dodge(0.85), label=T, lab.size=5) + 
  theme(text=element_text(size=20), 
  plot.margin=margin(10, 30, 10, 10), legend.position=c(0.2, 0.7))


#### 나. 시간에 따른 변화 ####
library(gganimate); library(scales)
data %>% ggbarplot(x="Country", y="DeathRate", fill="group", ylab="Death Rate(%)", 
  palette="Paired", position=position_dodge(0.85), label=T, lab.size=7)+
  theme(text=element_text(size=20), plot.margin=margin(10, 30, 10, 10),
        legend.position=c(0.2, 0.7))+
  transition_time(Date) + labs(title = "{frame_time}")
