#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); 
library(ggpubr); library(gganimate); 
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

# 미국은 주 데이터만 인정, 후베이는 따로 구분하지 않음.
rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  mutate(Country=ifelse(Country=="US" & !grepl(",", State), NA, Country)) %>% 
  group_by(Country,Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

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

max(rdata$COVID19$Date)



#### 2. 데이터 분석 ####
#### 가. 가장 최근 날짜 사망률과 비교하기 ####
rdata$data1 <- rdata$COVID19 %>% 
  select(c("Country", "Confirmed", "Deaths", "DeathRate", "Date")) %>% 
  filter(Country=="Korea" | Country=="China" | Country=="Japan" | Country=="US")

rdata$data2 <- rdata$H1N1 %>% select(c("Country", "Confirmed", "Deaths", "DeathRate"))

str(data <- rbind(data.frame(group="COVID19", rdata$data1), 
  data.frame(group="H1N1", merge(rdata$data2, rdata$data1['Date'], by=NULL))) %>%
      unique() %>% mutate(DeathRate=round(DeathRate*100, 2)) %>% 
      mutate(group=factor(group, levels=c("H1N1", "COVID19"))) %>% 
      mutate(Country=factor(Country, levels=c("Korea", "China", "Japan", "US"))) %>% 
  filter(Date<"2020-03-10"))

data[data$Date=="2020-03-09",] %>% ggbarplot(x="Country", y="DeathRate", 
  ylab="Death Rate(%)", fill="group", ylim=c(0,4.5), 
  position=position_dodge(0.85), label=T, lab.size=5) + 
  theme(text=element_text(size=20), 
  plot.margin=margin(10, 30, 10, 10), legend.position=c(0.2, 0.7))


#### 나. 시간에 따른 사망률 비교 ####
print(result <- data %>% ggplot(aes(x=Country, y=DeathRate, fill=group)) + 
  geom_bar(stat="identity", color="black", position=position_dodge(0.9))+
  geom_text(aes(label=paste0(DeathRate)), vjust=-0.5, 
            position=position_dodge(0.9), size=6)+
  theme(text=element_text(size=20), 
  plot.margin=margin(10, 10, 10, 10), legend.position=c(0.2, 0.7)) +
  transition_time(Date) + labs(title = "{frame_time}"))

animate(result, 300, fps=10, duration=30, end_pause=50, width=500, height=400, 
        renderer=gifski_renderer("H1N1vsCOVID19_DR.gif"))


#### 다. 확진자 수 비교 ####
print(result <- data %>% ggplot(aes(x=Country, y=Confirmed, fill=group)) + 
  geom_bar(stat="identity", color="black", position=position_dodge(0.8))+
  coord_cartesian(ylim=c(0, max(data$Confirmed)+10000))+
  scale_y_continuous(labels=scales::comma) + 
  geom_text(aes(label=paste0(scales::comma(Confirmed))), vjust=-0.5, 
            position=position_dodge(0.8), size=6) + theme(legend.position="top") +
  theme(text=element_text(size=20), plot.margin=margin(10, 10, 10, 10))+
  transition_time(Date) + labs(title = "{frame_time}"))

animate(result, 300, fps=10, duration=30, end_pause=50, width=500, height=400, 
        renderer=gifski_renderer("H1N1vsCOVID19_CM.gif"))
