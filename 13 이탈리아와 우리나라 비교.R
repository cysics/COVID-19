#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); 
rdata <- list()

rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")


rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  filter(Country=="Italy" | Country=="Korea, South") %>% 
  group_by(Country, Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

rdata$DeathCases <- read_csv(rdata$url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  filter(Country=="Italy" | Country=="Korea, South") %>% 
  group_by(Country, Variable) %>% summarise(Confirmed=sum(Deaths)) %>% 
  rename("Country"=1,"Date"=2,"Deaths"=3)

rdata$recoveredCases <- read_csv(rdata$url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  filter(Country=="Italy" | Country=="Korea, South") %>% 
  group_by(Country, Variable) %>% summarise(Confirmed=sum(Recovered)) %>% 
  rename("Country"=1,"Date"=2,"Recovered"=3)

data <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% arrange(Country, Date) %>% 
  mutate(DayConfirmed=0)

max(data$Date)

for(i in 2:(nrow(data)/2)){
  data[i,6] <- data[i, 3]-data[i-1, 3]
  data[nrow(data)/2+i,6] <- data[nrow(data)/2+i, 3]-data[nrow(data)/2+i-1, 3]
}

data$Country <- gsub("Korea\\, South", "Korea", data$Country)



##### 2. 데이터 분석 ####
library(gganimate); library(scales)

#### 가. 누적 확진자 추세 ####
print(result <- ggplot(data, aes(x=Date, y=Confirmed, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=Confirmed), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(scales::comma(Confirmed, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.4, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("Confirmed.gif"))

#### 나. 1일 확진자 추세 ####
print(result <- ggplot(data, aes(x=Date, y=DayConfirmed, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=DayConfirmed), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(scales::comma(DayConfirmed, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.4, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("DayConfirmed.gif"))


#### 다. 누적 사망자 추세 ####
print(result <- ggplot(data, aes(x=Date, y=Deaths, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=Deaths), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(scales::comma(Deaths, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.4, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("Deaths.gif"))

