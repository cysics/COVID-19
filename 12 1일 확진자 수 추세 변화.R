#### 1. 데이터 전처리 ####
#### 가. 중국 데이터 ####
library(tidyverse); library(reshape2); library(tibble); 
rdata <- list()

rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# 중국과 중국 이외를 구분
rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  filter(Country=="China") %>% 
  mutate(State=ifelse(State=="Hubei", "Hubei", "Others")) %>% 
  group_by(State,Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("State"=1,"Date"=2,"Confirmed"=3)

rdata$DeathCases <- read_csv(rdata$url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  filter(Country=="China") %>% 
  mutate(State=ifelse(State=="Hubei", "Hubei", "Others")) %>% 
  group_by(State,Variable) %>% summarise(Confirmed=sum(Deaths)) %>% 
  rename("State"=1,"Date"=2,"Deaths"=3)

rdata$recoveredCases <- read_csv(rdata$url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  filter(Country=="China") %>% 
  mutate(State=ifelse(State=="Hubei", "Hubei", "Others")) %>% 
  group_by(State,Variable) %>% summarise(Confirmed=sum(Recovered)) %>% 
  rename("State"=1,"Date"=2,"Recovered"=3)

China <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("State","Date")), rdata$recoveredCases, by.y=c("State","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% arrange(State, Date)

max(China$Date)

rdata$ChinAll <- data.frame(State="All", Date=China[1:(nrow(China)/2),2], 
                            Confirmed=0, Deaths=0, Recovered=0)

for(i in 1:(nrow(China)/2)){
  rdata$ChinAll[i,3] <- China[i,3]+China[(nrow(China)/2)+i,3]
  rdata$ChinAll[i,4] <- China[i,4]+China[(nrow(China)/2)+i,4]
  rdata$ChinAll[i,5] <- China[i,5]+China[(nrow(China)/2)+i,5]
}

China <- rbind(China, rdata$ChinAll) %>% mutate(DayConfirmed=0)

for(i in 2:(nrow(China)/3)){
  China[i,6] <- China[i, 3]-China[i-1, 3]
  China[nrow(China)/3+i,6] <- China[nrow(China)/3+i, 3]-China[nrow(China)/3+i-1, 3]
  China[2*nrow(China)/3+i,6] <- China[2*nrow(China)/3+i,3]-China[2*nrow(China)/3+i-1,3]
}

#### 나. 우리나라 데이터 ####
library(corona19); library(tidyverse); library(reshape2)

Korea <- getdata("state") %>% select(c(1, 5, 8:24)) %>% 
  melt(id=c("date")) %>% 
  mutate(variable=ifelse(variable=="Daegu", "Daegu", 
                         ifelse(variable=="confirmed", "All", "Others"))) %>% 
  group_by(date, variable) %>% summarize(value=sum(value)) %>% 
  arrange(variable, date) %>% mutate(DayConfirmed=0)

for(i in 2:(nrow(Korea)/3)){
  Korea[i,4] <- Korea[i, 3]-Korea[i-1, 3]
  Korea[nrow(Korea)/3+i,4] <- Korea[nrow(Korea)/3+i, 3]-Korea[nrow(Korea)/3+i-1, 3]
  Korea[2*nrow(Korea)/3+i,4] <- Korea[2*nrow(Korea)/3+i,3]-Korea[2*nrow(Korea)/3+i-1,3]
}

max(Korea$date)

##### 2. 데이터 분석 ####
library(gganimate); library(scales)

#### 가. 중국 데이터 비교 ####
print(result <- ggplot(China, aes(x=Date, y=DayConfirmed, color=State)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=DayConfirmed), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(scales::comma(DayConfirmed, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.7, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("DayConfirmC.gif"))


#### 나. 우리나라 데이터 비교 ####
print(result <- ggplot(Korea, aes(x=date, y=DayConfirmed, color=variable)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(date), yend=DayConfirmed), linetype=2) +
  geom_text(aes(x=max(date)+7, label=paste0(" ", scales::comma(DayConfirmed, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("DayConfirmK.gif"))
