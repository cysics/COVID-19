#### 1. 데이터 전처리 ####
#### 가. 중국과 중국 이외의 데이터 로딩 ####
library(tidyverse); library(reshape2); library(tibble); 
rdata <- list()

rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# 중국과 중국 이외를 구분
rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  mutate(Country=ifelse(Country=="China", "China", "Others")) %>% 
  group_by(Country,Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

rdata$DeathCases <- read_csv(rdata$url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  mutate(Country=ifelse(Country=="China", "China", "Others")) %>% 
  group_by(Country,Variable) %>% summarise(Deaths=sum(Deaths)) %>% 
  rename("Country"=1,"Date"=2,"Deaths"=3)

rdata$recoveredCases <- read_csv(rdata$url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  mutate(Country=ifelse(Country=="China", "China", "Others")) %>% 
  group_by(Country, Variable) %>% summarise(Recovered=sum(Recovered)) %>% 
  rename("Country"=1,"Date"=2,"Recovered"=3)

rdata$COVID19 <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y"))

#### 나. 대구와 대구이외 지역 데이터 ####
library(corona19)
rdata$Korea <- getdata("state") %>% select(c(1, 8:24)) %>% melt(id="date") %>% 
  mutate(variable=ifelse(variable=="Daegu", "Daegu", "Others")) %>% 
  group_by(date, variable) %>% summarize(value=sum(value)) %>% 
  arrange(variable, date)



#### 2. 중국과 중국 이외 비교 ####
data <- rdata$COVID19

library(gganimate); library(scales)
print(result <- ggplot(data, aes(x=Date, y=Confirmed, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=Confirmed), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(" ", scales::comma(Confirmed, 
       accuracy=1))), size=7) +
  theme(legend.position=c(0.2, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("CvsOConfirm.gif"))

print(result <- ggplot(data, aes(x=Date, y=Deaths, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=Deaths), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(" ", scales::comma(Deaths, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.2, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("CvsODeaths.gif"))

print(result <- ggplot(data, aes(x=Date, y=Recovered, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=Recovered), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(" ", scales::comma(Recovered, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.2, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("CvsORecovered.gif"))


#### 3. 대구와 대구 이외 비교 ####
data <- rdata$Korea

print(result <- ggplot(data, aes(x=date, y=value, color=variable)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(date), yend=value), linetype=2) +
  geom_text(aes(x=max(date)+7, label=paste0(" ", scales::comma(value, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.2, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("DvsOConfirmed.gif"))
