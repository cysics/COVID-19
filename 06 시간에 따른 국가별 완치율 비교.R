#### 1. 날짜에 따른 검사자, 음성판정, 확진자 수 ####
#### 가. 데이터 전처리 ####
library(corona19); library(tidyverse); library(reshape2)
library(gganimate); library(scales);
rdata <- list()

data1 <- getdata("state") %>% arrange(date) %>% 
  select(c("date", "test", "negative", "confirmed")) %>% 
  rename("날짜"=1, "검사자 수"=2,"음성판정자 수"=3, "확진자 수"=4) %>% 
  melt(id.vars="날짜")



#### 나. 그래프로 표현 ####
result1 <- ggplot(data1, aes(x=날짜, y=value, group=variable, color=variable)) + 
  scale_y_continuous(labels=comma) + ylab("누적 사례 수") +
  theme_classic() + geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(날짜), yend=value), linetype=2) +
  geom_text(aes(x=max(날짜)+7, label=comma(value)), size=7)+
  theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(날짜) + view_follow(fixed_y=T) + coord_cartesian(clip='off')

#### 다. 결과물 출력 ####
animate(result1, fps=5, end_pause=20, renderer=gifski_renderer("VirusEvolution.gif"))
animate(result1, fps=5, end_pause=20, renderer=av_renderer('VirusEvolution.mp4'))



#### 2. 날짜에 따른 나라별 완치율 ####
confirmedCases <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
confirmedCases <- confirmedCases %>% select(-c(Lat,Long)) %>%
  melt(id=c('Country/Region','Province/State'))
confirmedCases <- confirmedCases %>% group_by(`Country/Region`,variable) %>%
  summarise(Confirmed=sum(value))

recoveredCases <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')
recoveredCases <- recoveredCases %>% select(-c(Lat,Long)) %>%
  melt(id=c('Country/Region','Province/State'))
recoveredCases <- recoveredCases %>% group_by(`Country/Region`,variable) %>% 
  summarise(Recovered=sum(value))

colnames(confirmedCases) <- c("Country","Date","Confirmed")
colnames(recoveredCases) <- c("Country","Date","Recovered")
data2 <- merge(confirmedCases, recoveredCases, by.y=c("Country","Date"))

data2$Date<-as.Date(data2$Date,"%m/%d/%y")
data2$Date <- data2$Date+1
data2$Country <- gsub("United Arab Emirates", "A.Emirates", data2$Country)
data2$Country <- gsub("Mainland China", "China", data2$Country)
data2$Country <- gsub("South Korea", "Korea", data2$Country)


CureRate <- data2 %>% filter(Confirmed>20) %>% group_by(Country)  %>% 
  mutate(CureRate = spline(x=Date, y=Recovered/Confirmed, xout=Date)$y) %>% 
  group_by(Date) %>% mutate(Rank=min_rank(-CureRate)*1) %>% ungroup() %>% 
  filter(CureRate>0.01 & Rank<10)

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

nrow(rdata$confirmedCases %>% filter(Country=="US")) # 50개 주가 나오면 정상

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
rdata$COVID19$Country <- gsub("United Arab Emirates", "A.Emirates", 
                              rdata$COVID19$Country)

max(rdata$COVID19$Date)

CureRate <- rdata$COVID19 %>% filter(Confirmed>20) %>% group_by(Country)  %>% 
  mutate(CureRate = spline(x=Date, y=Recovered/Confirmed, xout=Date)$y) %>% 
  group_by(Date) %>% mutate(Rank=min_rank(-CureRate)*1) %>% ungroup() %>% 
  filter(CureRate>0.01 & Rank<10)

result2 <- ggplot(CureRate, aes(Rank, group=Country, fill=Country, color=Country)) +
  geom_tile(aes(y=CureRate/2, height=CureRate, width=0.9), alpha=0.8, color=NA) +
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=7) +
  geom_text(aes(y=CureRate, label=paste0(" ",round(CureRate, 2))), hjust=0, size=7)+ 
  coord_flip(clip="off", expand=F) + scale_y_continuous() +
  scale_x_reverse() + guides(color=F, fill=F) +
  labs(title='{closest_state}', x = "", y = "Complete Cure Rate") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin=margin(20, 60, 20, 95),
        text=element_text(size=20)) +
  transition_states(Date, state_length=1) + ease_aes('cubic-in-out')

animate(result2, 600, fps=10, duration=60, end_pause=50, width=800, height=600, 
        renderer=gifski_renderer("CureEvolution.gif"))
animate(result2, 600, fps=10, duration=60, end_pause=50, width=800, height=600, 
        renderer=av_renderer("CureEvolution.mp4"))
