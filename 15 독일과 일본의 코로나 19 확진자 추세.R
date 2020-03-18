#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); 
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

# 확진자, 사망자, 완치자 데이터 합치기 
data <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  arrange(Country, Date) %>% mutate(DayConfirmed=0)

max(data$Date)

data$Country <- gsub("Korea\\, South", "Korea", data$Country)
data$Country <- gsub("United Kingdom", "UK", data$Country)
data$Country <- gsub("United Arab Emirates", "A.Emirates", data$Country)



#### 2. 국가간 비교하기 ####
library(gganimate); library(scales)

#### 가. 시간에 따른 국가별 비교 ####
Rank <- data %>% filter(Confirmed>100 & Date>"2020-02-15") %>% 
  group_by(Date) %>% mutate(Rank=min_rank(-Confirmed)*1) %>% 
  ungroup() %>% filter(Rank<10)

print(result <- ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=Confirmed/2, height=Confirmed, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=7) +
  geom_text(aes(y=Confirmed, label=paste0(" ",comma(Confirmed))), 
            hjust=0, size=7)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title='{closest_state}', x = "", y = "Confirmed cases") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 70, 20, 90), text=element_text(size=20))+
  transition_states(Date, 4, 1))

animate(result, 600, fps=10, duration=60, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("Confirmed.gif"))


#### 나. 국가간 비교하기 ####
Line <-data %>% filter(Date>"2020-02-15") %>% 
  filter(Country=="Korea" | Country=="Germany") %>% 
  arrange(Country, Date)

Line <-data %>% filter(Date>"2020-02-15") %>% 
  filter(Country=="Japan" | Country=="Austria") %>% 
  arrange(Country, Date)
  
unique(data$Country)

print(result <- ggplot(Line, aes(x=Date, y=Confirmed, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date), yend=Confirmed), linetype=2) +
  geom_text(aes(x=max(Date)+7, label=paste0(scales::comma(Confirmed, 
            accuracy=1))), size=7) +
  theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
            plot.margin=margin(10, 30, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ConfirmedLineKG.gif"))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ConfirmedLineJA.gif"))
