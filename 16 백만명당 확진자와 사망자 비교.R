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
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  arrange(Country, Date) %>% mutate(DayConfirmed=0)

max(rdata$World$Date)
# unique(rdata$World$Country)

# 타이완에 *표 없애기, 우리나라 Korea로 표현하기, 국가 이름 일치시키기
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("The Bahamas", "Bahamas", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("Republic of the Congo", "Congo", rdata$World$Country)

#### 나. 나라별 인구 수 ####
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2019") %>% select(c(1,3))   # 인구는 2019년 기준

# 국가 이름 일치시키기 위해
# setdiff(unique(rdata$World$Country), unique(rdata$Population$Country)) 

# 국가 이름 일치시키기
rdata$Population$Country <- gsub("South Korea", "Korea", rdata$Population$Country)
rdata$Population$Country <- gsub("United States", "US", rdata$Population$Country)
rdata$Population$Country <- gsub("Czech Republic", "Czechia", 
                                 rdata$Population$Country)

# 검사자 수와 인구수 합치기
data <- merge(rdata$World, rdata$Population)

# 주요국가 긴 이름 줄이기
# unique(data$Country)
data$Country <- gsub("United Kingdom", "UK", data$Country)
data$Country <- gsub("United Arab Emirates", "A.Emirates", data$Country)
data$Country <- gsub("Dominican Republic", "Dominican", data$Country)
data$Country <- gsub("Burkina Faso", "Burkina", data$Country)

# 백만명당 검사자, 확진자, 사망자 구하기
data <- data %>% mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeathsperM=Deaths*1000000/Population)




#### 2. 국가간 비교하기 ####
#### 가. 누적 확진자 수 ####
library(gganimate); library(scales)
#### _1) 어제 기준 확진자 수 ####
Rank <- data %>% filter(Date==Sys.Date()-1) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-Confirmed)*1) %>% 
  ungroup() %>% filter(Rank<16)

Rank <- data %>% filter(Date==Sys.Date()-1) %>% 
  filter(Population>1000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-Confirmed)*1) %>% 
  ungroup() %>% filter(Rank<16)

ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=Confirmed/2, height=Confirmed, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=Confirmed, label=paste0(" ",comma(Confirmed))), vjust=0.4,
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_y_continuous(labels=comma) +
  scale_x_reverse() + guides(color=F, fill=F) +
  labs(title=Sys.Date()-1, x = "", y = "Confirmed cases") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 60), text=element_text(size=18))

#### _2) 시간에 따른 확진자 수 변화 ####
Rank <- data %>% filter(Confirmed>100 & Date>"2020-02-15") %>% 
  filter(Population>1000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-Confirmed)*1) %>% 
  ungroup() %>% filter(Rank<10)

print(result <- ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=Confirmed/2, height=Confirmed, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=Confirmed, label=paste0(" ",round(Confirmed, 0))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title='{closest_state}', x = "", y = "Confirmed cases") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 60), text=element_text(size=18))+
  transition_states(Date, 4, 1))

animate(result, 600, fps=10, duration=60, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("Confirmed.gif"))


#### _3) 백만명당 누적 확진자 수 ####
Rank <- data %>% filter(Date==Sys.Date()-1) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-ConfirmedperM)*1) %>% 
  ungroup() %>% filter(Rank<16)

Rank <- data %>% filter(Date==Sys.Date()-1) %>% 
  filter(Population>1000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-ConfirmedperM)*1) %>% 
  ungroup() %>% filter(Rank<16)

Rank <- data %>% filter(Date==Sys.Date()-1) %>% 
  filter(Population>10000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-ConfirmedperM)*1) %>% 
  ungroup() %>% filter(Rank<16)

ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=ConfirmedperM/2, height=ConfirmedperM, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=ConfirmedperM, 
            label=paste0(" ",comma(ConfirmedperM, accuracy=1))), hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title=Sys.Date()-1, x = "", y = "Confirmed cases per Million") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 75), text=element_text(size=18))


#### _4) 시간에 따른 백만명당 확진자 수 변화 ####
Rank <- data %>% filter(Confirmed>100 & Date>"2020-02-15") %>% 
  filter(Population>1000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-ConfirmedperM)*1) %>% 
  ungroup() %>% filter(Rank<10)

print(result <- ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=ConfirmedperM/2, height=ConfirmedperM, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=ConfirmedperM, label=paste0(" ",round(ConfirmedperM, 0))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title='{closest_state}', x = "", y = "Confirmed cases per Million") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 60), text=element_text(size=18))+
  transition_states(Date, 4, 1))

animate(result, 600, fps=10, duration=60, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ConfirmedperM.gif"))


#### 나. 누적 사망자 수 ####
#### _1) 전날 기준 사망자 수 ####
Rank <- data %>% filter(Date==Sys.Date()-1) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-Deaths)*1) %>% 
  ungroup() %>% filter(Rank<11)

ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=Deaths/2, height=Deaths, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=Deaths, label=paste0(" ",comma(Deaths, accuracy=1))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() + 
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title=Sys.Date()-1, x = "", y = "Death cases per Million") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 70), text=element_text(size=18))

#### _2) 전날 기준 백만명당 사망자 수 ####
Rank <- data %>% filter(Date==Sys.Date()-1) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-DeathsperM)*1) %>% 
  ungroup() %>% filter(Rank<16)

Rank <- data %>% filter(Date==Sys.Date()-1) %>% 
  filter(Population>1000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-DeathsperM)*1) %>% 
  ungroup() %>% filter(Rank<16)

ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=DeathsperM/2, height=DeathsperM, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=DeathsperM, label=paste0(" ",comma(DeathsperM, accuracy=1))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() + 
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title=Sys.Date()-1, x = "", y = "Death cases per Million") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 70), text=element_text(size=18))

##### _3) 시간에 따른 백만명당 사망자 수 ####
Rank <- data %>% filter(Confirmed>100 & Date>"2020-02-10") %>% 
  filter(Population>1000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-DeathsperM)*1) %>% 
  ungroup() %>% filter(Rank<10)

print(result <- ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=DeathsperM/2, height=DeathsperM, width=0.9),
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=DeathsperM, label=paste0(" ",round(DeathsperM, 1))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title='{closest_state}', x = "", y = "Death cases per Million") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 60), text=element_text(size=18))+
  transition_states(Date, 4, 1))

animate(result, 600, fps=10, duration=60, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("DeathperM.gif"))

#### 다. 사망률 비교 ####
#### _1) 전날 사망률 비교 ####
Rank <- data %>% filter(Confirmed>10 & Date==Sys.Date()-1) %>% 
  group_by(Date) %>% mutate(Rank=min_rank(-DeathRate)*1) %>% 
  ungroup() %>% filter(Rank<16)

Rank <- data %>% filter(Confirmed>10 & Date==Sys.Date()-1) %>% 
  filter(Population>1000000) %>% 
  group_by(Date) %>% mutate(Rank=min_rank(-DeathRate)*1) %>% 
  ungroup() %>% filter(Rank<16)

ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=DeathRate/2, height=DeathRate, width=0.9), alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=DeathRate, label=paste0(" ",round(DeathRate, 2))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_y_continuous(labels=comma) +
  scale_x_reverse() + guides(color=F, fill=F) +
  labs(title=Sys.Date()-1, x = "", y = "DeathRate") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 65), text=element_text(size=18))

##### _2) 시간에 따른 사망률 ####
Rank <- data %>% filter(Confirmed>100 & Date>"2020-02-10") %>% 
  filter(Population>1000000) %>% group_by(Date) %>% 
  mutate(Rank=min_rank(-DeathRate)*1) %>% 
  ungroup() %>% filter(Rank<10)

print(result <- ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=DeathRate/2, height=DeathRate, width=0.9),
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=DeathRate, label=paste0(" ",round(DeathRate, 2))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  labs(title='{closest_state}', x = "", y = "Death Rate") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 55, 20, 60), text=element_text(size=18))+
  transition_states(Date, 4, 1))

animate(result, 600, fps=10, duration=60, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("DeathRate.gif"))


