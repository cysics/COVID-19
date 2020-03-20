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



#### 2. 미국과 이탈리아 비교하기 ####
library(gganimate); library(scales)

#### 가. 날짜별 비교 ####
# 미국의 전날 데이터 있는지 확인
data %>% filter(Country=="Italy" & Date==Sys.Date()-1)
data %>% filter(Country=="US" & Date==Sys.Date()-1)

# 미국의 전날 데이터 없으면(NA로 표시되면) Sys.Date()-2로 수정해 주세요.
Line <-data %>% filter(Date>"2020-02-20" & Date<=Sys.Date()-1) %>% 
  filter(Country=="US" | Country=="Italy") %>% 
  arrange(Country, Date)

print(result <- ggplot(Line, aes(x=Date, y=Confirmed, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date)+1, yend=Confirmed), linetype=2) +
  geom_text(aes(x=max(Date)+5, label=paste0(scales::comma(Confirmed, 
        accuracy=1))), size=7) +
  theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
        plot.margin=margin(10, 30, 10, 10)) +
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ConfirmedLineUI1.gif"))


#### 나. 60명 이상 된 날부터 비교 ####
Italy <-data %>% filter(Country=="Italy" & Date>"2020-02-21") %>% 
  arrange(Country, Date)
US <-data %>% filter(Country=="US" & Date>"2020-02-27") %>% 
  arrange(Country, Date)

Italy$Date <- c(1:nrow(Italy))
US$Date <- c(1:nrow(US))

# 미국의 전날 데이터가 없으면(NA로 표시되면) 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US)-1,], US[1:nrow(US)-1,])

# 미국의 전날 데이터가 있으면 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US),], US[1:nrow(US),])

print(result <- ggplot(Line, aes(x=Date, y=Confirmed, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date)+1, yend=Confirmed), linetype=2) +
  geom_text(aes(x=max(Date)+3, label=paste0(scales::comma(Confirmed, 
            accuracy=1))), size=7) +
  theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
            plot.margin=margin(10, 30, 10, 10)) +
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ConfirmedLineUI2.gif"))

#### 다. 1000명을 초과한 날부터 비교 ####
Italy <-data %>% filter(Country=="Italy" & Date>"2020-02-28") %>% 
  arrange(Country, Date)
US <-data %>% filter(Country=="US" & Date>"2020-03-10") %>% 
  arrange(Country, Date)

Italy$Date <- c(1:nrow(Italy))
US$Date <- c(1:nrow(US))

# 미국의 전날 데이터가 없으면(NA로 표시되면) 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US)-1,], US[1:nrow(US)-1,])

# 미국의 전날 데이터가 있으면 다음 코드를 실행시켜 주세요.
Line <- rbind(Italy[1:nrow(US),], US[1:nrow(US),])

print(result <- ggplot(Line, aes(x=Date, y=Confirmed, color=Country)) + 
  scale_y_continuous(labels=comma) + theme_classic() + 
  geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Date)+1, yend=Confirmed), linetype=2) +
  geom_text(aes(x=max(Date)+2, label=paste0(scales::comma(Confirmed, 
            accuracy=1))), size=7) +
  theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
            plot.margin=margin(10, 30, 10, 10)) +
  transition_reveal(Date) + view_follow(fixed_y=T) + coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ConfirmedLineUI3.gif"))


