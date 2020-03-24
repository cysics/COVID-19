#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr)
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

# 확진자, 사망자, 완치자 합치기
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y"))

# 타이완에 *표 없애기, 우리나라 Korea로 표현하기, 국가 이름 일치시키기
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)

# 사망률 계산하기
head(rdata$World <- rdata$World %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  arrange(Country, Date))

max(rdata$World$Date)

#### 나. 나라별 인구수 ####
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2019") %>% select(c(1,3))   # 인구는 2019년 기준

# 국가 이름 확인
# setdiff(rdata$World$Country, rdata$Population$Country)
# unique(rdata$World$Country)
# rdata$Population$Country
# rdata$Population %>% filter(Country=="Timor")

# 이름 일치시키기(페로, 홍콩, 팔레스타인 제외)
rdata$Population$Country <- gsub("South Korea", "Korea", rdata$Population$Country)
rdata$Population$Country <- gsub("United States", "US", rdata$Population$Country)
rdata$Population$Country <- gsub("Czech Republic", "Czechia", 
                                 rdata$Population$Country)
rdata$Population$Country <- gsub("East Timorc", "Timorc", rdata$Population$Country)
rdata$World$Country <- gsub("Bahamas, The", "Bahamas", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("Gambia, The", "Gambia", rdata$World$Country)
rdata$World$Country <- gsub("East Timor", "Timor", rdata$World$Country)



# 검사자 수, 인구, 확진자, 사망자, 완치자 데이터 모두 합치기
head(data <- merge(rdata$Population, rdata$World, by='Country') %>% 
       arrange(Country, Date))

# 백만명당 확진자, 사망자 구하기
data <- data %>% mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeathsperM=Deaths*1000000/Population) 



#### 2. 변화 추세 비교 ####
#### 가. 중국, 한국, 이탈리아 확진자 수 비교 ####
library(gganimate); library(scales)
data %>% filter(Date==Sys.Date()-1)

# 전날 데이터가 없으면 Sys.Date()-1로, 있으면 Sys.Date()로 해주세요.
print(China <-data %>% filter(Country=="China" & 
  Date>="2020-01-23" & Date<Sys.Date()) %>% arrange(Country, Date))

print(Italy <-data %>% filter(Country=="Italy" & 
  Date>="2020-02-22" & Date<Sys.Date()) %>% arrange(Country, Date))

print(Korea <-data %>% filter(Country=="Korea" & 
  Date>="2020-02-18" & Date<Sys.Date()) %>% arrange(Country, Date))

China$Date <- c(1:nrow(China))
Italy$Date <- c(1:nrow(Italy))
Korea$Date <- c(1:nrow(Korea))

# 전날 데이터가 없으면 nrow(Iraq)-1로, 있으면 nrow(Iraq)로 수정해 주세요.
Line <- rbind(China[1:nrow(Italy),], Italy[1:nrow(Italy),], 
              Korea[1:nrow(Italy),])

print(result <- ggplot(Line, aes(x=Date, y=Confirmed, color=Country)) + 
        scale_y_continuous(labels=comma) + theme_classic() + 
        geom_line(size=1.2) + geom_point(size=5) + 
        geom_segment(aes(xend=max(Date)+1, yend=Confirmed), linetype=2) +
        geom_text(aes(x=max(Date)+5, 
                      label=paste0(comma(Confirmed, accuracy=1))), size=7) +
        theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
              plot.margin=margin(10, 30, 10, 10)) +
        transition_reveal(Date) + view_follow(fixed_y=T) + 
        coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ChinaItalyKorea.gif"))



#### 나. 백만명당 확진자 수 비교 ####
print(result <- ggplot(Line, aes(x=Date, y=ConfirmedperM, color=Country)) + 
        scale_y_continuous(labels=comma) + theme_classic() + 
        labs(y = "Confirmed Cases per million")+
        geom_line(size=1.2) + geom_point(size=5) + 
        geom_segment(aes(xend=max(Date)+1, yend=ConfirmedperM), linetype=2) +
        geom_text(aes(x=max(Date)+5, 
              label=paste0(comma(ConfirmedperM, accuracy=1))), size=7) +
        theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
              plot.margin=margin(10, 30, 10, 10)) +
        transition_reveal(Date) + view_follow(fixed_y=T) + 
        coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("CIKConfirmedperM.gif"))


#### 다. 사망자 수 비교 ####
print(result <- ggplot(Line, aes(x=Date, y=Deaths, color=Country)) + 
        scale_y_continuous(labels=comma) + theme_classic() + 
        labs(y = "Confirmed Cases per million")+
        geom_line(size=1.2) + geom_point(size=5) + 
        geom_segment(aes(xend=max(Date)+1, yend=Deaths), linetype=2) +
        geom_text(aes(x=max(Date)+5, 
                      label=paste0(comma(Deaths, accuracy=1))), size=7) +
        theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
              plot.margin=margin(10, 30, 10, 10)) +
        transition_reveal(Date) + view_follow(fixed_y=T) + 
        coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("CIKDeaths.gif"))
