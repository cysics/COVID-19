#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr)
rdata <- list()

#### 가. 전세계 데이터 #####
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

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


# 확진자, 사망자 합치기
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
                           by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y"))

unique(rdata$World$Country)

# 타이완에 *표 없애기, 우리나라 Korea로 표현하기, 국가 이름 일치시키기
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("United Kingdom", "UK", rdata$World$Country)
rdata$World$Country <- gsub("United Arab Emirates", "A.Emirates", 
                            rdata$World$Country)

# 사망률 계산하기
head(rdata$data <- rdata$World %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
       arrange(Country, Date))

max(rdata$data$Date)
rdata$data %>% filter(Country=="Korea" & Date<=Sys.Date()-1)



#### 나. 이탈리아 전체와 지방 ####
rdata$urlItaly <- c("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")

# 이탈리아 전체
Italy1 <- read_csv(rdata$urlItaly[1]) %>% select(c(1,9:11)) %>% 
  rename("Date"=1, "Recovered"=2, "Deaths"=3, "Confirmed"=4) %>% 
  melt(id=c("Date"))
Italy1$variable <- factor(Italy1$variable, 
                         levels=c("Confirmed", "Recovered", "Deaths"))

# 증상, 집중치료, 자가격리
Italy2 <- read_csv(rdata$urlItaly[1]) %>% select(c(1,3,4,6)) %>% 
  rename("Date"=1, "Symptom"=2, "IntensiveCare"=3, "Isolation"=4) %>% 
  melt(id=c("Date"))
Italy2$variable <- factor(Italy2$variable, 
                          levels=c("Isolation", "Symptom", "IntensiveCare"))

# 지역별 : 롬바르디아, 베네토 및 나머지 주로 구분
Italy3 <- read_csv(rdata$urlItaly[2]) %>% select(c(1,4,10)) %>% 
  rename("Date"=1, "State"=2, "Confirmed"=3) %>% 
  mutate(State=ifelse(State=="Veneto", "Veneto", 
                      ifelse(State=="Lombardia", "Lombardia", "Others"))) %>% 
  group_by(Date, State) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  mutate(Daily=0)


#### 2. 이탈리아, 중국, 한국 비교 ####
library(gganimate); library(scales)

# 전날 데이터가 없으면 Sys.Date()-1로, 있으면 Sys.Date()로 해주세요.
print(China <-rdata$data %>% filter(Country=="China" & 
  Date>="2020-01-23" & Date<Sys.Date()) %>% arrange(Country, Date))

print(Italy <-rdata$data %>% filter(Country=="Italy" & 
  Date>="2020-02-22" & Date<Sys.Date()) %>% arrange(Country, Date))

print(Korea <-rdata$data %>% filter(Country=="Korea" & 
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


#### 3. 이탈리아 분석 ####
#### 가. 누적 확진자, 회복자, 사망자 수 등  ####
library(ggpubr)
ggline(Italy1, x="Date", y="value", color="variable", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14))


#### 나. 입원 및 자가격리 ####
ggline(Italy2, x="Date", y="value", color="variable", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14))


#### 다. 지역별 누적 확진자 수 ####
ggline(Italy3, x="Date", y="Confirmed", color="State", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14))


#### 라. 지역별 1일 감염자 수 추정 ####
Day <- 20 # 15, 20일 후에 완치된다고 가정할 경우
for(i in 1:(nrow(Italy3)/3-1)){
  if(i>Day){
    Italy3[3*i+1,4] <- Italy3[3*i+1, 3]-Italy3[3*i+1-3*Day,3]
    Italy3[3*i+2,4] <- Italy3[3*i+2, 3]-Italy3[3*i+2-3*Day,3]
    Italy3[3*i+3,4] <- Italy3[3*i+3, 3]-Italy3[3*i+3-3*Day,3]
  }else{
    Italy3[3*i+1,4] <- Italy3[3*i+1, 3]
    Italy3[3*i+2,4] <- Italy3[3*i+2, 3]
    Italy3[3*i+3,4] <- Italy3[3*i+3, 3]
  }
}

ggline(Italy3, x="Date", y="Daily", color="State", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14))




