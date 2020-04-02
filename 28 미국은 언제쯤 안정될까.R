#### 1. 데이터 가져오기 ####
library(pins); library(tidyverse); library(reshape2)

rdata <- list()

#### 가. 우리나라 데이터 가져오기 ####
board_register_kaggle(token="data/kaggle.json")
pin_find("COVID-19", board="kaggle") %>%  DT::datatable()
rdata$urlK <- pin_get("kimjihoo/coronavirusdataset")
print(rdata$Korea <- read_csv(rdata$urlK[10]) %>% arrange(desc(date))) # TimeProvince.csv

Korea <- rdata$Korea %>% select(-c(time)) %>% filter(date>="2020-02-24") %>% 
  mutate(province=ifelse(province=="Daegu", "Daegu", 
         ifelse(province=="Gyeongsangbuk-do", "Gyeongbuk", "Others"))) %>% 
  rename("Date"=1, "Province"=2, "Confirmed"=3, "Recovered"=4, "Deaths"=5) %>% 
  group_by(Date, Province) %>% 
  summarise(Confirmed=sum(Confirmed), Recovered=sum(Recovered), Deaths=sum(Deaths))


#### 나. 이탈리아 데이터 가져오기 ####
rdata$urlItaly <- c("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")

Italy <- read_csv(rdata$urlItaly[2]) %>% select(c(1,4,10)) %>% 
  rename("Date"=1, "Province"=2, "Confirmed"=3) %>% 
  mutate(Province=ifelse(Province=="Veneto", "Veneto", 
                         ifelse(Province=="Lombardia", "Lombardia", "Others"))) %>% 
  separate(Date, sep=" ", into=c("Date", "time")) %>% 
  mutate(Date=as.Date(.$Date, "%Y-%m-%d")) %>% select(c(1,3:4)) %>% 
  group_by(Date, Province) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  mutate(Daily=0)


#### 다. 미국 데이터 가져오기 ####
rdata$urlUS <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

US <- read_csv(rdata$urlUS) %>% select(c(7,12:ncol(.))) %>% 
  melt(id="Province_State") %>% rename("Province"=1, "Date"=2, "Confirmed"=3) %>% 
  mutate(Province=ifelse(Province=="New York", "New York", 
    ifelse(Province=="New Jersey", "New Jersey", "Others"))) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  filter(Date>="2020-02-24") %>% 
  group_by(Date, Province) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  mutate(Daily=0)



#### 다. 비슷한 지역 묶기 ####
rdata$Daegu <- Korea %>% filter(Province=="Daegu") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Daegu=2)
rdata$Gyeongbuk <- Korea %>% filter(Province=="Gyeongbuk") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Gyeongbuk=2)
rdata$KOthers <- Korea %>% filter(Province=="Others") %>% 
  select(Date, Confirmed) %>% rename(Date=1, KOthers=2)

rdata$Rombardia <- Italy %>% filter(Province=="Lombardia") %>% 
  filter(Date>="2020-02-24" & Date<="2020-03-30") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Rombardia=2)
rdata$Veneto <- Italy %>% filter(Province=="Veneto") %>% 
  filter(Date>="2020-02-24" & Date<="2020-03-30") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Veneto=2)
rdata$IOthers <- Italy %>% filter(Province=="Others") %>% 
  filter(Date>="2020-02-24" & Date<="2020-03-30") %>% 
  select(Date, Confirmed) %>% rename(Date=1, IOthers=2)

rdata$NewYork <- US %>% filter(Province=="New York") %>% 
  filter(Date>="2020-02-24" & Date<="2020-03-30") %>% 
  select(Date, Confirmed) %>% rename(Date=1, NewYork=2)
rdata$NewJersey <- US %>% filter(Province=="New Jersey") %>% 
  filter(Date>="2020-02-24" & Date<="2020-03-30") %>% 
  select(Date, Confirmed) %>% rename(Date=1, NewJersey=2)
rdata$UOthers <- US %>% filter(Province=="Others") %>% 
  filter(Date>="2020-02-24" & Date<="2020-03-30") %>% 
  select(Date, Confirmed) %>% rename(Date=1, IOthers=2)

rdata$data1 <- data.frame(rdata$Daegu, Rombardia=rdata$Rombardia$Rombardia,
                          NewYork=rdata$NewYork$NewYork) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) 
# 대구, Rombardia, New York
rdata$data2 <- data.frame(rdata$Gyeongbuk, Veneto=rdata$Veneto$Veneto) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) # 경북, Veneto
rdata$data3 <- data.frame(rdata$Daegu, Veneto=rdata$Veneto$Veneto) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) # 대구, Veneto
rdata$data4 <- data.frame(rdata$KOthers, Veneto=rdata$IOthers$IOthers) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) # Others들끼리


#### 라. 전세계 확진자와 사망자 등 ####
# 코로나19 데이터 주소가 바뀌었습니다.
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

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


# 확진자, 사망자 합치기
rdata$World <- merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")) %>% mutate(Date=as.Date(.$Date, "%m/%d/%y"))

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

data <- rdata$data %>% 
  filter(Country=="Korea" | Country=="US" | Country=="Italy") %>% 
  filter(Date>="2020-02-22")


#### 2. 데이터 비교 ####
library(ggpubr); library(scales)
data$Country <- factor(data$Country, 
                          levels=c("Korea", "Italy", "US"))
ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)


ggline(Korea, x="Date", y="Confirmed", color="Province", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.7, 0.5)) +
  scale_y_continuous(labels=comma)


Italy$Province <- factor(Italy$Province, 
                       levels=c("Lombardia", "Veneto", "Others"))
ggline(Italy, x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

US$Province <- factor(US$Province, 
                         levels=c("New York", "New Jersey", "Others"))
ggline(US, x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data1, x="Date", y="Confirmed", color="Province", 
       size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data2, x="Date", y="Confirmed", color="Province", 
       size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data3, x="Date", y="Confirmed", color="Province", 
       size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.2, 0.8)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data4, x="Date", y="Confirmed", color="Province", 
       size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14)) +
  scale_y_continuous(labels=comma)

