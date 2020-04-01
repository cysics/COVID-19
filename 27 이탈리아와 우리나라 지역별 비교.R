#### 1. 데이터 가져오기 ####
library(pins); library(tidyverse); library(reshape2)

rdata <- list()

#### 가. 우리나라 데이터 가져오기 ####
board_register_kaggle(token="data/kaggle.json")
pin_find("COVID-19", board="kaggle") %>%  DT::datatable()
rdata$url <- pin_get("kimjihoo/coronavirusdataset")


print(rdata$Korea <- read_csv(rdata$url[1]))  # Case.csv
print(rdata$Korea <- read_csv(rdata$url[2]))  # PatientInfo.csv
print(rdata$Korea <- read_csv(rdata$url[3]))  # PatientRoute.csv
print(rdata$Korea <- read_csv(rdata$url[4]))  # Region.csv
print(rdata$Korea <- read_csv(rdata$url[5]))  # SearchTrend.csv
print(rdata$Korea <- read_csv(rdata$url[6]))  # SeoulFloating.csv
print(rdata$Korea <- read_csv(rdata$url[7]))  # Time.csv
print(rdata$Korea <- read_csv(rdata$url[8]))  # TimeAge.csv
print(rdata$Korea <- read_csv(rdata$url[9]))  # TimeGender.csv
print(rdata$Korea <- read_csv(rdata$url[10])) # TimeProvince.csv
print(rdata$Korea <- read_csv(rdata$url[11])) # Weather.csv

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
  select(Date, Confirmed) %>% rename(Date=1, Rombardia=2)
rdata$Veneto <- Italy %>% filter(Province=="Veneto") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Veneto=2)
rdata$IOthers <- Italy %>% filter(Province=="Others") %>% 
  select(Date, Confirmed) %>% rename(Date=1, IOthers=2)

rdata$data1 <- data.frame(rdata$Daegu, Rombardia=rdata$Rombardia$Rombardia) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) # 대구, Rombardia
rdata$data2 <- data.frame(rdata$Gyeongbuk, Veneto=rdata$Veneto$Veneto) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) # 경북, Veneto
rdata$data3 <- data.frame(rdata$Daegu, Veneto=rdata$Veneto$Veneto) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) # 대구, Veneto
rdata$data4 <- data.frame(rdata$KOthers, Veneto=rdata$IOthers$IOthers) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) # Others들끼리

#### 2. 데이터 비교 ####
library(ggpubr); library(scales)
ggline(Korea, x="Date", y="Confirmed", color="Province", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.7, 0.5)) +
  scale_y_continuous(labels=comma)

ggline(Italy, x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
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

