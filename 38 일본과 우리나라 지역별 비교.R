#### 1. 데이터 가져오기 ####
library(pins); library(tidyverse); library(reshape2)
library(tabulizer); library(webshot);

rdata <- list()

#### 가. 전세계 데이터 가져오기 ####
rdata$urlW <- c("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
rdata$World <- read_csv(rdata$urlW) %>% select(c(2:16)) %>% 
  rename("Country"=1, "Date"=2, "Confirmed"=3, "DailyC"=4, "Deaths"=5, "DailyD"=6,
         "ConfirmedperM"=7, "DailyCperM"=8, "DeathsperM"=9, "DailyDperM"=10,
         "TotalTests"=11, "DailyT"=12, "TestsperT"=13, "DailyTperT"=14, 
         "TestsU"=15)

unique(rdata$World$Country)
rdata$World$Country <- gsub("South Korea", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("United States", "US", rdata$World$Country)
rdata$World$Country <- gsub("United Kingdom", "UK", rdata$World$Country)

#### 나. 우리나라 데이터 가져오기 ####
# 위키피디아 : 나중에 잘 활용해 보자. 일단 보류
# webshot("https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_South_Korea", "Korea.pdf")
# print(rdata$Korea <- extract_tables("Korea.pdf", pages=1, encoding="UTF-8")[6] %>% as.data.frame())
# rdata$Korea %>% slice(c(3:(nrow(.)-12)))

rdata$urlK <- c("https://raw.githubusercontent.com/jihoo-kim/Data-Science-for-COVID-19/master/dataset/Time/TimeProvince.csv")
rdata$Korea <- read_csv(rdata$urlK) %>% select(-c(2)) %>% 
  rename("Date"=1, "Province"=2, "Confirmed"=3, "Recovered"=4, "Deaths"=5) %>% 
  mutate(Province=ifelse(Province=="Daegu", "Daegu",
               ifelse(Province=="Gyeongsangbuk-do", "Gyeongbuk", "Others"))) %>% 
  group_by(Date, Province) %>% summarise(Confirmed=sum(Confirmed))

#### 다. 일본 데이터 (캐글) ####
board_register_kaggle(token="data/kaggle.json")
pin_find("Japan", board="kaggle") %>%  DT::datatable()
rdata$urlJ <- pin_get("lisphilar/covid19-dataset-in-japan")
print(rdata$Japan <- read_csv(rdata$urlJ[2]) %>% 
        rename("Date"=1, "Province"=2, "Confirmed"=3, "TotalTests"=4, 
               "Recovered"=5, "Deaths"=6) %>% 
        mutate(Province=ifelse(Province=="Tokyo", "Tokyo",
                     ifelse(Province=="Hokkaido", "Hokkaido", "Others"))) %>% 
        group_by(Date, Province) %>% summarise(Confirmed=sum(Confirmed)))


#### 라. 이탈리아 데이터 가져오기 ####
rdata$urlI <- c("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")

rdata$Italy <- read_csv(rdata$urlI[2]) %>% select(c(1,4,10)) %>% 
  rename("Date"=1, "Province"=2, "Confirmed"=3) %>% 
  mutate(Date=as.Date(Date)) %>% 
  mutate(Province=ifelse(Province=="Veneto", "Veneto", 
                         ifelse(Province=="Lombardia", "Lombardia", "Others"))) %>% 
  group_by(Date, Province) %>% summarise(Confirmed=sum(Confirmed))



#### 마. 비슷한 지역 묶기 ####
rdata$Daegu <- rdata$Korea %>% filter(Province=="Daegu") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Daegu=2)
rdata$Gyeongbuk <- rdata$Korea %>% filter(Province=="Gyeongbuk") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Gyeongbuk=2)
rdata$KOthers <- rdata$Korea %>% filter(Province=="Others") %>% 
  select(Date, Confirmed) %>% rename(Date=1, KOthers=2)

rdata$Rombardia <- rdata$Italy %>% filter(Province=="Lombardia") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Rombardia=2)
rdata$Veneto <- rdata$Italy %>% filter(Province=="Veneto") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Veneto=2)
rdata$IOthers <- rdata$Italy %>% filter(Province=="Others") %>% 
  select(Date, Confirmed) %>% rename(Date=1, IOthers=2)

rdata$Tokyo <- rdata$Japan %>% filter(Province=="Tokyo") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Tokyo=2)
rdata$Hokkaido <- rdata$Japan %>% filter(Province=="Hokkaido") %>% 
  select(Date, Confirmed) %>% rename(Date=1, Hokkaido=2)
rdata$JOthers <- rdata$Japan %>% filter(Province=="Others") %>% 
  select(Date, Confirmed) %>% rename(Date=1, JOthers=2)

rdata$data1 <- merge(merge(rdata$Daegu, rdata$Rombardia, all=T),
                         rdata$Tokyo, all=T) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) 

rdata$data11 <- merge(rdata$Daegu, rdata$Tokyo, all=T) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) 

rdata$data2 <- merge(merge(rdata$Gyeongbuk, rdata$Veneto, all=T),
                     rdata$Hokkaido, all=T) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) 

rdata$data21 <- merge(rdata$Gyeongbuk, rdata$Hokkaido, all=T) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) 

rdata$data3 <- merge(merge(rdata$KOthers, rdata$IOthers, all=T),
                     rdata$JOthers, all=T) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) 

rdata$data31 <- merge(rdata$KOthers, rdata$JOthers, all=T) %>% 
  melt(id="Date") %>% rename(Date=1, Province=2, Confirmed=3) 

#### 바. 일본과 이탈리아 우리나라 비교 ####
print(rdata$KoreaC <- rdata$World %>% filter(Country=="Korea") %>% 
        filter(Confirmed>=1000))
print(rdata$JapanC <- rdata$World %>% filter(Country=="Japan") %>% 
        filter(Confirmed>=150))
print(rdata$ItalyC <- rdata$World %>% filter(Country=="Italy") %>% 
        filter(Confirmed>=1000 & Confirmed<=13000))
rdata$KoreaC <- rdata$KoreaC %>% mutate(Date=c(1:nrow(.))) %>% 
  select(c(2,3)) %>% rename(Date=1, Korea=2)
rdata$JapanC <- rdata$JapanC %>% mutate(Date=c(1:nrow(.))) %>% 
  select(c(2,3)) %>% rename(Date=1, Japan=2)
rdata$ItalyC <- rdata$ItalyC %>% mutate(Date=c(1:nrow(.))) %>% 
  select(c(2,3)) %>% rename(Date=1, Italy=2)
rdata$WorldC <- merge(merge(rdata$JapanC, rdata$ItalyC, all.x=T),
                      rdata$KoreaC, all=T)


#### 2. 데이터 비교 ####
library(ggpubr); library(scales)
#### 가. 국가별 비교 ####
ggline(rdata$World %>% select(c(1:3)) %>% filter(Date>"2020-02-20") %>% 
         filter(Country=="Japan" | Country=="Korea" | Country=="Italy"), 
       x="Date", y="Confirmed", color="Country", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$WorldC %>% melt(id.var="Date"), x="Date", y="value", 
       color="variable", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.5, 0.4)) +
  scale_y_continuous(labels=comma)

ggline(rdata$Korea %>% filter(Date>"2020-02-15"), 
       x="Date", y="Confirmed", color="Province", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.7, 0.6)) +
  scale_y_continuous(labels=comma)

ggline(rdata$Italy %>% mutate(Province=factor(Province, 
       levels=c("Lombardia", "Veneto", "Others"))), 
       x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$Japan %>% mutate(Province=factor(Province, 
       levels=c("Tokyo", "Hokkaido", "Others"))),
       x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)



#### 나. 심각지역 비교 ####
ggline(rdata$data1 %>% filter(Date>"2020-02-20"), 
       x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data11 %>% filter(Date>"2020-02-20"), 
       x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.5, 0.6)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data2, x="Date", y="Confirmed", color="Province", 
       size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data21 %>% filter(Date>"2020-02-20"), 
       x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.6, 0.6)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data3, x="Date", y="Confirmed", color="Province", 
       size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.2, 0.8)) +
  scale_y_continuous(labels=comma)

ggline(rdata$data31 %>% filter(Date>"2020-02-20"), 
       x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14),
        legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)


#### 다. 교실수업 ####
ggline(rdata$World %>% filter(Country=="Singapore" & Date>"2020-03-15"),
       x="Date", y="Confirmed", color="Country", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$World %>% filter(Country=="Singapore" & Date>"2020-03-15"),
       x="Date", y="DailyC", color="Country", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$World %>% filter(Country=="Singapore" & Date>"2020-03-15"), 
       ylim=c(1,100), x="Date", y="DailyC", color="Country", size=1.2)+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.8, 0.3)) +
  scale_y_continuous(labels=comma)

ggline(rdata$Japan %>% filter(Province=="Hokkaido"), 
       x="Date", y="Confirmed", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(rdata$Japan %>% filter(Province=="Hokkaido") %>% as.data.frame() %>% 
         mutate(Confirmed=as.numeric(as.character(Confirmed))) %>% 
         mutate(DailyC=Confirmed-lag(Confirmed)) %>% arrange(desc(Date)), 
       x="Date", y="DailyC", color="Province", size=1.2, legend="right")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)




rdata$World %>% filter(Country=="Singapore") %>% 
  filter(Date>"2020-03-20" & Date<"2020-04-01")
