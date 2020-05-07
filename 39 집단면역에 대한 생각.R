#### 1. 데이터 가져오기 ####
library(pins); library(tidyverse); library(reshape2)
library(tabulizer); library(webshot);

rdata <- list()

#### 가. 전세계 데이터 가져오기(영국) ####
rdata$urlW <- c("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
rdata$World <- read_csv(rdata$urlW) %>% select(c(2:16)) %>% 
  rename("Country"=1, "Date"=2, "Confirmed"=3, "DailyC"=4, "Deaths"=5, "DailyD"=6,
         "ConfirmedperM"=7, "DailyCperM"=8, "DeathsperM"=9, "DailyDperM"=10,
         "TotalTests"=11, "DailyT"=12, "TestsperT"=13, "DailyTperT"=14, 
         "TestsU"=15) %>% mutate(DeathRate=100*Deaths/Confirmed)

max(rdata$World$Date)
unique(rdata$World$Country)
rdata$World$Country <- gsub("South Korea", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("United States", "US", rdata$World$Country)
rdata$World$Country <- gsub("United Kingdom", "UK", rdata$World$Country)


#### 나. 전세계 데이터(미국) ####
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")

rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  mutate(Country=ifelse(Country=="Diamond Princess", "Japan", Country)) %>% 
  group_by(Country, Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

# 날짜 및 나라별 사망자 수
rdata$DeathCases <- read_csv(rdata$url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  mutate(Country=ifelse(Country=="Diamond Princess", "Japan", Country)) %>% 
  group_by(Country, Variable) %>% summarise(Deaths=sum(Deaths)) %>% 
  rename("Country"=1,"Date"=2,"Deaths"=3)

# 날짜 및 나라별 완치자 수
rdata$recoveredCases <- read_csv(rdata$url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  mutate(Country=ifelse(Country=="Diamond Princess", "Japan", Country)) %>% 
  group_by(Country, Variable) %>% summarise(Recovered=sum(Recovered)) %>% 
  rename("Country"=1,"Date"=2,"Recovered"=3)


# 확진자, 사망자 합치기
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y"))

max(rdata$World$Date)

unique(rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("West Bank and Gaza", "Palestinian", rdata$World$Country)
rdata$World$Country <- gsub("United Kingdom", "UK", rdata$World$Country)
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("United Arab Emirates", "A. Emirates", rdata$World$Country)

rdata$World %>% filter(Country=="Sweden") %>% arrange(desc(Date))

#### 2. 데이터 비교 ####
library(gganimate); library(ggpubr); library(scales)

#### 가. 스웨덴 ####
data <- rdata$World %>% filter(Date>="2020-03-01") %>% 
  filter(Country=="Sweden" | Country=="Finland"| Country=="Norway"| 
           Country=="Denmark" | Country=="Japan")
data$Country <- factor(data$Country, levels=c("Sweden", "Norway", "Denmark", 
                                              "Finland", "Japan"))

ggline(data, x="Date", y="TotalTests", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.25, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="TestsperT", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="ConfirmedperM", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Deaths", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DeathsperM", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DeathRate", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)


#### 나. 누적확진자 대비 1일 확진자 ####
head(data <- rdata$World %>% arrange(Country, Date) %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
              Daily=Confirmed-lag(Confirmed)) %>% filter(Date=="2020-04-02") %>% 
       mutate(Group=ifelse(Country=="Finland", "Finland", 
                    ifelse(Country=="Norway", "Norway",
                    ifelse(Country=="Japan", "Japan", 
                    ifelse(Country=="Denmark", "Denmark", 
                    ifelse(Country=="Sweden", "Sweden", "Others")))))))
data$Group <- factor(data$Group, levels=c("Norway", "Denmark", "Japan", 
                                          "Sweden", "Finland", "Others"))
ggscatter(data, x="Confirmed", y="Daily", color="Group", 
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "grey"),
          label="Country", repel=T, 
          label.select=c("Norway", "Denmark", "Japan", "Sweden", "Finland", 
                         "Korea", "China", "US", "Italy", "Spain"))+
  scale_x_continuous(labels=comma, trans="log10", limits=c(100,300000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(10,50000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.005, 0.04, 0.3)), size=1,
              col=c("Black", "Black", "Black"), alpha=0.5,
              show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=200000, y=400, label="a=0.005", size=5, color="grey")+
  annotate("text", x=60000, y=1000, label="a=0.050", size=5, color="grey")+
  annotate("text", x=35000, y=30000, label="a=0.300", size=5, color="grey")

head(data <- rdata$World %>% arrange(Country, Date) %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
              Daily=Confirmed-lag(Confirmed)) %>% filter(Date==Sys.Date()) %>% 
       mutate(Group=ifelse(Country=="Finland", "Finland", 
                    ifelse(Country=="Norway", "Norway",
                    ifelse(Country=="Japan", "Japan", 
                    ifelse(Country=="Denmark", "Denmark", 
                    ifelse(Country=="Sweden", "Sweden", "Others")))))))
data$Group <- factor(data$Group, levels=c("Norway", "Denmark", "Japan", 
                                          "Sweden", "Finland", "Others"))
ggscatter(data, x="Confirmed", y="Daily", color="Group", 
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "grey"),
          label="Country", repel=T, 
          label.select=c("Norway", "Denmark", "Japan", "Sweden", "Finland", 
                         "Korea", "China", "US", "Italy", "Spain"))+
  scale_x_continuous(labels=comma, trans="log10", limits=c(100,300000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(10,50000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.005, 0.04, 0.3)), size=1,
              col=c("Black", "Black", "Black"), alpha=0.5,
              show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=200000, y=400, label="a=0.005", size=5, color="grey")+
  annotate("text", x=60000, y=1000, label="a=0.050", size=5, color="grey")+
  annotate("text", x=10000, y=10000, label="a=0.300", size=5, color="grey")



#### 가. 아리마 모델 (스웨덴) ####
library(forecast)
data <- rdata$World %>% 
  filter(Country=="Sweden" & Date>="2020-03-01") %>% 
  arrange(Date) %>% select("Confirmed")
auto.arima(data$Confirmed)
forecast(arima(ts(data$Confirmed), order=c(0,2,0)))
plot(forecast(arima(ts(data$Confirmed), order=c(0,2,0))))
