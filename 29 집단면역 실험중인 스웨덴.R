#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr);
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

# 사망률과 1일 확진자 계산하기
head(rdata$data <- rdata$World %>% arrange(Country, Date) %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
              Daily=Confirmed-lag(Confirmed)) %>% filter(Date>"2020-02-01") %>% 
       mutate(Group=ifelse(Country=="Korea", "Korea", 
                    ifelse(Country=="US", "US",
                    ifelse(Country=="Japan", "Japan", 
                    ifelse(Country=="China", "China", 
                    ifelse(Country=="Sweden", "Sweden", "Others")))))))

data$Group <- factor(data$Group, levels=c("Korea", "China", "US", 
                                          "Japan", "Sweden", "Others"))
max(data$Date)



#### 2. 스웨덴, 핀란드, 노르웨이, 덴마크 ####
filter <- dplyr::filter
unique(rdata$data$Country)
data <- rdata$data %>% filter(Country=="Sweden" | Country=="Finland"|
                                Country=="Norway"| Country=="Denmark" |
                                Country=="Japan")
data$Country <- factor(data$Country, levels=c("Sweden", "Norway", "Denmark", 
                                          "Finland", "Japan"))
ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Deaths", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)


#### 3. 누적확진자 대비 1일 확진자 ####
library(gganimate); library(scales); library(ggpubr)
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
print(result <- ggscatter(data, x="Confirmed", y="Daily", color="Group", 
      legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
      palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "grey"),
      label="Country", repel=T, label.select=c("Norway", "Denmark", "Japan", 
        "Sweden", "Finland", "Korea", "China", "US", "Italy", "Spain"))+
      scale_x_continuous(labels=comma, trans="log10", limits=c(100,300000)) + 
      scale_y_continuous(labels=comma, trans="log10", limits=c(20,50000)) +
      theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
      transition_time(Date) + labs(title = "{frame_time}"))

animate(result, 600, fps=10, duration=60, end_pause=50, width=500, height=400, 
        renderer=gifski_renderer("DeathEvolution2.gif"))  