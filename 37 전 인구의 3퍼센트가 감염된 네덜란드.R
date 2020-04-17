#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr);
rdata <- list()

#### 가. 전세계 데이터 #####
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")

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

##### 나. 나라별 인구수 합치기 ####
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2019") %>% select(c(1,3))   # 인구는 2019년 기준

# 국가 이름 맞추기 
setdiff(rdata$World$Country, rdata$Population$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)
rdata$Population$Country <- gsub("South Korea", "Korea", rdata$Population$Country)
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$Population$Country <- gsub("United States", "US", rdata$Population$Country)

# 확진자 수를 기준으로 데이터 합치기
rdata$WorldP <- merge(rdata$World, rdata$Population) %>% arrange(Country, Date)


#### 다. 검사자 수 합치기 ####
# Sys.setlocale("LC_ALL", "english")
# rdata$TotalTests <- read_csv("data/full-list-total-tests-for-covid-19.csv") %>% 
#   select(c(1,3:4)) %>% rename(Country=1, Date=2, TotalTests=3) %>% 
#   mutate(Date=lubridate::mdy(Date))
# Sys.setlocale("LC_ALL", "korean")

# github 공개
rdata$TotalTests <- read_csv(rdata$url[4]) %>% 
  separate(Entity, sep=" ", into=c("Country", "X1", "X2")) %>% 
  mutate(Country=ifelse(!grepl("-", X1), paste(Country, X1), Country)) %>% 
  select(c(1, 4, 8:10)) %>% 
  rename("Country"=1, "Date"=2, "TotalTests"=3, "DailyT"=4, "TestsperT"=5) %>% 
  filter(DailyT>0)

# 나라 이름 맞추기
setdiff(rdata$TotalTests$Country, rdata$WorldP$Country)
rdata$TotalTests$Country <- gsub("South Korea", "Korea", rdata$TotalTests$Country)
rdata$TotalTests$Country <- gsub("United States", "US", rdata$TotalTests$Country)
setdiff(rdata$TotalTests$Country, rdata$WorldP$Country)

# 데이터 합치기
rdata$data <- merge(rdata$TotalTests, rdata$WorldP, all=T) %>% 
  arrange(Country, Date) 
# unique(rdata$data$Country)
rdata$data$Country <- gsub("United Kingdom", "UK", rdata$data$Country)
rdata$data$Country <- gsub("United Arab Emirates", "A.Emirates", rdata$data$Country)
rdata$data$Country <- gsub("Dominican Republic", "Dominica", rdata$data$Country)

# 각종 값 계산
rdata$data <- rdata$data %>% group_by(Country) %>% 
  mutate(DailyC=Confirmed-lag(Confirmed),
         DailyT=TotalTests-lag(TotalTests),
         DailyD=Deaths-lag(Deaths),
         DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
         ConfirmedperM=Confirmed/Population,
         ConfirmedperT=Confirmed/TotalTests,
         DeathsperM=Deaths/Population,
         DeathsperT=Deaths/TotalTests,
         DeathRateperT=DeathRate/TotalTests) %>% ungroup(Country)

rdata$data %>% filter(Country=="Japan") %>% arrange(desc(Date))



#### 2. 네덜란드와 인접한 국가들 확진자수와 사망자 수 비교 ####
library(gganimate); library(ggpubr); library(scales)

data <- rdata$data %>% filter(Country=="Netherlands" | Country=="Belgium"|
                                Country=="UK"| Country=="Denmark" |
                                Country=="Germany") %>% 
  filter(Date>"2020-03-01")
data$Country <- factor(data$Country, levels=c("Netherlands", "Belgium", "Germany",
                                              "UK", "Denmark"))
ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Deaths", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="TestsperT", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="DeathRate", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)


#### 3. 누적확진자 대비 1일 확진자 ####
head(data <- rdata$data %>% arrange(Country, Date) %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
              DailyC=Confirmed-lag(Confirmed)) %>% filter(Date=="2020-04-01") %>% 
       filter(DailyC>0) %>% 
       mutate(Group=ifelse(Country=="Netherlands", "Netherlands", 
                    ifelse(Country=="Belgium", "Belgium",
                    ifelse(Country=="UK", "UK", 
                    ifelse(Country=="Germany", "Germany", 
                    ifelse(Country=="Japan", "Japan", "Others")))))))
data$Group <- factor(data$Group, levels=c("Netherlands", "Belgium", "UK", 
                                          "Germany", "Japan", "Others"))
ggscatter(data, x="Confirmed", y="DailyC", color="Group", title="2020-04-01",
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("red", "darkgreen", "darkviolet", "blue", "orange", "grey"),
          label="Country", repel=T, label.select=c( "Japan", 
            "Sweden", "Finland", "Korea", "China", "US", "Italy", "Spain",
            "Netherlands", "Belgium", "UK", "Germany", "Taiwan", 
            "Singapore", "Austria"))+ # "Thailand", "Israel", "Switzerland", 
  scale_x_continuous(labels=comma, trans="log10", limits=c(100,700000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(20,80000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10))

head(data <- rdata$data %>% arrange(Country, Date) %>% 
       mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed),
              DailyC=Confirmed-lag(Confirmed)) %>% filter(Date==Sys.Date()-1) %>% 
       filter(DailyC>0) %>% 
       mutate(Group=ifelse(Country=="Netherlands", "Netherlands", 
                    ifelse(Country=="Belgium", "Belgium",
                    ifelse(Country=="UK", "UK", 
                    ifelse(Country=="Germany", "Germany", 
                    ifelse(Country=="Japan", "Japan", "Others")))))))
data$Group <- factor(data$Group, levels=c("Netherlands", "Belgium", "UK", 
                                          "Germany", "Japan", "Others"))
ggscatter(data, x="Confirmed", y="DailyC", color="Group", title="2020-04-16",
      legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
      palette=c("red", "darkgreen", "darkviolet", "blue", "orange", "grey"),
      label="Country", repel=T, label.select=c("Denmark", "Japan", 
        "Finland", "Korea", "China", "US", "Italy", "Spain", # "Sweden", 
        "Netherlands", "Belgium", "UK", "Germany", "Taiwan", "Australia",
        "Singapore", "Austria"))+ # "Thailand", "Israel", "Switzerland", 
      scale_x_continuous(labels=comma, trans="log10", limits=c(100,700000)) + 
      scale_y_continuous(labels=comma, trans="log10", limits=c(20,80000)) +
      theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10))

ggscatter(data, x="Confirmed", y="DailyD", color="Group", 
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("red", "darkgreen", "darkviolet", "blue", "orange", "grey"),
          label="Country", repel=T, label.select=c("Denmark", "Japan", 
            "Finland", "Korea", "China", "US", "Italy", "Spain", # "Sweden", 
            "Netherlands", "Belgium", "UK", "Germany", "Taiwan", "Australia",
            "Singapore", "Austria"))+ # "Thailand", "Israel", "Switzerland", 
  scale_x_continuous(labels=comma, trans="log10", limits=c(100,700000)) + 
  scale_y_continuous(labels=comma, trans="log10") +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10))



#### 4. 네델란드와 비슷한 국가들 ####
#### 가. 변수간 군집분석 ####
library(pheatmap); library(NbClust); library(factoextra)
data <- rdata$data %>% select(c("Confirmed", "Deaths", "Recovered", "DailyC", 
  "DailyD", "DeathRate", "ConfirmedperM", "DeathsperM", "TotalTests")) %>% 
  filter(Confirmed>1000) %>% na.omit

pheatmap(cor(data, method="pearson"), cutree_rows=4, cutree_cols=4,
         display_numbers=T, fontsize=8, number_format="%.2f", number_color="grey0")



#### 나. 군집분석 ####
rdata$data %>% filter(Country=="Netherlands") %>% arrange(desc(Date))
rdata$cutoff <-1000
rdata$times <- 10

rdata$cutoff <-9000
rdata$times <- 2
data <- rdata$data %>% group_by(Country) %>% filter(!is.na(Confirmed)) %>% 
  filter(max(Confirmed)>=rdata$cutoff*rdata$times) %>% 
  filter(Confirmed>=rdata$cutoff & Confirmed<=rdata$cutoff*rdata$times) %>% 
  mutate(Days=max(Date)-min(Date)+1) %>% arrange(desc(Days)) %>% 
  mutate(Days=as.numeric(as.character(Days))) %>% 
  arrange(desc(Confirmed)) %>% slice(1) %>% 
  select(c("Country", "Days", "Confirmed", "Recovered", 
           "DeathRate", "ConfirmedperM")) %>% 
  ungroup() %>% column_to_rownames("Country") %>% 
  select(c("Days", "Confirmed", "Recovered", 
           "DeathRate", "ConfirmedperM")) %>% 
  na.omit

NbClust(scale(data), distance="euclidean", min.nc=2, max.nc=6, method="average")
# 천명에서 만명(5개 추천)

rdata$fit.hc <- data %>% scale() %>% dist(method="euclidean") %>% 
  hclust(method="ward.D2")

fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=2,
          color_labels_by_k=T, rect=T)
fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=5,  
          color_labels_by_k=T, rect=T)

data %>% melt() %>% FSA::Summarize(value~variable, .) %>% 
  mutate_if(is.numeric, round, 2)
# 4월 16일 기준 1만명을 초과하는 국가 23개국
# 천명이 만명 되는데 평균 14일 걸리고 보통 530여명 증가하면서 만명 초과
# 해당 기간 동안 회복된 사람은 859명, 평균 사망률은 2.87%이다.