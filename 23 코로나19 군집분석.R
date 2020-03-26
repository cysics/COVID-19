#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr)
rdata <- list()

#### 가. 전세계 확진자와 사망자 등 ####
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
rdata$data %>% filter(Country=="US" & Date<=Sys.Date()-1)

#### 나. 나라별 인구수와 국가를 신뢰하는 정도 ####
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2019") %>% select(c(1,3))   # 인구는 2019년 기준
rdata$Trust <- read_csv("data/Trust.csv") # 인구는 2018년 기준

# 국가 이름 확인
setdiff(rdata$Trust$Country, rdata$Population$Country)

head(rdata$TrustP <- merge(rdata$Trust, rdata$Population, by='Country'))

setdiff(rdata$TrustP$Country, rdata$World$Country)
unique(rdata$World$Country)
rdata$Population$Country
rdata$Population %>% filter(Country=="Timor")

# 이름 일치시키기(페로, 홍콩, 팔레스타인 제외)
rdata$TrustP$Country <- gsub("South Korea", "Korea", rdata$TrustP$Country)
rdata$TrustP$Country <- gsub("United States", "US", rdata$TrustP$Country)
rdata$TrustP$Country <- gsub("Czech Republic", "Czechia", 
                                 rdata$TrustP$Country)
rdata$TrustP$Country <- gsub("United Kingdom", "UK", rdata$TrustP$Country)
rdata$World$Country <- gsub("Bahamas, The", "Bahamas", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("Gambia, The", "Gambia", rdata$World$Country)
rdata$World$Country <- gsub("East Timor", "Timor", rdata$World$Country)


#### 다. 다 합치기 ####
# 검사자 수, 인구, 확진자, 사망자, 완치자 데이터 모두 합치기
head(rdata$data <- merge(rdata$TrustP, rdata$World, by='Country') %>% 
       arrange(Country, Date))

unique(rdata$data$Country)
rdata$data %>% filter(Country=="US") %>% mutate(max=max(Confirmed))

# 백만명당 확진자, 사망자 구하기
rdata$data <- rdata$data %>% mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeathsperM=Deaths*1000000/Population)



#### 2. 군집분석 ####
library(pheatmap); library(NbClust); library(factoextra)

#### 가. 기본 변수간 군집분석 ####
data <- rdata$data %>% filter(Date=="2020-03-25") %>% 
  select(-c("Country", "Date"))

pheatmap(cor(data, method="pearson"), cutree_rows=4, cutree_cols=4,
         display_numbers=T, fontsize=8, number_format="%.2f", number_color="grey0")


#### 나. 코로나19 확산 관련 ####
# 1000명 이상의 확진자를 낸 국가들 ####
rdata$data %>% group_by(Country) %>% filter(Date<Sys.Date()) %>% 
  filter(Confirmed>=1000) %>% distinct(Country) %>% pull() %>% length()

cutoff <-100
cutoff <-500
cutoff <-1000
data <- rdata$data %>% group_by(Country) %>% 
  filter(max(Confirmed)>=cutoff*10) %>% 
  filter(Confirmed>=cutoff & Confirmed<=cutoff*10) %>% 
  mutate(Days=max(Date)-min(Date)) %>% arrange(desc(Days)) %>% 
  mutate(Days=as.numeric(as.character(Days))) %>% filter(Date==max(Date)) %>% 
  select(c("Country", "Date", "Trust", "Days", "Confirmed", "Deaths")) %>% 
  ungroup() %>% column_to_rownames("Country") %>% 
  select(c("Days", "Confirmed", "Deaths"))

NbClust(scale(data), distance="euclidean", min.nc=2, max.nc=5, method="average")
# 군집수 추천 : cutoff100(4개), cutoff500(4개), cutoff1000(3개)

rdata$fit.hc <- data %>%  scale() %>% dist(method="euclidean") %>% 
  hclust(method="ward.D2")
# manhanttan or euclidean 유클리드 제곱거리(single, complete, average, mcquitty, median or centroid), 오차제곱합(ward.D, ward.D2)
fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=3.7, k=4,  #cutoff=100
          color_labels_by_k=T, rect=T)
fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=2, k=4,    #cutoff=500
          color_labels_by_k=T, rect=T)
fviz_dend(rdata$fit.hc, cex=1.3, labels_track_height=2, k=3,    #cutoff=1000
          color_labels_by_k=T, rect=T)


