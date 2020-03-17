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

# 확진자, 사망자, 완치자 합치기 (3월 11일과 12일만)
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  filter(Date=="2020-03-11" | Date=="2020-03-12") %>% 
  arrange(Country, Date) %>% mutate(DayConfirmed=0)

# 타이완에 *표 없애기, 우리나라 Korea로 표현하기 
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)

# 3월 12일 데이터에서 11일 데이터를 빼서 1일 확진자 수 구하기
for(i in 1:(nrow(rdata$World)/2)){
  rdata$World[i*2,7] <- abs(rdata$World[i*2, 3]-rdata$World[i*2-1, 3])
}

# 3월 12일 데이터만 남기기
rdata$World <- rdata$World %>% filter(Date=="2020-03-12")


#### 나. 나라별 총검사자 수와 인구수 ####
rdata$TotalTests <- read_csv("data/TotalTests.csv")
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2019") %>% select(c(1,3))   # 인구는 2019년 기준

# 국가 이름 확인
rdata$Population$Country
rdata$TotalTests$Country

# 국가이름 일치시키기
rdata$TotalTests$Country <- gsub("Australia - New South Wales", 
                                 "Australia", rdata$TotalTests$Country)
rdata$TotalTests$Country <- gsub("Canada - Alberta", 
                                 "Canada", rdata$TotalTests$Country)
rdata$TotalTests$Country <- gsub("China - Guangdong", 
                                 "China", rdata$TotalTests$Country)
rdata$TotalTests$Country <- gsub("United States - CDC samples tested", 
                                 "US", rdata$TotalTests$Country)
rdata$Population$Country <- gsub("United States", 
                                "US", rdata$Population$Country)

# 검사자 수와 인구수 합치기
rdata$Tests <- merge(rdata$TotalTests, rdata$Population)

# 긴 이름 줄이고, 우리나라 Korea로 표현하기
rdata$Tests$Country <- gsub("Czech Republic", "Czechia", rdata$Tests$Country)
rdata$Tests$Country <- gsub("South Korea", "Korea", rdata$Tests$Country)

#### 다. 모두 합치기 ####
# 검사자 수, 인구, 확진자, 사망자, 완치자 데이터 모두 합치기
head(data <- merge(rdata$Tests, rdata$World, all.x=T))
data$Country <- gsub("United Kingdom", "UK", data$Country)

# 백만명당 검사자, 확진자, 사망자 구하기
data <- data %>% mutate(TestsperM=TotalTests*1000000/Population) %>% 
  mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeatsperM=Deaths*1000000/Population)


#### 2. 랭킹 비교 ####
#### 가. 총검사자 수 ####
library(ggpubr); library(scales)
Rank <- data %>% group_by(Date) %>% mutate(Rank=min_rank(-TotalTests)*1) %>% 
  ungroup() %>% filter(Rank<10)

ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=TotalTests/2, height=TotalTests, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=TotalTests, label=paste0(" ",comma(TotalTests, accuracy=1))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_y_continuous(labels=comma) +
  scale_x_reverse() + guides(color=F, fill=F) +
  labs(title='2020-03-12', x = "", y = "Total Tests") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 60, 20, 60), text=element_text(size=18))

#### _2) 백만명당 검사자 수 ####
Rank <- data %>% group_by(Date) %>% mutate(Rank=min_rank(-TestsperM)*1) %>% 
  ungroup() %>% filter(Rank<10)

ggplot(Rank, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=TestsperM/2, height=TestsperM, width=0.9), 
            alpha=0.8, color=NA)+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=TestsperM, label=paste0(" ",comma(TestsperM, accuracy=1))), 
            hjust=0, size=5)+ 
  coord_flip(clip="off", expand=F) + scale_y_continuous(labels=comma) +
  scale_x_reverse() + guides(color=F, fill=F) +
  labs(title='2020-03-12', x = "", y = "Tests per million") +
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 60, 20, 60), text=element_text(size=18))


#### 3. 상관관계 분석 ####
cor.test(data$TestsperM, data$ConfirmedperM)

# 백만명당 검사자에 따른 백만명당 확진자 수
ggscatter(data, x="TestsperM", y="ConfirmedperM", 
          xlab="Tests performed per million", 
          ylab="Confirmed cases per million",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=2000, label.y=250, label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Bahrain", "Korea", "Iceland", "Norway",
                         "Slovenia", "Italy", "France", "Austria",
                         "Taiwan", "Denmark", "Russia", "Netherlands",
                         "Japan", "US", "China", "Canada"))

# 백만명당 검사자에 따른 새로운 확진자 수
ggscatter(data, x="TestsperM", y="DayConfirmed", 
          xlab="Tests performed per million", 
          ylab="New Confirmed cases per million",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=2000, label.y=300, label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Bahrain", "Korea", "Iceland", "Norway",
                         "Denmark", "Canada", "France", "Austria",
                         "Japan", "US", "China"))

# 백만명당 검사자에 따른 백만명당 사망자 수
ggscatter(data, x="TestsperM", y="DeatsperM", 
          xlab="Tests performed per million", ylab="Death cases per million",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=2000, label.y=10, label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Bahrain", "Korea", "Iceland", "Norway",
                         "Italy", "France", "Taiwan", 
                         "Japan", "US", "China"))

# 백만명당 검사자에 따른 사망률 수
ggscatter(data, x="TestsperM", y="DeathRate", 
          xlab="Tests performed per million", 
          ylab="Death Rate",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=2000, label.y=5, label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Bahrain", "Korea", "Iceland", "Norway",
                         "Italy", "India", "France",
                         "Taiwan", "Poland", 
                         "Japan", "US", "China", "Philippines"))






