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

max(rdata$World$Date)


##### 나. 나라별 인구수 ####
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2017") %>% select(c(1,3))   # 인구는 2019년 기준

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
  rename("Country"=1, "Date"=2, "TotalTests"=3, "DailyT"=4, "TestsperT"=5)

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

rdata$data %>% filter(Country=="Korea") %>% arrange(desc(Date))
rdata$data %>% filter(Country=="Japan") %>% arrange(desc(Date))

#### 다. 다른 질병에 대한 정보 ####
rdata$Diseases <- readxl::read_excel("data/DiseaseComparison.xlsx") %>% 
  melt(id.var=c("Name")) %>% 
  mutate(variable=as.Date(as.numeric(variable), origin="2019-12-31")) %>% 
  arrange(Name)
# 엑셀 날짜 -> 숫자로 변경 -> 날짜인데 시작날짜 지정하기
# janitor 패키지의 excel_numeric_to_date() 이용하기


#### 2 그래프로 표현 ####
data <- rdata$Diseases %>% group_by(variable) %>% 
  arrange(variable, -value) %>% mutate(Rank=1:n()) %>%
  ungroup() %>% filter(Rank<=10 & variable<="2020-04-12")
   

result <- ggplot(data, aes(Rank, group=Name, fill=Name, color=Name)) +
  geom_tile(aes(y=value/2, height=value, width=0.9), alpha=0.8, color=NA) +
  geom_text(aes(y=0, label=paste(Name, " ")), vjust=0.2, hjust=1, size=7) +
  geom_text(aes(y=value, label=paste0(" ", comma(value))), hjust=0, size=7)+ 
  coord_flip(clip="off", expand=F) + scale_x_reverse() + 
  guides(color=F, fill=F) + scale_y_continuous(labels=comma) +
  labs(title="Aggregated disease comparison", 
       subtitle="{closest_state}", x = "", y = "Deaths") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin=margin(20, 80, 20, 140),
        text=element_text(size=20)) +
  transition_states(variable, transition_length=200000, state_length=2000) + 
  ease_aes('cubic-in-out')

animate(result, 1200, fps=10, duration=120, width=800, height=600, 
        renderer=gifski_renderer("DiseaseComparison.gif"))

animate(result, 1200, fps=10, duration=120, width=1280, height=720, 
        renderer=av_renderer("DiseaseComparison.mp4"))
