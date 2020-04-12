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




#### 2. 감염자 수 예측하기 ####
Inference <- function(country, Day){
  df <- list()
  # 실제 값
  Rnum <- rdata$data %>% filter(Country==country) %>% 
    filter(Date>Day-6 & Date<Day) %>% arrange(desc(Date)) %>% 
    mutate(DailyC=ifelse(DailyC==0, 0.01, DailyC)) %>% 
    mutate(DailyC=ifelse(Country=="Korea" & Date=="2020-03-23", 102, DailyC)) %>%
    mutate(DailyC=ifelse(Country=="Japan" & Date=="2020-04-05", 518, DailyC)) %>% 
    mutate(DailyC=ifelse(Country=="Japan" & Date=="2020-03-30", 130, DailyC)) %>% 
    mutate(DailyC=ifelse(Country=="Japan" & Date=="2020-03-16", 53, DailyC)) %>% 
    mutate(DailyC=ifelse(Country=="Japan" & Date=="2020-03-12", 60, DailyC)) %>%
    select("DailyC") %>% pull()
  # 감염력은 하루에 최소 0.01명부터 최대 100명까지
  beta <- 10^seq(log10(0.01), log10(100), length.out=10)
  
  # 감염자수는 최소 확진자부터 최대확진자의 100배까지
  I <- 10^seq(log10(min(Rnum)), log10(10*max(Rnum)), length.out=20) %>% round(0)
  
  # 최소 확진률(0보다 커야 한다. 우리나라 5일기준 0.1 추천)
  p <- 0.1
  
  # I + betaI - gammaI = Rnum 을 만족하는 gamma가 0에서 1사이의 값이면 오케이
  # 5일전 데이터 확인 오리지널 
  Ndays <- data.frame()
  for(i in I){
    for(b in beta){
      if(Rnum[5]/i>=p & Rnum[5]/i<=1){
        Ndays<-rbind(Ndays, 
                        data.frame(I0=round(i,0), b1=b, g1=Rnum[5]/i,
                                   I1=round(round(i,0)*(1+b-Rnum[5]/i),0), 
                                   C1=(Rnum[5]/i)*i))
      }
    }
  }
  
  # 4일전 데이터 확인
  N400 <- c("N41","N42","N43","N44","N45","N46","N47","N48","N49",
            "N410","N411","N412","N413","N414","N415","N416","N417","N418",
            "N419","N420")
  N300 <- c("N31","N32","N33","N34","N35","N36","N37","N38","N39",
            "N310","N311","N312","N313","N314","N315","N316","N317","N318",
            "N319","N320")
  N200 <- c("N21","N22","N23","N24","N25","N26","N27","N28","N29",
            "N210","N211","N212","N213","N214","N215","N216","N217","N218",
            "N219","N220")
  N100 <- c("N11","N12","N13","N14","N15","N16","N17","N18","N19",
            "N110","N111","N112","N113","N114","N115","N116","N117","N118",
            "N119","N120")
  for(num in 1:20){
    df[[N400[num]]] <- data.frame()
    df[[N300[num]]] <- data.frame()
    df[[N200[num]]] <- data.frame()
    df[[N100[num]]] <- data.frame()
  }
  
  for(num in 1:20){
    for(i in Ndays[Ndays$I0==I[num],4]){
      for(b in beta){
        if(Rnum[4]/i>=p & Rnum[4]/i<=1){
          df[[N400[num]]] <- 
            rbind(df[[N400[num]]],
                  data.frame(I0=I[num], b2=b, g2=Rnum[4]/i,
                             I2=round(i*(1+b-Rnum[4]/i),0), I1=i,
                             C2=(Rnum[4]/i)*i))
        }
      }
    }
  }
  # 0.328(13초) 0.1(21초), 0.05(44초), 0.01(168초, 2분 48초) 
  for(day in 3:1){
    for(num in 1:20){
      if(nrow(eval(parse(text=paste0("df$N",day+1,num))))>0){
        for(i in eval(parse(text=paste0("df$N",day+1,num)))[,4]){
          for(b in beta){
            if(Rnum[day]/i>=p & Rnum[day]/i<=1){
              if(day==3){
                df[[N300[num]]] <- 
                  rbind(df[[N300[num]]],
                        data.frame(I0=I[num], b2=b, g2=Rnum[day]/i,
                                   I2=round(i*(1+b-Rnum[day]/i),0), I1=i,
                                   C2=(Rnum[day]/i)*i))
              }
              if(day==2){
                df[[N200[num]]] <- 
                  rbind(df[[N200[num]]],
                        data.frame(I0=I[num], b2=b, g2=Rnum[day]/i,
                                   I2=round(i*(1+b-Rnum[day]/i),0), I1=i,
                                   C2=(Rnum[day]/i)*i))
              }
              if(day==1){
                df[[N100[num]]] <- 
                  rbind(df[[N100[num]]],
                        data.frame(I0=I[num], b2=b, g2=Rnum[day]/i,
                                   I2=round(i*(1+b-Rnum[day]/i),0), I1=i,
                                   C2=(Rnum[day]/i)*i))
              }
            }
          }
        }
      }
    }
  }
  result<-
    data.frame(I=I, 
               N4=c(nrow(df$N41), nrow(df$N42), nrow(df$N43), nrow(df$N44), 
                    nrow(df$N45), nrow(df$N46), nrow(df$N47), nrow(df$N48), 
                    nrow(df$N49), nrow(df$N410), nrow(df$N411), nrow(df$N412),
                    nrow(df$N413), nrow(df$N414), nrow(df$N415), nrow(df$N416), 
                    nrow(df$N417), nrow(df$N418), nrow(df$N419), nrow(df$N420)),
               N3=c(nrow(df$N31), nrow(df$N32), nrow(df$N33), nrow(df$N34), 
                    nrow(df$N35), nrow(df$N36), nrow(df$N37), nrow(df$N38), 
                    nrow(df$N39), nrow(df$N310), nrow(df$N311), nrow(df$N312),
                    nrow(df$N313), nrow(df$N314), nrow(df$N315), nrow(df$N316), 
                    nrow(df$N317), nrow(df$N318), nrow(df$N319), nrow(df$N320)),
               N2=c(nrow(df$N21), nrow(df$N22), nrow(df$N23), nrow(df$N24), 
                    nrow(df$N25), nrow(df$N26), nrow(df$N27), nrow(df$N28), 
                    nrow(df$N29), nrow(df$N210), nrow(df$N211), nrow(df$N212),
                    nrow(df$N213), nrow(df$N214), nrow(df$N215), nrow(df$N216), 
                    nrow(df$N217), nrow(df$N218), nrow(df$N219), nrow(df$N220)),
               N1=c(nrow(df$N11), nrow(df$N12), nrow(df$N13), nrow(df$N14), 
                    nrow(df$N15), nrow(df$N16), nrow(df$N17), nrow(df$N18), 
                    nrow(df$N19), nrow(df$N110), nrow(df$N111), nrow(df$N112),
                    nrow(df$N113), nrow(df$N114), nrow(df$N115), nrow(df$N116), 
                    nrow(df$N117), nrow(df$N118), nrow(df$N119), nrow(df$N120)))
  return(result[which.max(result$N1),1])
}

#### 가. 우리나라 ####
data <- data.frame(Date=Sys.Date()-6, 
                   Inum=Inference("Korea", Sys.Date()-1)) # 약22초
for(i in 2:50){
  data <- rbind(data, data.frame(Date=Sys.Date()-i-5, 
                                 Inum=Inference("Korea", Sys.Date()-i)))
}

Korea <- merge(rdata$data %>% filter(Country=="Korea") %>% 
                arrange(desc(Date)) %>% select("Date", "DailyC") %>% 
                filter(Date>="2020-02-17"), data, all.x=T) %>% melt(id.var="Date")

ggline(Korea, x="Date", y="value", color="variable", ylim=c(0,2800))

data <- data.frame(Date=Sys.Date()-6, 
                   Inum=Inference("Japan", Sys.Date()-1)) # 약22초
system.time(for(i in 2:50){
  data <- rbind(data, data.frame(Date=Sys.Date()-i-5, 
                                 Inum=Inference("Japan", Sys.Date()-i)))
}) # 50개 1830초(30분 30초)

Japan <- merge(rdata$data %>% filter(Country=="Japan") %>% 
                 arrange(desc(Date)) %>% select("Date", "DailyC") %>% 
                 filter(Date>="2020-02-17"), data, all.x=T) %>% melt(id.var="Date")

ggline(Japan, x="Date", y="value", color="variable", ylim=c(0,2800))
