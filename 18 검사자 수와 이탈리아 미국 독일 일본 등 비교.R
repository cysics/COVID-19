#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr)
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

# 확진자, 사망자, 완치자 합치기
rdata$World <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  arrange(Country, Date) %>% mutate(DayConfirmed=0)

# 타이완에 *표 없애기, 우리나라 Korea로 표현하기, 국가 이름 일치시키기
rdata$World$Country <- gsub("Taiwan\\*", "Taiwan", rdata$World$Country)
rdata$World$Country <- gsub("Korea\\, South", "Korea", rdata$World$Country)
rdata$World$Country <- gsub("The Bahamas", "Bahamas", rdata$World$Country)
rdata$World$Country <- gsub("North Macedonia", "Macedonia", rdata$World$Country)
rdata$World$Country <- gsub("Republic of the Congo", "Congo", rdata$World$Country)


#### 나. 나라별 총검사자 수와 인구수 ####
#### _1) 3월 20일자 데이터 ####
rdata$TotalTests <- read_csv("data/TestsVSConfirmed.csv")
rdata$TotalTests <- rdata$TotalTests[!is.na(rdata$TotalTests$TotalTests),]
rdata$Population <- read_csv("data/Population.csv") %>% 
  filter(Year=="2019") %>% select(c(1,3))   # 인구는 2019년 기준

# 국가 이름 확인하고 일치시키기 
setdiff(unique(rdata$TotalTests$Country), rdata$Population$Country)
rdata$Population$Country
rdata$TotalTests$Country
# rdata$TotalTests[!is.na(str_match(rdata$TotalTests$Country, 'Australia')),]
# rdata$TotalTests[!is.na(str_match(rdata$TotalTests$Country, 'Canada')),]
# rdata$TotalTests[!is.na(str_match(rdata$TotalTests$Country, 'China')),]
# rdata$TotalTests[!is.na(str_match(rdata$TotalTests$Country, 'United States')),]
rdata$TotalTests$Country <- gsub("China - Guangdong", "China", 
                                 rdata$TotalTests$Country)

# 검사자 수와 인구수 합치기
rdata$Tests <- merge(rdata$TotalTests, rdata$Population) %>% 
  mutate(TestsperM=TotalTests*1000000/Population) %>% 
  mutate(ConfirmedperM=ConfirmedCases*1000000/Population)

# 모두 합치기
# 국가 이름 확인
setdiff(rdata$Tests$Country, rdata$World$Country)
unique(rdata$World$Country)

# 이름 일치시키기(페로, 홍콩, 팔레스타인 제외)
rdata$Tests$Country <- gsub("Czech Republic", "Czechia", rdata$Tests$Country)
rdata$Tests$Country <- gsub("South Korea", "Korea", rdata$Tests$Country)
rdata$Tests$Country <- gsub("United States", "US", rdata$Tests$Country)

# 검사자 수, 인구, 확진자, 사망자, 완치자 데이터 모두 합치기
head(data <- merge(rdata$Tests, rdata$World, by='Country') %>% 
       arrange(Country, Date))

# 긴 이름 줄이기
unique(data$Country)
data$Country <- gsub("United Kingdom", "UK", data$Country)
data$Country <- gsub("United Arab Emirates", "A.Emirates", data$Country)

# 백만명당 확진자, 사망자 구하기
data <- data %>% filter(Date=="2020-03-20") %>% 
  mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
  mutate(DeathsperM=Deaths*1000000/Population) %>% 
  mutate(Group=ifelse(Country=="Korea" | Country=="US" | Country=="Italy" | 
         Country=="Japan" | Country=="France" | Country=="Taiwan" | 
         Country=="Russia" | Country=="UK",  "A", "B"))



#### _2) 3월 12일자 데이터 ####
rdata$TotalTestsP <- read_csv("data/TotalTests.csv")

# 국가이름 일치시키기
setdiff(rdata$TotalTestsP$Country, rdata$Population$Country)
rdata$TotalTestsP$Country <- gsub("Australia - New South Wales", 
                                 "Australia", rdata$TotalTestsP$Country)
rdata$TotalTestsP$Country <- gsub("Canada - Alberta", 
                                 "Canada", rdata$TotalTestsP$Country)
rdata$TotalTestsP$Country <- gsub("China - Guangdong", 
                                 "China", rdata$TotalTestsP$Country)
rdata$TotalTestsP$Country <- gsub("United States - CDC samples tested", 
                                 "United States", rdata$TotalTestsP$Country)

# 검사자 수와 인구수 합치기
rdata$TestsP <- merge(rdata$TotalTestsP, rdata$Population) %>% 
  mutate(TestsperM=TotalTests*1000000/Population) 

# 3월 12일자 확진자, 사망자, 완치자 데이터 
rdata$WorldP <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
  by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  mutate(DeathRate=ifelse(Confirmed==0, 0, 100*Deaths/Confirmed)) %>% 
  filter(Date=="2020-03-11" | Date=="2020-03-12") %>% 
  arrange(Country, Date) %>% mutate(DayConfirmed=0)

# 이름 일치시키기
setdiff(rdata$TestsP$Country, unique(rdata$WorldP$Country))
rdata$TestsP$Country <- gsub("Czech Republic", "Czechia", rdata$TestsP$Country)
rdata$TestsP$Country <- gsub("South Korea", "Korea", rdata$TestsP$Country)
rdata$WorldP$Country <- gsub("Korea\\, South", "Korea", rdata$WorldP$Country)
rdata$WorldP$Country <- gsub("Taiwan\\*", "Taiwan", rdata$WorldP$Country)
rdata$TestsP$Country <- gsub("United States", "US", rdata$TestsP$Country)

# 3월 12일 데이터에서 11일 데이터를 빼서 1일 확진자 수 구하기
for(i in 1:(nrow(rdata$WorldP)/2)){
  rdata$WorldP[i*2,7] <- abs(rdata$WorldP[i*2, 3]-rdata$WorldP[i*2-1, 3])
}

# 3월 12일 데이터만 남기기
rdata$WorldP <- rdata$WorldP %>% filter(Date=="2020-03-12")

# 모두 합치기
# 검사자 수, 인구, 확진자, 사망자, 완치자 데이터 모두 합치기
head(rdata$dataP <- merge(rdata$TestsP, rdata$WorldP, all.x=T) %>% 
       mutate(ConfirmedperM=Confirmed*1000000/Population) %>% 
       mutate(DeathsperM=Deaths*1000000/Population) %>% 
       rename(ConfirmCases=6))
rdata$dataP$Country <- gsub("United Kingdom", "UK", rdata$dataP$Country)

print(rdata$dataPS <- rdata$dataP %>% rename(ConfirmedCases=6) %>% 
  filter(Country=="Korea" | Country=="US" | Country=="Italy" | Country=="Japan" |
         Country=="Iran" | Country=="France" | Country=="Taiwan" |
         Country=="Russia" | Country=="UK")) %>% 
  mutate(DeathsperM=Deaths*1000000/Population)


#### 2. 랭킹 비교 ####
#### 가. 총검사자 수 ####
#### _1) 3월 12일 ####
library(ggpubr); library(scales)

Rank12 <- rdata$dataP %>% mutate(Rank=min_rank(-TotalTests)*1) %>% filter(Rank<11)

ggplot(Rank12, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=TotalTests/2, height=TotalTests, width=0.9), 
            alpha=0.8, color=NA)+
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=TotalTests, label=paste0(" ",comma(TotalTests, accuracy=1))), 
            hjust=0, size=5)+ 
  labs(title='2020-03-12', x = "", y = "Total Tests") +
  theme(plot.title=element_text(hjust=0, size=18),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 60, 20, 60), text=element_text(size=18))


#### _2) 3월 20일 ####
rdata$Tests$Country <- gsub("South Korea", "Korea", rdata$Tests$Country)
rdata$Tests$Country <- gsub("United States", "US", rdata$Tests$Country)
rdata$Tests$Country <- gsub("United Kingdom", "UK", rdata$Tests$Country)
rdata$Tests$Country <- gsub("Faeroe Islands", "Faeroe", rdata$Tests$Country)
rdata$Tests$Country <- gsub("United Arab Emirates", "A.Emirates", 
                            rdata$Tests$Country)

Rank20 <- rdata$Tests %>% mutate(Rank=min_rank(-TotalTests)*1) %>% filter(Rank<11)

ggplot(Rank20, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=TotalTests/2, height=TotalTests, width=0.9), 
            alpha=0.8, color=NA)+
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=TotalTests, label=paste0(" ",comma(TotalTests, accuracy=1))), 
            hjust=0, size=5)+ 
  labs(title='2020-03-20', x = "", y = "Total Tests") +
  theme(plot.title=element_text(hjust=0, size=18),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 60, 20, 60), text=element_text(size=18))

#### _3) 두 시점 비교 ####
rdata$data12 <- rdata$dataP %>% filter(Country=="Korea" | Country=="US" | 
            Country=="Italy" | Country=="Japan" | Country=="France" | 
            Country=="Taiwan" | Country=="Russia" | Country=="UK") %>% 
  select(c(1, 2, 4, 6)) %>% rename(ConfirmedCases=4)

rdata$data20 <- rdata$Tests %>% filter(Country=="Korea" | Country=="US" | 
            Country=="Italy" |Country=="Japan" | Country=="France" | 
            Country=="Taiwan" | Country=="Russia" | Country=="UK") %>% 
  select(c(1, 3, 6, 4))

dataCom <- rbind(data.frame(Date="2020-03-12", rdata$data12),
                 data.frame(Date="2020-03-20", rdata$data20))

dataCom$Country <- factor(dataCom$Country , 
    levels=c("Japan", "Taiwan", "France", "UK", "US", "Russia", "Italy", "Korea"))

ggplot(dataCom, aes(Country, y=TotalTests, fill=Date)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_tile(aes(y=TotalTests/2, height=TotalTests, width=0.9), 
            alpha=0.8, color=NA, stat="identity", 
            position=position_dodge(width=1))+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(x=Country, y=TotalTests, label=paste0(" ", 
            comma(TotalTests, accuracy=1))), hjust=0, size=5,
            position=position_dodge(width=1),
            inherit.aes=T) + guides(fill=F) +
  coord_flip(clip="off", expand=F) + scale_y_continuous(labels=comma) +
  labs(x = "", y = "Total Tests") + scale_fill_brewer(palette="Paired")+
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 80, 20, 60), text=element_text(size=18))



#### 나. 백만명당 검사자 수 ####
#### _1) 3월 12일 비교 ####
Rank12 <- rdata$dataP %>% mutate(Rank=min_rank(-TestsperM)*1) %>% filter(Rank<11)

ggplot(Rank12, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=TestsperM/2, height=TestsperM, width=0.9), 
            alpha=0.8, color=NA)+
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=TestsperM, label=paste0(" ",comma(TestsperM, accuracy=1))), 
            hjust=0, size=5)+ 
  labs(title='2020-03-12', x = "", y = "Total Tests per million") +
  theme(plot.title=element_text(hjust=0, size=18),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 60, 20, 60), text=element_text(size=18))

#### _2) 3월 20일 비교 ####
Rank20 <- rdata$Tests %>% mutate(Rank=min_rank(-TestsperM)*1) %>% filter(Rank<11)

ggplot(Rank20, aes(Rank, group=Country, fill=Country)) +
  geom_tile(aes(y=TestsperM/2, height=TestsperM, width=0.9), 
            alpha=0.8, color=NA)+
  coord_flip(clip="off", expand=F) + scale_x_reverse() +
  scale_y_continuous(labels=comma) + guides(color=F, fill=F) +
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(y=TestsperM, label=paste0(" ",comma(TestsperM, accuracy=1))), 
            hjust=0, size=5)+ 
  labs(title='2020-03-20', x = "", y = "Total Tests per million") +
  theme(plot.title=element_text(hjust=0, size=18),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 60, 20, 60), text=element_text(size=18))

#### _3) 두 시점 비교 ####
dataCom$Country <- factor(dataCom$Country , 
  levels=c("Japan", "US", "France", "Taiwan", "UK", "Russia", "Italy", "Korea"))

ggplot(dataCom, aes(Country, y=TestsperM, fill=Date)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_tile(aes(y=TestsperM/2, height=TestsperM, width=0.9), 
            alpha=0.8, color=NA, stat="identity", 
            position=position_dodge(width=1))+
  geom_text(aes(y=0, label=paste(Country, " ")), vjust=0.2, hjust=1, size=5) +
  geom_text(aes(x=Country, y=TestsperM, label=paste0(" ", 
            comma(TestsperM, accuracy=1))), hjust=0, size=5,
            position=position_dodge(width=1),
            inherit.aes=T) + guides(fill=F) +
  coord_flip(clip="off", expand=F) + scale_y_continuous(labels=comma) +
  labs(x = "", y = "Total Tests per million")+scale_fill_brewer(palette="Paired")+
  theme(plot.title=element_text(hjust=0, size=20),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
        plot.margin=margin(20, 80, 20, 60), text=element_text(size=18))

#### 3. 상관관계 분석 ####
#### 가. 검사자 수에 따른 확진자 수 ####
# 검사자에 따른 확진자 수1 : 전체 표시
ggscatter(rdata$Tests, x="TotalTests", y="ConfirmedCases", 
          xlab="Total Tests", ylab="Confirmed cases", 
          palette=c("blue", "gray"), legend="none",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
               label.sep = ",", col="Red"), label="Country", repel=T,
          label.select=c("Korea", "Italy", "Iran", "US", "UK", "Germany",
                         "Spain", "France", "Austria", "Japan", "Norway",
                         "Canada", "Australia", "Russia", "A.Emirates",
                         "Finland", "Pakistan", "Philippines", "Indonesia", 
                         "Taiwan", "Vietnam", "Belarus", "Palestine",
                         "Malta", "India", "Bahrain", "China", "Ukraine"))+
  yscale ("log10", .format=T) + xscale ("log10", .format=T)

# 검사자에 따른 확진자 수2 : 주요 관심 국가 분리
rdata$Tests <- rdata$Tests %>% 
  mutate(Group=ifelse(Country=="Korea" | Country=="US" | Country=="Italy" | 
                      Country=="Japan" | Country=="France" | Country=="Taiwan" | 
                      Country=="Russia" | Country=="UK",  "A", "B"))

ggscatter(rdata$Tests, x="TotalTests", y="ConfirmedCases", 
          xlab="Total Tests", ylab="Confirmed cases", color="Group",
          palette=c("blue", "gray"), legend="none",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
               label.sep = ",", col="Red"), label="Country", repel=T,
          label.select=c("Korea", "Italy", "Iran", "US", "UK", "Germany",
                         "Spain", "France", "Austria", "Japan", "Norway",
                         "Canada", "Australia", "Russia", "A.Emirates",
                         "Finland", "Pakistan", "Philippines", "Indonesia", 
                         "Taiwan", "Vietnam", "Belarus", "Palestine",
                         "Malta", "India", "Bahrain", "China", "Ukraine"))+
  yscale ("log10", .format=T) + xscale ("log10", .format=T)

# 검사자에 따른 확진자 수3 : 3월 12일 대비 증감 확인
ggscatter(rdata$Tests, x="TotalTests", y="ConfirmedCases", 
          xlab="Total Tests", ylab="Confirmed cases", color="Group",
          palette=c("blue", "gray"), legend="none",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.sep = ",", col="Red"), label="Country", repel=T,
          label.select=c("Korea", "Italy", "Iran", "US", "UK", "Germany",
                         "Spain", "France", "Austria", "Japan", "Norway",
                         "Canada", "Australia", "Russia", "A.Emirates",
                         "Finland", "Pakistan", "Philippines", "Indonesia", 
                         "Taiwan", "Vietnam", "Belarus", "Palestine",
                         "Malta", "India", "Bahrain", "China", "Ukraine"))+
  yscale ("log10", .format=T) + xscale ("log10", .format=T)+
  geom_point(data=rdata$dataPS, colour="red", size=2, label="Country")+
  geom_text(data=rdata$dataPS, aes(label=Country), hjust=1, vjust=1, col="red")

# 검사자에 따른 확진자 수4 : 기울기가 일정한 선 표시
ggscatter(rdata$Tests, x="TotalTests", y="ConfirmedCases", 
          xlab="Total Tests", ylab="Confirmed cases", color="Group",
          palette=c("blue", "gray"), legend="none",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.sep = ",", col="Red"), label="Country", repel=T,
          label.select=c("Korea", "Italy", "Iran", "US", "UK", "Germany",
                         "Spain", "France", "Austria", "Japan", "Norway",
                         "Canada", "Australia", "Russia", "A.Emirates",
                         "Finland", "Pakistan", "Philippines", "Indonesia", 
                         "Taiwan", "Vietnam", "Belarus", "Palestine",
                         "Malta", "India", "Bahrain", "China", "Ukraine"))+
  yscale ("log10", .format=T) + xscale ("log10", .format=T)+
  geom_point(data=rdata$dataPS, colour="red", size=2, label="Country")+
  geom_text(data=rdata$dataPS, aes(label=Country), hjust=1, vjust=1, col="red")+
  geom_abline(data.frame(s=c(0.001, 0.005, 0.03, 0.15)), size=1,
       col=c("darkviolet", "darkviolet", "darkviolet", "darkviolet"), alpha=0.5,
       show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=14000, y=10, label="a=0.001", size=4, color="darkviolet")+
  annotate("text", x=3000, y=10, label="a=0.005", size=4, color="darkviolet")+
  annotate("text", x=1000, y=20, label="a=0.030", size=4, color="darkviolet")+
  annotate("text", x=400, y=100, label="a=0.150", size=4, color="darkviolet")


#### 나. 백만명당 확진자 수 ####
ggscatter(rdata$Tests, x="TestsperM", y="ConfirmedperM", 
          xlab="Tests performed per million", color="Group",
          ylab="Confirmed cases per million", legend="none",
          palette=c("blue", "gray"), add="reg.line", conf.int=T, 
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Italy", "Korea", "Spain", "US", "Iran", "Estonia",
                         "Faeroe", "Iceland", "Austraia", "Finland", "India",
                         "France", "Germany", "UK", "Austria", "Slovenia",
                         "Japan", "Taiwan", "China", "Bahrain", "Denmark",
                         "Norway", "Australia", "A.Emirates", "Belarus",
                         "Russia", "Vietnam", "Brazil", "india", "Philippines",
                         "Pakistan", "Indonesia", "Ukraine", "China"))+
  yscale ("log10", .format=T) + xscale ("log10", .format=T)+
  geom_point(data=rdata$dataPS, colour="red", size=2, label="Country")+
  geom_text(data=rdata$dataPS, aes(label=Country), hjust=1, vjust=1, col="red")+
  geom_abline(data.frame(s=c(0.001, 0.005, 0.03, 0.15)), size=1,
       col=c("darkviolet", "darkviolet", "darkviolet", "darkviolet"), alpha=0.5,
       show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=800, y=0.35, label="a=0.001", size=4, color="darkviolet")+
  annotate("text", x=120, y=0.30, label="a=0.005", size=4, color="darkviolet")+
  annotate("text", x=60, y=0.8, label="a=0.030", size=4, color="darkviolet")+
  annotate("text", x=40, y=13, label="a=0.150", size=4, color="darkviolet")


#### 다. 백만명당 사망자 수 ####
ggscatter(data[data$Date=="2020-03-20",], x="TestsperM", y="DeathsperM", 
          xlab="Tests performed per million", ylab="Death cases per million",
          add="reg.line", conf.int=T, 
          palette=c("blue", "gray"), color="Group", legend="none",
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Italy", "Spain", "Iran", "Switzerland",
                         "Netherlands", "A.Emirates", "Australia",
                         "Bahrain", "Korea", "Iceland", "Norway",
                         "France", "Taiwan", "Russia", "Canada", "UK",
                         "Japan", "US", "China", "Austria", "Mexico",
                         "India", "Indonesia", "Philippines"))+
  yscale ("log10", .format=T) + xscale ("log10", .format=T)+
  geom_point(data=rdata$dataPS, colour="red", size=2, label="Country")+
  geom_text(data=rdata$dataPS, aes(label=Country), hjust=1, vjust=1, col="red")+
  geom_abline(data.frame(s=c(0.001, 0.005, 0.03, 0.0001)), size=1,
       col=c("darkviolet", "darkviolet", "darkviolet", "darkviolet"), alpha=0.5,
       show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=50, y=0.02, label="a=0.001", size=4, color="darkviolet")+
  annotate("text", x=25, y=0.30, label="a=0.005", size=4, color="darkviolet")+
  annotate("text", x=30, y=2.5, label="a=0.030", size=4, color="darkviolet")+
  annotate("text", x=200, y=0.01, label="a=0.0001", size=4, color="darkviolet")


# 백만명당 검사자에 따른 사망률 수
ggscatter(data, x="TestsperM", y="DeathRate", 
          xlab="Tests performed per million", ylab="Death Rate",
          add="reg.line", conf.int=T,
          color="Group", palette=c("blue", "gray"), legend="none",
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=1.5, label.y=1, label.sep = ",", col="Red"),
          label="Country", repel=T,
          label.select=c("Bahrain", "Korea", "Iceland", "Norway",
                         "Italy", "India", "France", "Ukraine",
                         "Taiwan", "Indonesia", "Iran", "UK",
                         "Japan", "US", "China", "Philippines",
                         "Spain", "Netherlands", "Hungary", "India",
                         "Brazil", "Turkey", "Mexico", "Pakistan",
                         "Germany", "Slovernia", "Bahrain", "Denmark",
                         "Panama", "Slovenia", 
                         "Belguim", "Canada", "A.Emirates"))+
  yscale ("log10", .format=T) + xscale ("log10", .format=T)+
  geom_point(data=rdata$dataPS, colour="red", size=2, label="Country")+
  geom_text(data=rdata$dataPS, aes(label=Country), hjust=1, vjust=1, col="red")







