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

max(rdata$World$Date)


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
Sys.setlocale("LC_ALL", "english")
rdata$TotalTests <- read_csv("data/full-list-total-tests-for-covid-19.csv") %>% 
  select(c(1,3:4)) %>% rename(Country=1, Date=2, TotalTests=3) %>% 
  mutate(Date=lubridate::mdy(Date))
Sys.setlocale("LC_ALL", "korean")

# 나라 이름 맞추기
setdiff(rdata$TotalTests$Country, rdata$WorldP$Country)
rdata$TotalTests$Country <- gsub("South Korea", "Korea", rdata$TotalTests$Country)
rdata$TotalTests$Country <- gsub("United States", "US", rdata$TotalTests$Country)

rdata$TotalTests <- rdata$TotalTests %>% 
  filter(Country!="India, people tested" & Country!="US, specimens tested (CDC)")
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
# 아직 데이터가 불완전해 보인다. 
# 일본의 4월 2일자 테스트는 전날에 비해 2건 증가했으나 확진자는 317명 증가ㅠㅠ


#### 2. 군집분석 ####
#### 가. 변수간 군집분석 ####
library(pheatmap); library(NbClust); library(factoextra)
data <- rdata$data %>% select(c("Confirmed", "Deaths", "Recovered", "DailyC", 
        "DailyD", "DeathRate", "ConfirmedperM", "DeathsperM")) %>% na.omit

pheatmap(cor(data, method="pearson"), cutree_rows=4, cutree_cols=4,
         display_numbers=T, fontsize=8, number_format="%.2f", number_color="grey0")

#### 나. 국가간 군집분석 ####
# 1000명 이상 확진자가 나온 국가의 수 확인
rdata$data %>% group_by(Country) %>% filter(Date<Sys.Date()) %>% 
  filter(Confirmed>=1000) %>% distinct(Country) %>% pull() %>% length()

# 국가별 검사자 데이터가 있는 날짜 수 확인
rdata$data %>% group_by(Country) %>% filter(!is.na(TotalTests)) %>% 
  summarise(Num=length(TotalTests))
# 국가별로 최근 데이터로 10개만 뽑아볼까?
head(data <- rdata$data %>% group_by(Country) %>% filter(!is.na(TotalTests)) %>% 
  filter(length(Date)>=10) %>% arrange(desc(Date)) %>% slice(1:10))
# 어떻게 사용해야할지... 나중에 써먹어 보자.

rdata$cutoff <-100
rdata$cutoff <-500
rdata$cutoff <-1000
data <- rdata$data %>% group_by(Country) %>% filter(!is.na(Confirmed)) %>% 
  filter(max(Confirmed)>=rdata$cutoff*10) %>% 
  filter(Confirmed>=rdata$cutoff & Confirmed<=rdata$cutoff*10) %>% 
  mutate(Days=max(Date)-min(Date)+1) %>% arrange(desc(Days)) %>% 
  mutate(Days=as.numeric(as.character(Days))) %>% 
  arrange(desc(Confirmed)) %>% slice(1) %>% 
  select(c("Country", "Days", "Confirmed", "Deaths", "TotalTests")) %>% 
  ungroup() %>% column_to_rownames("Country") %>% 
  select(c("Days", "Confirmed", "Deaths", "TotalTests")) %>% na.omit

NbClust(scale(data), distance="euclidean", min.nc=2, max.nc=5, method="average")
NbClust(scale(data), distance="euclidean", min.nc=2, max.nc=6, method="average")
# 100(2), 500(2), 1000(5)

rdata$fit.hc <- data %>% scale() %>% dist(method="euclidean") %>% 
  hclust(method="ward.D2")

fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=2,  #cutoff=100, 500
          color_labels_by_k=T, rect=T)
fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=4,  #cutoff=100, 500
          color_labels_by_k=T, rect=T)
fviz_dend(rdata$fit.hc, cex=1.2, labels_track_height=5, k=5,  #cutoff=100, 500
          color_labels_by_k=T, rect=T)




#### 3. 그래프 비교 ####
library(gganimate); library(ggpubr); library(scales)
unique(rdata$data$Country)

#### 가. 확진자 수 대비 1일 증가량 ####
# 7일전
data <- rdata$data %>% group_by(Country) %>% filter(Date==Sys.Date()-7) %>% 
  filter(!is.na(DailyC)) %>% 
  mutate(Group=ifelse(Country=="Germany", "Germany", 
               ifelse(Country=="US", "US",
               ifelse(Country=="Japan", "Japan", 
               ifelse(Country=="Italy", "Italy", 
               ifelse(Country=="Sweden", "Sweden", "Others"))))))
data$Group <- factor(data$Group, levels=c("Germany", "US", "Italy", 
                                          "Japan", "Sweden", "Others"))
data %>% filter(Confirmed>1000 & Confirmed <2000 & DailyC<50)

ggscatter(data, x="Confirmed", y="DailyC", color="Group", title=Sys.Date()-7,
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "grey"),
          label="Country", repel=T, label.select=c("Germany", "Japan", 
            "Sweden", "Korea", "China", "US", "Italy", "Spain", "Turkey",
            "Iran", "UK", "France", "Switzerland", "Austria", 
            "South Africa"))+ #"Luxembourg", 
  scale_x_continuous(labels=comma, trans="log10", limits=c(500,500000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(20,80000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.005, 0.04, 0.3)), size=1,
              col=c("Black", "Black", "Black"), alpha=0.5,
              show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=200000, y=400, label="a=0.005", size=5, color="grey")+
  annotate("text", x=420000, y=8000, label="a=0.050", size=5, color="grey")+
  annotate("text", x=35000, y=40000, label="a=0.300", size=5, color="grey")

# 1일전
data <- rdata$data %>% group_by(Country) %>% filter(Date==Sys.Date()-1) %>% 
  filter(!is.na(DailyC)) %>% 
  mutate(Group=ifelse(Country=="Germany", "Germany", 
               ifelse(Country=="US", "US",
               ifelse(Country=="Japan", "Japan", 
               ifelse(Country=="Italy", "Italy", 
               ifelse(Country=="Sweden", "Sweden", "Others"))))))
data$Group <- factor(data$Group, levels=c("Germany", "US", "Italy", 
                                          "Japan", "Sweden", "Others"))

ggscatter(data, x="Confirmed", y="DailyC", color="Group", title=Sys.Date()-1,
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "grey"),
          label="Country", repel=T, label.select=c("Germany", "Japan", 
            "Sweden", "Korea", "China", "US", "Italy", "Spain", "Turkey",
            "Iran", "UK", "France", "Switzerland", "Austria", "Australia",
            "Greece", "South Africa"))+ #"Luxembourg", 
  scale_x_continuous(labels=comma, trans="log10", limits=c(500,500000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(20,80000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.005, 0.04, 0.3)), size=1,
              col=c("Black", "Black", "Black"), alpha=0.5,
              show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=200000, y=300, label="a=0.005", size=5, color="grey")+
  annotate("text", x=300000, y=13000, label="a=0.050", size=5, color="grey")+
  annotate("text", x=50000, y=50000, label="a=0.300", size=5, color="grey")




#### 나. 1일 검사자 수 대비 확진자 수 ####
# 7일 전
data <- rdata$data %>% group_by(Country) %>% filter(Date==Sys.Date()-7) %>% 
  filter(!is.na(TotalTests)) %>% 
  mutate(Group=ifelse(Country=="Korea", "Korea", 
               ifelse(Country=="UK", "UK",
               ifelse(Country=="Japan", "Japan", 
               ifelse(Country=="Italy", "Italy", 
               ifelse(Country=="Taiwan", "Taiwan", "Others"))))))
data$Group <- factor(data$Group, levels=c("Korea", "UK", "Italy", 
                                          "Japan", "Taiwan", "Others"))

ggscatter(data, x="DailyT", y="DailyC", color="Group", title=Sys.Date()-7,
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "grey"),
          label="Country", repel=T, label.select=c("Korea", "Taiwan",
            "South Africa", "Japan", "UK", "Italy", "Ecuador"))+
  scale_x_continuous(labels=comma, trans="log10", limits=c(500,50000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(10,10000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.008, 0.05, 0.3)), size=1,
              col=c("Black", "Black", "Black"), alpha=0.5,
              show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=15000, y=50, label="a=0.008", size=5, color="grey")+
  annotate("text", x=20000, y=500, label="a=0.050", size=5, color="grey")+
  annotate("text", x=3000, y=1600, label="a=0.300", size=5, color="grey")

# 1일 전
data <- rdata$data %>% group_by(Country) %>% filter(Date==Sys.Date()-1) %>% 
  filter(!is.na(TotalTests)) %>% 
  mutate(Group=ifelse(Country=="Korea", "Korea", 
               ifelse(Country=="UK", "UK",
               ifelse(Country=="Japan", "Japan", 
               ifelse(Country=="Italy", "Italy", 
               ifelse(Country=="Taiwan", "Taiwan", "Others"))))))
data$Group <- factor(data$Group, levels=c("Korea", "UK", "Italy", 
                                          "Japan", "Taiwan", "Others"))

ggscatter(data, x="DailyT", y="DailyC", color="Group", title=Sys.Date()-1,
          legend="none", size="DeathRate", font.label=list(size=16), alpha=0.5,
          palette=c("blue", "darkgreen", "darkviolet", "red", "orange", "grey"),
          label="Country", repel=T)+
  scale_x_continuous(labels=comma, trans="log10", limits=c(500,50000)) + 
  scale_y_continuous(labels=comma, trans="log10", limits=c(10,10000)) +
  theme(text=element_text(size=16), plot.margin=margin(0, 30, 10, 10)) +
  geom_abline(data.frame(s=c(0.008, 0.05, 0.3)), size=1,
    col=c("Black", "Black", "Black"), alpha=0.5,
    show.legend=F, mapping=aes(slope=1, intercept=log10(s)))+
  annotate("text", x=20000, y=80, label="a=0.008", size=5, color="grey")+
  annotate("text", x=20000, y=500, label="a=0.050", size=5, color="grey")+
  annotate("text", x=3000, y=1600, label="a=0.300", size=5, color="grey")


#### 4. 개별국가 비교 ####
library(gganimate); library(ggpubr); library(scales)

#### 가. 일본과 비슷한 확진자 수를 가진 국가들 ####
rdata$data %>% filter(Date==Sys.Date()-1 & Confirmed>3000 & Confirmed<5000)
# 인도가 가장 심각, 파키스탄, 일본, 필리핀, 칠레, 덴마크 순.

data <- rdata$data %>% filter(Country=="Philippines" | Country=="Pakistan" |
      Country=="India" | Country=="Japan")
data$Country <- factor(data$Country, levels=c("Japan", "India", "Pakistan",
                                              "Philippines"))

ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Deaths", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)


#### 나. 일본과 이탈리아, 스페인, 미국 비교 ####
print(rdata$Japan <-rdata$data %>% filter(Country=="Japan" & Confirmed>2000) %>% 
        arrange(Country, Date))

print(rdata$Italy <-rdata$data %>% filter(Country=="Italy" & Confirmed>2000) %>% 
        arrange(Country, Date))

print(rdata$US <-rdata$data %>% filter(Country=="US" & Confirmed>2000) %>% 
        arrange(Country, Date))

print(rdata$China <-rdata$data %>% filter(Country=="China" & Confirmed>2000) %>% 
        arrange(Country, Date))

rdata$Japan$Date <- c(1:nrow(rdata$Japan))
rdata$Italy$Date <- c(1:nrow(rdata$Italy))
rdata$US$Date <- c(1:nrow(rdata$US))
rdata$China$Date <- c(1:nrow(rdata$China))

# 전날 데이터가 없으면 nrow(Iraq)-1로, 있으면 nrow(Iraq)로 수정해 주세요.
data <- rbind(rdata$Japan[1:nrow(rdata$Japan),], rdata$Italy[1:nrow(rdata$Japan),], 
              rdata$US[1:nrow(rdata$Japan),], rdata$China[1:nrow(rdata$Japan),])

data$Country <- factor(data$Country, levels=c("China", "US", "Italy", "Japan"))

ggline(data, x="Date", y="Confirmed", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

ggline(data, x="Date", y="Deaths", color="Country", size=1.2, legend="top")+
  theme(legend.title=element_text(size=16), legend.text=element_text(size=14), 
        text=element_text(size=16), legend.position=c(0.3, 0.7)) +
  scale_y_continuous(labels=comma)

print(result <- ggplot(data, aes(x=Date, y=Confirmed, color=Country)) + 
        scale_y_continuous(labels=comma) + theme_classic() + 
        geom_line(size=1.2) + geom_point(size=5) + 
        geom_segment(aes(xend=max(Date)+1, yend=Confirmed), linetype=2) +
        geom_text(aes(x=max(Date)+2, 
                      label=paste0(comma(Confirmed, accuracy=1))), size=7) +
        theme(legend.position=c(0.3, 0.8), text=element_text(size=25),
              plot.margin=margin(10, 30, 10, 10)) +
        transition_reveal(Date) + view_follow(fixed_y=T) + 
        coord_cartesian(clip='off'))

animate(result, 300, fps=10, duration=30, end_pause=100, width=500, height=400, 
        renderer=gifski_renderer("ChinaItalyKorea.gif"))
