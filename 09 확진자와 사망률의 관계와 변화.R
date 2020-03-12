#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble)
rdata <- list()

# Johns Hopkins University Center for Systems Science and Engineering
# https://github.com/CSSEGISandData/COVID-19
rdata$url <- c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", 
               "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# 미국 데이터는 50개주 데이터만, 후베이와 후베이 이외의 중국으로 구분
rdata$confirmedCases <- read_csv(rdata$url[1]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1, "State"=2, "Variable"=3, "Confirmed"=4) %>% 
  mutate(Country=ifelse(Country=="US" & !grepl(",", State), NA, Country)) %>% 
  mutate(Country=ifelse(!is.na(State) & State=="Hubei", "Hubei", Country)) %>% 
  group_by(Country,Variable) %>% summarise(Confirmed=sum(Confirmed)) %>% 
  rename("Country"=1,"Date"=2,"Confirmed"=3)

nrow(rdata$confirmedCases %>% filter(Country=="US")) # 50개 주가 나오면 정상

rdata$DeathCases <- read_csv(rdata$url[2]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Deaths"=4) %>% 
  mutate(Country=ifelse(Country=="US" & !grepl(",", State), NA, Country)) %>% 
  mutate(Country=ifelse(!is.na(State) & State=="Hubei", "Hubei", Country)) %>% 
  group_by(Country,Variable) %>% summarise(Deaths=sum(Deaths)) %>% 
  rename("Country"=1,"Date"=2,"Deaths"=3)

rdata$recoveredCases <- read_csv(rdata$url[3]) %>% select(-c(Lat,Long)) %>% 
  melt(id=c('Country/Region','Province/State')) %>% 
  rename("Country"=1,State=2, "Variable"=3, "Recovered"=4) %>% 
  mutate(Country=ifelse(Country=="US" & !grepl(",", State), NA, Country)) %>% 
  mutate(Country=ifelse(!is.na(State) & State=="Hubei", "Hubei", Country)) %>% 
  group_by(Country, Variable) %>% summarise(Recovered=sum(Recovered)) %>% 
  rename("Country"=1,"Date"=2,"Recovered"=3)

rdata$COVID19 <- merge(merge(rdata$confirmedCases, rdata$DeathCases, 
                             by.y=c("Country","Date")), rdata$recoveredCases, by.y=c("Country","Date")) %>% 
  mutate(Date=as.Date(.$Date, "%m/%d/%y")) %>% 
  mutate(DeathRate=Deaths/Confirmed)

rdata$COVID19$Country <- gsub("Korea\\, South", "Korea", rdata$COVID19$Country)
rdata$COVID19$Country <- gsub("Iran \\(Islamic Republic of\\)", "Iran",
                              rdata$COVID19$Country)
rdata$COVID19$Country <- gsub("Hong Kong SAR", "Hong Kong", rdata$COVID19$Country)

max(rdata$COVID19$Date)



#### 2. 상관관계 ####
#### 가. 우리나라(1일 확진자에 따른 사망률) ####
data <- rdata$COVID19 %>% filter(Confirmed>100) %>% 
  mutate(DeathRate=100*Deaths/Confirmed) %>% filter(Country=="Korea") %>% 
  arrange(Date) %>% mutate(Date=as.character(Date)) %>% 
  mutate(Date=str_sub(Date, 6, 10)) %>% mutate(DayConfirmed=0)

for(i in 2:nrow(data)){
  data[i,7] <- data[i, 3]-data[i-1, 3]
}
library(ggpubr)
ggscatter(data, x="DayConfirmed", y="DeathRate", xlab="1일 확진자 수", ylab="사망률",
          label="Date", repel=T, title="우리나라 1일 확진자 수 대비 사망률",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=450, label.y=0.9, label.sep = ","))


#### 나. 미국(1일 확진자에 따른 사망률) ####
data <- rdata$COVID19 %>% filter(Confirmed>100) %>% 
  mutate(DeathRate=100*Deaths/Confirmed) %>% filter(Country=="US") %>% 
  arrange(Date) %>% mutate(Date=as.character(Date)) %>% 
  mutate(Date=str_sub(Date, 6, 10)) %>% mutate(DayConfirmed=20)

for(i in 2:nrow(data)){
  data[i,7] <- data[i, 3]-data[i-1, 3]
}

ggscatter(data, x="DayConfirmed", y="DeathRate", xlab="1일 확진자 수", ylab="사망률",
          label="Date", repel=T,  title="미국 1일 확진자 수 대비 사망률",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=200, label.y=8.5, label.sep = ","))

data <- rdata$COVID19 %>% filter(Confirmed>100 & Date<as.Date("2020-03-10")) %>% 
  mutate(DeathRate=100*Deaths/Confirmed) %>% filter(Country=="US") %>% 
  arrange(Date) %>% mutate(Date=as.character(Date)) %>% 
  mutate(Date=str_sub(Date, 6, 10)) %>% mutate(DayConfirmed=20)

for(i in 2:nrow(data)){
  data[i,7] <- data[i, 3]-data[i-1, 3]
}
ggscatter(data, x="DayConfirmed", y="DeathRate", xlab="1일 확진자 수", ylab="사망률",
          label="Date", repel=T,  title="미국 1일 확진자 수 대비 사망률",
          add="reg.line", conf.int=T,
          add.params=list(color="blue", fill="lightgray"),
          cor.coef=T, cor.coeff.args=list(method="pearson", 
              label.x=70, label.y=9, label.sep = ","))



#### 3. 시간에 따른 확진자, 사망자, 사망률 시각화 ####
data <- rdata$COVID19 %>% filter(Confirmed>100) %>% 
  mutate(DeathRate=100*Deaths/Confirmed)

data <- rdata$COVID19 %>% filter(Confirmed>100) %>% 
  mutate(DeathRate=100*Deaths/Confirmed) %>% filter(Country!="Hubei")

data <- rdata$COVID19 %>% filter(Confirmed>100) %>% 
  mutate(DeathRate=100*Deaths/Confirmed) %>% 
  filter(Country!="Hubei" & Country!="China" &
           Country!="Korea" & Country!="Iran" & Country!="Italy")

#### 가. 확진자 대비 사망률 정상스케일 ####
library(gganimate); library(scales)

print(result <- ggplot(data, aes(x=Confirmed, y=DeathRate, 
  color=Country, size=Deaths))+scale_size(range=c(5, 10))+ylab("Death Rate (%)")+
  scale_x_continuous(labels=comma)+geom_point(show.legend=F, alpha=0.5)+
  geom_text(aes(x=Confirmed, y=DeathRate, label=Country), vjust=-1, show.legend=F)+
  geom_hline(yintercept=2.5, color="violet") + annotate("text", 
      x=max(data$Confirmed)-max(data$Confirmed)/6, y=2.9, 
      label="Spanish flu(2.5%)", size=6, color="violet")+
  geom_hline(yintercept=9.6, color="red") + annotate("text", 
      x=max(data$Confirmed)-max(data$Confirmed)/6, y=10.2, 
      label="SARS virus(9.6%)", size=6, color="red")+
  geom_hline(yintercept=0.1, color="blue") + annotate("text", 
      x=max(data$Confirmed)-max(data$Confirmed)/6, y=-0.4, 
      label="Seasonal flu(0.1%)", size=6, color="blue")+
  theme(text=element_text(size=20), plot.margin=margin(10, 30, 10, 10))+
  transition_time(Date) + labs(title = "{frame_time}"))

animate(result, 300, fps=10, duration=30, end_pause=50, width=500, height=400, 
        renderer=gifski_renderer("DeathRateEvolution1.gif"))

##### 나. 확진자 대비 사망률 로그스케일 ####
print(result <- ggplot(data, aes(x=Confirmed, y=DeathRate, 
  color=Country, size=Deaths))+scale_size(range=c(5, 10)) + ylab("Death Rate (%)") +
  scale_x_continuous(labels=comma, trans="log10") +
  geom_point(show.legend=F, alpha=0.5)+ geom_text(aes(x=Confirmed, 
      y=DeathRate, label=Country), vjust=-1, show.legend=F)+
  geom_hline(yintercept=2.5, color="violet") + annotate("text", 
      x=max(data$Confirmed)-max(data$Confirmed)*0.6, y=2.9, 
      label="Spanish flu(2.5%)", size=6, color="violet")+
  geom_hline(yintercept=9.6, color="red") + annotate("text", 
      x=max(data$Confirmed)-max(data$Confirmed)*0.6, y=10.2, 
      label="SARS virus(9.6%)", size=6, color="red")+
  geom_hline(yintercept=0.1, color="blue") + annotate("text", 
      x=max(data$Confirmed)-max(data$Confirmed)*0.6, y=-0.4, 
      label="Seasonal flu(0.1%)", size=6, color="blue")+
  theme(text=element_text(size=20), plot.margin=margin(10, 30, 10, 10))+
  transition_time(Date) + labs(title = "{frame_time}"))

animate(result, 300, fps=10, duration=30, end_pause=50, width=500, height=400, 
        renderer=gifski_renderer("DeathRateEvolution2.gif"))

#### 다. 확진자 대비 사망자 정상스케일 ####
print(result <- ggplot(data, aes(x=Confirmed, y=Deaths, 
                           color=Country, size=DeathRate))+
  scale_x_continuous(labels=comma, limits=c(0, 15000)) + 
  scale_y_continuous(labels=comma, limits=c(-1, 300)) + 
  geom_point(show.legend=F, alpha=0.5)+scale_size(range=c(5, 10))+ 
  geom_text(aes(x=Confirmed, y=Deaths, label=Country), vjust=-1, show.legend=F)+
  geom_abline(data.frame(a=c(0.001, 0.025, 0.096)), col=c("blue", "violet", "red"), 
              show.legend=F, mapping=aes(slope=a, intercept=0)) +
  annotate("text", x=6000, y=280, label="SARS virus(9.6%)", size=6, color="red")+
  annotate("text", x=10000,y=170,label="Spanish flu(2.5%)", size=6, color="violet")+
  annotate("text", x=11000,y=-0.4,label="Seasonal flu(0.1%)", size=6, color="blue")+
  theme(text=element_text(size=20), plot.margin=margin(10, 30, 10, 10))+
  transition_time(Date) + labs(title = "{frame_time}"))

animate(result, 300, fps=10, duration=30, end_pause=50, width=500, height=400, 
        renderer=gifski_renderer("DeathEvolution1.gif"))

#### 라. 확진자 대비 사망자 로그스케일 ####
print(result <- ggplot(data, aes(x=Confirmed, y=Deaths, 
                           color=Country, size=DeathRate))+
  scale_x_continuous(labels=comma, trans="log10", limits=c(10, 100000)) +
  scale_y_continuous(labels=comma, trans="log10", limits=c(10, 300)) +
  geom_text(aes(x=Confirmed, y=Deaths, label=Country), vjust=-1, show.legend=F)+
  geom_abline(data.frame(s=c(1, 0.001, 0.025, 0.096)), 
              col=c("black", "blue", "violet", "red"), 
              show.legend=F, mapping=aes(slope=1, intercept=log10(s))) +
  annotate("text", x=80, y=250, label="Death 100%", size=6, color="Black")+
  annotate("text", x=1000, y=200, label="SARS virus(9.6%)", size=6, color="red")+
  annotate("text", x=5000, y=100, label="Spanish flu(2.5%)", 
           size=6, color="violet")+
  annotate("text", x=10000, y=15, label="Seasonal flu(0.1%)", size=6, color="blue")+
  geom_point(show.legend=F, alpha=0.5)+scale_size(range=c(5, 10))+ 
  theme(text=element_text(size=20), plot.margin=margin(10, 30, 10, 10))+
  transition_time(Date) + labs(title = "{frame_time}"))

animate(result, 300, fps=10, duration=30, end_pause=50, width=500, height=400, 
        renderer=gifski_renderer("DeathEvolution2.gif"))
