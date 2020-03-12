#### 1. 데이터 가져오기 ####
library(dplyr); library(reshape2); library(ggplot2); library(gganimate)
library(tidyr); library(scales)
rdata <- read.csv("data/Gender.csv") %>% mutate(Date=as.Date(Date,"%Y-%m-%d"))


#### 2. 시간에 따라 남녀 확진 비율은 어떻게 달라질까? ####
print(data <- rdata %>% select(c("Date", "Male", "Female")) %>% 
        melt(id=c("Date")) %>% arrange(Date))

print(result1 <- ggplot(data=data, aes(x=factor(1), y=value, fill=variable), 
       labels=paste0(data$variable, " (", data$value, "%)")) +
  geom_bar(width=1, stat="identity") + coord_polar(theta="y") +
  ylab("") + xlab("") + 
  labs(title='COVID-19 in South Korea \n {closest_state}') +
  theme(plot.title = element_text(hjust = 0, size = 22),
        legend.position = "none", axis.ticks = element_blank(), 
        panel.grid = element_blank(), axis.text  = element_blank())+
  geom_text(aes(x=1, label=paste0(variable, " : ", value, "%")), 
            position=position_stack(vjust=0.5), size=7)+
  transition_states(states = Date, 4, 1))

animate(result1, fps=10, duration=20, start_pause=10, 
        renderer=gifski_renderer("GenderEvolution.gif"))


#### 2. 시간에 따라 성별에 의한 사망률 차이는 어떻게 변했을까? ####
# 3월 2일부터 데이터가 있음.
rdata[rdata$Date=="2020-03-07",2:5]
prop.test(c(26, 18), c(2522, 4245))

date <- "2020-03-02"
date <- "2020-03-03"
prop.test(c(rdata[rdata$Date==date,2], rdata[rdata$Date==date,3]), 
          c(rdata[rdata$Date==date,4], rdata[rdata$Date==date,5])) 
date <- "2020-03-04"
date <- "2020-03-05"
date <- "2020-03-06"
date <- "2020-03-07"



#### 3. 시간과 성별에 의한 확진율과 사망률의 변화 ####
print(data <- rdata %>% select(c("Date","Male_Confirmed","Female_Confirmed",
  "Male_DeathRate","Female_DeathRate")) %>% 
  gather(key="label", value="num", -Date) %>%
  separate(label, c("Gender", "type"), sep = "_") %>% spread(type, num) %>% 
  tail(12))

print(result2 <- ggplot(data, aes(x=Confirmed, y=DeathRate, colour=Gender)) + 
  scale_x_continuous(labels=comma) + xlab("Confirmed") + ylab("Death Rate (%)") +
  labs(title = 'COVID-19 in South Korea \n {frame_along}')+ 
  theme_classic() + geom_line(size=1.2) + geom_point(size=5) + 
  geom_segment(aes(xend=max(Confirmed), yend=DeathRate), linetype=2) +
  geom_text(aes(x=max(Confirmed), label=round(DeathRate, 3)), hjust=0, size=7)+
  theme(legend.position=c(0.5, 0.5), text=element_text(size=20),
        plot.margin=margin(10, 32, 10, 10))+
  transition_reveal(Date) + view_follow(fixed_y=T) + 
  coord_cartesian(clip='off', expand=T))

animate(result2, fps=10, duration=10, start_pause=10, 
        renderer=gifski_renderer("DeathRateEvolution.gif"))
