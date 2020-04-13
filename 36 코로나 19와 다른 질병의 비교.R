#### 1. 데이터 전처리 ####
library(tidyverse); library(reshape2); library(tibble); library(stringr);
rdata <- list()

#### 질병에 의한 사망자 정보 ####
rdata$Diseases <- readxl::read_excel("data/DiseaseComparison.xlsx") %>% 
  melt(id.var=c("Name")) %>% 
  mutate(variable=as.Date(as.numeric(variable), origin="2019-12-31")) %>% 
  arrange(Name)


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
