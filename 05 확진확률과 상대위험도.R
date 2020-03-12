#### 1. 데이터 불러오기 ####
library(corona19); library(dplyr); library(Epi); library(ggpubr)
rdata <- getdata("state") %>% arrange(date)

#### 2. 확진확률 및 상대위험도 계산 ####
data <- rdata[c(1,5,4)] %>% mutate(확진확률=100*confirmed/(confirmed+negative)) %>% 
  mutate(상대위험도=1) %>% mutate(pvalue=1) 
for(i in 2:nrow(data)){
  data[i,5] <- 1/twoby2(as.matrix(data[(i-1):i, 2:3]))$measures[1]
  data[i,6] <- twoby2(as.matrix(data[(i-1):i, 2:3]))$p.value[1]
}
data <- data %>% mutate(stars=ifelse(pvalue < 0.001, "***", 
        ifelse(pvalue < 0.01, "**", ifelse(pvalue < 0.05, "*", "")))) %>% 
  mutate_if(is.numeric, round, 2)
data$label1 <- paste0(data$확진확률, data$stars)
data$label2 <- paste0(data$상대위험도, data$stars)

ggline(data, x="date", y="확진확률", ylab="확진확률(%)", color="#FC4E07", 
       ylim=c(0, 6), label=data$label1, size=1.2, font.label=c(13,"plain"), 
       label.rectangle=T, repel=T)

# Zoom 버튼을 눌러서 이미지를 보세요.

ggline(data, x="date", y="상대위험도", ylab="상대위험도(배)", color="#FC4E07", 
       ylim=c(0, 2), label=data$label2, size=1.2, font.label=c(13,"plain"), 
       label.rectangle=T, repel=T)+ geom_hline(yintercept=1, linetype = 2)

