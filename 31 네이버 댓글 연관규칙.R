#### 1. 댓글 가져오기 ####
library(DNH4); library(N2H4); library(dplyr); library(stringr)
rdata <- list()

NaverCom <-function(url, pages){
  bigpages <- str_sub(pages, 1,-3)
  smallpages <- str_sub(pages, -2,-1)
  for(i in 1:bigpages){
    if(i==1){
      comments <- data.frame(getComment(url, type="list",
        pageSize=100, page=i)$result$commentList)[,c("contents", "sympathyCount", 
        "antipathyCount", "replyAllCount", "country", "userName")]
    }else{
      comments <- rbind(comments, data.frame(getComment(url, type="list", 
       pageSize=100, page=i)$result$commentList)[,c("contents", "sympathyCount", 
       "antipathyCount", "replyAllCount", "country", "userName")])
    }
  }
  comments <- rbind(comments, data.frame(getComment(url, type="list", 
       pageSize=smallpages, page=i)$result$commentList)[,c("contents", 
       "sympathyCount", "antipathyCount", "replyAllCount", "country", 
       "userName")]) %>% arrange(desc(sympathyCount))
  return(comments)
}
rdata$naver <- NaverCom("https://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=102&oid=023&aid=0003520741", 3043) # 2020-04-05 18시 즈음

head(rdata$naver$contents, 5)

rdata$daum <- DNH4::getComment("https://news.v.daum.net/v/20200404010035480", 
           limit=100) %>% select(c("content", "likeCount", "dislikeCount", 
                          "user_displayName"))


#### 2. 데이터 분석 ####
library(tidyverse); library(reshape2); library(KoNLP); library(tidytext); 
library(wordcloud); library(ggpubr); library(widyr)
useSejongDic()


#### 가. 데이터 전처리 ####
rdata$naverC <- rdata$naver$contents %>% 
  str_replace_all(pattern = '[ㄱ-ㅎ]', '')              # ㅋㅋ, ㅎㅎ
rdata$naverC <- rdata$naverC %>% 
  str_replace_all(pattern='[ㅏ-ㅣ]', ' ')               # ㅠㅠ
rdata$naverC <- rdata$naverC %>% 
  str_replace_all(pattern='[~!♪♬@#$%^&*()_+=?.,]', ' ') # 특수문자
rdata$naverC <- rdata$naverC %>% 
  str_replace_all(pattern='사진|이모티콘', ' ')         # 이모티콘
rdata$naverC <- rdata$naverC %>% 
  str_replace_all(pattern='(http.+)', ' ')              # 홈페이지
rdata$naverC <- rdata$naverC %>% 
  str_replace_all(pattern='[A-Za-z]', ' ')              # 영어
rdata$naverC <- rdata$naverC %>% 
  str_replace_all(pattern='\\\n', ' ')                  # 줄바꿈
rdata$naverC <- rdata$naverC %>% 
  str_replace_all(pattern = '\\s+', ' ')                # 여러 칸을 한칸으로


# http://blog.daum.net/sys4ppl/6
#### 나. 형태소 분석 ####
print(rdata$dataN <-  rdata$naverC %>%  SimplePos09 %>% melt %>% as_tibble %>% 
  select(3, 1) %>% mutate(word=str_match(value, '([가-힣]+)/[NP]')[,2]) %>% 
  select(c(1,3)) %>% dplyr::rename(line=1, word=2) %>% 
  na.omit %>% filter(str_length(word)>=2))

#### 다. 연관규칙 ####
library(arules); library(arulesViz)

#### _1) 상위 100위권 댓글####
data <- rdata$dataN %>% filter(line<=100)

image(rdata$trans <- as(sapply(split(data$word, data$line), unique), 
                  "transactions"))

# 연관규칙 만들기
inspect(rdata$rules <- subset(apriori(rdata$trans, 
  parameter=list(supp=0.06, conf=0.05)), subset=(lift>1.01 | lift<0.99)) %>% 
    sort(by="support"))

# 시각화
plot(sort(rdata$rules, by="support"), method="graph")

# 연관 단어 확률 계산
rdata$temp <- rdata$naverC[1:100]
length(rdata$temp[grepl("중국", rdata$temp) & 
                    grepl("코로나", rdata$temp)])/length(rdata$temp)
length(rdata$temp[grepl("자화자찬", rdata$temp) & 
                    grepl("방역", rdata$temp)])/length(rdata$temp)
length(rdata$temp[grepl("정부", rdata$temp) & !grepl("아니", rdata$temp) &
                    grepl("잘하", rdata$temp)])/length(rdata$temp)

#### _2) 101위권 이하 댓글 ####
data <- rdata$dataN %>% filter(line>100)

image(rdata$trans <- as(sapply(split(data$word, data$line), unique), 
                        "transactions"))

# 연관규칙 만들기
inspect(rdata$rules <- subset(apriori(rdata$trans, 
                                      parameter=list(supp=0.06, conf=0.05)), subset=(lift>1.01 | lift<0.99)) %>% 
          sort(by="support"))

# 시각화
plot(sort(rdata$rules, by="support"), method="graph")

# 연관 단어 확률 계산
rdata$temp <- rdata$naverC[101:length(rdata$naverC)]
length(rdata$temp[grepl("중국", rdata$temp) & 
                    grepl("코로나", rdata$temp)])/length(rdata$temp)
length(rdata$temp[grepl("자화자찬", rdata$temp) & 
                    grepl("방역", rdata$temp)])/length(rdata$temp)
length(rdata$temp[grepl("정부", rdata$temp) & !grepl("아니", rdata$temp) &
                    grepl("잘하", rdata$temp)])/length(rdata$temp)


#### _3) 다른 종류의 그래프들 ####
plot(sort(rdata$rules, by="support"), method="graph", engine="visNetwork")

plot(rdata$rules, method="paracoord", control=list(reorder=T))
plot(rdata$rules, measure=c("support", "lift"),
     shading="confidence", interactive=T)

#### _4) 특정 단어 관련, 혹은 배재한 연관규칙 ####
inspect(rdata$rules_interest <- subset(rdata$rules, (items %in% !c("자화자찬"))))
inspect(rdata$rules_interest <- subset(rdata$rules, (items %in% c("짜파구리"))))
plot(sort(rdata$rules_interest, by="support"), method="graph", engine='visNetwork')


