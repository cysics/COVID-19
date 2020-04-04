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
naver <- NaverCom("https://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=102&oid=023&aid=0003520741", 2720) # 2020-04-04 11:23 즈음

daum <- DNH4::getComment("https://news.v.daum.net/v/20200404010035480", 
           limit=100) %>% select(c("content", "likeCount", "dislikeCount", 
                          "user_displayName"))

rdata$daum <- daum
rdata$naver <- naver
naver <- rdata$naver[1:100,]
daum <- rdata$daum

#### 2. 다음 데이터 분석 ####
library(tidyverse); library(reshape2); library(KoNLP); library(tidytext); 
library(wordcloud); library(ggpubr); library(widyr); library(kospacing)
useSejongDic()


#### 가. 단어 빈도수 비교 ####
#### _1) 네이버 댓글 전처리 ####
naver <- naver$contents %>% str_replace_all(pattern = '[ㄱ-ㅎ]', '')   # ㅋㅋ, ㅎㅎ
naver <- naver %>% str_replace_all(pattern = '[ㅏ-ㅣ]', '')            # ㅠㅠ
naver <- naver %>% str_replace_all(pattern = '[~!♪♬@#$%^&*()_+=?.]', '') # 특수문자
naver <- naver %>% str_replace_all(pattern = '사진|이모티콘', '')      # 이모티콘
naver <- naver %>% str_replace_all(pattern = '(http.+)', '')           # 홈페이지
naver <- naver %>% str_replace_all(pattern = '[A-Za-z]', '')           # 영어
naver <- naver %>% str_replace_all(pattern = '\\\n', ' ')              # 줄바꿈
naver <- naver %>% str_replace_all(pattern = '\\s+', ' ')   # 여러 칸을 한칸으로

dataN <-  naver %>%  SimplePos09 %>% melt %>% as_tibble %>% select(3, 1) %>% 
  mutate(word=str_match(value, '([가-힣]+)/[NP]')[,2]) %>% 
  select(c(1,3)) %>% dplyr::rename(line=1, word=2) %>% 
  na.omit %>% filter(str_length(word)>=2)

#### _2) 네이버 댓글 워드 클라우드 ####
dataN %>% count(word, sort=T) %>% head(300) %>% 
  with(wordcloud(word, n, random.order=F, rot.per=0.1, scale = c(3, 0.3), 
                 colors=brewer.pal(8,"Dark2")))

#### _3) 네이버 댓글 단어 빈도수 ####
dataN %>% count(word, sort=T) %>% head(15) %>%
  ggbarplot(., x="word", y="n", fill="word", legend="none",
            xlab="단어", ylab="빈도수", label=T)+
  scale_x_discrete(guide=guide_axis(n.dodge=3))
naver[head(grep("정부", naver), 3)]
naver[head(grep("자화자찬", naver), 3)]
naver[head(grep("문재인", naver), 4)]
naver[head(grep("경제", naver), 4)]


#### 4) 다음 댓글 전처리 ####
daum <- daum$content %>% str_replace_all(pattern = '[ㄱ-ㅎ]', '')    # ㅋㅋ, ㅎㅎ
daum <- daum %>% str_replace_all(pattern = '[ㅏ-ㅣ]', '')            # ㅠㅠ
daum <- daum %>% str_replace_all(pattern = '[~!♪♬@#$%^&*()_+=?.]', '') # 특수문자
daum <- daum %>% str_replace_all(pattern = '사진|이모티콘', '')      # 이모티콘
daum <- daum %>% str_replace_all(pattern = '(http.+)', '')           # 홈페이지
daum <- daum %>% str_replace_all(pattern = '[A-Za-z]', '')           # 영어
daum <- daum %>% str_replace_all(pattern = '\\\n', ' ')              # 줄바꿈
daum <- daum %>% str_replace_all(pattern = '\\s+', ' ')    # 여러 칸을 한칸으로

dataD <-  daum %>%  SimplePos09 %>% melt %>% as_tibble %>% select(3, 1) %>% 
  mutate(word=str_match(value, '([가-힣]+)/[NP]')[,2]) %>% 
  select(c(1,3)) %>% dplyr::rename(line=1, word=2) %>% 
  na.omit %>% filter(str_length(word)>=2)

#### _5) 다음 댓글 워드 클라우드 ####
dataD %>% count(word, sort=T) %>% head(300) %>% 
  with(wordcloud(word, n, random.order=F, rot.per=0.1, scale = c(3, 0.3), 
                 colors=brewer.pal(8,"Dark2")))

dataD %>% count(word, sort=T) %>% head(15) %>%
  ggbarplot(., x="word", y="n", fill="word", legend="none",
            xlab="단어", ylab="빈도수", label=T)+
  scale_x_discrete(guide=guide_axis(n.dodge=3))
daum[head(grep("중국", daum), 3)]
daum[head(grep("아니", daum), 3)]
daum[head(grep("신천지", daum), 3)]
daum[head(grep("조선일보", daum), 4)]


#### 나. 단어간 상관관계  ####
library(igraph); library(tidygraph); library(ggraph)

#### 1) 네이버 댓글 단어간 상관관계 ####
print(word_cors <- dataN %>% group_by(word) %>% filter(n()>5) %>%  
        pairwise_cor(word, line, sort=T))

word_cors %>% filter(correlation>=0.25) %>% as_tbl_graph(directed=FALSE) %>% 
  mutate(degree = centrality_degree(), group = group_infomap()) %>% 
  ggraph(layout="fr") + geom_edge_link(color='gray50', alpha=.8) + 
  geom_node_point(aes(color=factor(group), size=degree)) + 
  geom_node_text(aes(label=name), size=6, repel=T) + theme_graph() + 
  theme(legend.position='none')

naver[head(grep("경제", naver), 4)]

word_cors %>% filter(item1 %in% c("자화자찬", "문재인", "중국", "입국금지")) %>% 
  group_by(item1) %>% top_n(10) %>% ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggbarplot(x="item2", y="correlation", fill="item2", legend="none")+
  facet_wrap(~ item1, scales = "free") + coord_flip()


#### _2) 다음 댓글 단어간 상관관계 ####
print(word_cors <- dataD %>% group_by(word) %>% filter(n()>5) %>%  
        pairwise_cor(word, line, sort=T))
daum[head(grep("쓰레기", daum), 4)]

word_cors %>% filter(correlation>=0.25) %>% as_tbl_graph(directed=FALSE) %>% 
  mutate(degree = centrality_degree(), group = group_infomap()) %>% 
  ggraph(layout="fr") + geom_edge_link(color='gray50', alpha=.8) + 
  geom_node_point(aes(color=factor(group), size=degree)) + 
  geom_node_text(aes(label=name), size=6, repel=T) + theme_graph() + 
  theme(legend.position='none')

# 단어간 상관관계 시각화
word_cors %>% filter(item1 %in% c("아니", "대만", "신천지", "조선일보")) %>% 
  group_by(item1) %>% top_n(10) %>% ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggbarplot(x="item2", y="correlation", fill="item2", legend="none")+
  facet_wrap(~ item1, scales = "free") + coord_flip()

naver[head(grep("중국", naver), 3)]







