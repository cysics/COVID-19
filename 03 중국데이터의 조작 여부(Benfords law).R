library(tabulizer); library(tidyverse); library(dplyr); library(webshot); library(BenfordTests)

#### 기생충 총수입  Benford test ####
webshot("https://namu.wiki/w/%EA%B8%B0%EC%83%9D%EC%B6%A9(%EC%98%81%ED%99%94)/%ED%9D%A5%ED%96%89", "Parasite.pdf")
print(Parasite <- extract_tables("Parasite.pdf", pages=1, encoding="UTF-8")[2] %>% as.data.frame())
print(Parasite <- Parasite %>% slice(c(7:37, 39:41, 44)) %>% 
        separate(X2, sep=" ", into=c("X3", "X4", "X5", "X6", "X7", "X8", "X9")) %>%
        select(c(4,5)) %>% rename(개봉일=1, 총수입=2))
print(Parasite <- lapply(Parasite, function(x) gsub("\\일", "", x)) %>% as.data.frame())
print(Parasite <- lapply(Parasite, function(x) gsub("\\$", "", x)) %>% as.data.frame())
print(Parasite <- lapply(Parasite, function(x) gsub("\\,", "", x)) %>% as.data.frame())
print(Parasite <- lapply(Parasite, function(x) as.numeric(as.character(x))) %>% as.data.frame())

chisq.benftest(Parasite$개봉일, digits=1) 
# 일부 데이터가 바뀌면서 벤포드 법칙을 만족하는 것으로 나오네요. ㅠㅠ

chisq.benftest(Parasite$총수입, digits=1)
pbenf(digits=1)
signifd.analysis(Parasite$총수입, digits=1)


#### 우리나라 지역벼 코로나19 관련 데이터 불러오기 ####
webshot("http://ncov.mohw.go.kr/bdBoardList_Real.do?brdId=1&brdGubun=13&ncvContSeq=&contSeq=&board_id=&gubun=", "covid19.pdf")
print(korea <- extract_tables("covid19.pdf", pages=1, encoding="UTF-8")[2] %>% as.data.frame() %>% 
        slice(5:21) %>% select(c(4,6,8)) %>% rename(확진자수=1, 격리해제수=2, 사망자=3))
print(korea <- lapply(korea, function(x) gsub("\\,", "", x)) %>% as.data.frame())
print(korea <- lapply(korea, function(x) as.numeric(as.character(x))) %>% as.data.frame())

chisq.benftest(korea$확진자수, digits=1)
chisq.benftest(korea$격리해제수, digits=1)
chisq.benftest(korea$사망자, digits=1)
chisq.benftest(union(union(korea$확진자수, korea$격리해제수), korea$사망자), digits=1)

chisq.benftest(korea$확진자수, digits=2)
chisq.benftest(korea$격리해제수, digits=2)
chisq.benftest(korea$사망자, digits=2)

chisq.benftest(korea$확진자수*2, digits=1)
chisq.benftest(korea$격리해제수*2, digits=1)
chisq.benftest(korea$사망자*2, digits=1)



#### 중국 지역별 코로나19 데이터 불러오기 ####
print(china1 <- extract_tables("https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200215-sitrep-26-covid-19.pdf", pages=2)
      %>% as.data.frame())
print(china1 <- china1 %>% slice(6:39) %>% separate(X9, sep=" ", into=c("XX", "확진자")) %>% select(c(2, 10, 11)) %>% rename(인구=1, 확진자=2, 사망자=3)) 
print(china1 <- lapply(china1, function(x) as.numeric(as.character(x))) %>% as.data.frame())

chisq.benftest(china1$인구, digits=1)
chisq.benftest(china1$확진자, digits=1)
chisq.benftest(china1$사망자, digits=1)

chisq.benftest(china1$인구, digits=2)
chisq.benftest(china1$확진자, digits=2)

chisq.benftest(china1$인구*2, digits=2)
chisq.benftest(china1$확진자*2, digits=2)

chisq.benftest(china1$인구*3, digits=2)
chisq.benftest(china1$확진자*3, digits=2)


print(china2 <- extract_tables("https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200221-sitrep-32-covid-19.pdf", pages=3) %>% as.data.frame())
print(china2 <- china2 %>% slice(8:41) %>% separate(X2, sep=" ", into=c("X3", "확진자", "사망자")) %>%
        select(c(3, 4)) %>% rename(확진자=1,사망자=2))
print(china2 <- lapply(china2, function(x) as.numeric(as.character(x))) %>% as.data.frame())

chisq.benftest(china2$확진자, digits=1)
chisq.benftest(china2$사망자, digits=1)

chisq.benftest(china2$확진자, digits=2)

chisq.benftest(china2$확진자*2, digits=2)

chisq.benftest(china2$확진자*3, digits=1)

chisq.benftest(china2$확진자*7, digits=1)


print(china3 <- extract_tables("https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200227-sitrep-38-covid-19.pdf", pages=3) %>% as.data.frame())
print(china3 <- china3 %>% slice(7:40) %>% separate(X6, sep=" ", into=c("확진자", "사망자")) %>% 
        select(c(6, 7)) %>% rename(확진자=1,사망자=2))
print(china3 <- lapply(china2, function(x) as.numeric(as.character(x))) %>% as.data.frame())

chisq.benftest(china3$확진자, digits=1)
chisq.benftest(china3$사망자, digits=1)

chisq.benftest(china3$확진자, digits=2)

chisq.benftest(china3$확진자*2, digits=2)

chisq.benftest(china3$확진자*6, digits=1)


# 그외 날짜들은 생략




