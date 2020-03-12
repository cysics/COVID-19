#### 1. 데이터 불러오기 ####
library(corona19)
rdata <- getdata("patient")


#### 2. 데이터 전처리 #####
library(dplyr)
colnames(rdata)
data <- rdata %>% select(c("patient_id", "confirmed_date", "released_date", 
                           "deceased_date", "sex", "birth_year"))
data <- data[!is.na(data$confirmed_date) & !is.na(data$sex) & 
             !is.na(data$birth_year),] %>% 
  mutate(sex=as.factor(sex)) %>% mutate(Age=2021-birth_year) %>% 
  mutate(Fstate=ifelse(is.na(deceased_date), 0, 1)) %>% 
  mutate(Lenfol=ifelse(!is.na(deceased_date), 
         ifelse(deceased_date<confirmed_date, 0, deceased_date-confirmed_date), 
         Sys.Date()-confirmed_date))

colSums(is.na(data)) #중요변수Lenfol(추적총기간),Fstate(최종 상태),sex(성별),Age(나이)
summary(as.factor(data$Fstate)) # 12일 기준으로 죽은사람 27명? 지난번 보다 줄었네요. 
fit <- list()



#### 3. 성별에 따른 생존분석  ####
#### 가. Kaplan-Meier ####
library(survival); library(survminer)
summary(fit$Gender <- survfit(Surv(Lenfol, Fstate)~sex, data))
ggsurvplot(fit$Gender, data, pval=T, pval.coord=c(13, 0.5))
ggsurvplot(fit$Gender, data, pval=T, fun="event", pval.coord=c(25, 0.05))

#### 나. Log-Rank test ####
survdiff(Surv(Lenfol, Fstate)~sex, data)
# 이하 데이터는 이전 데이터이므로 필요한 경우 위 결과를 보고 수정하세요.
# 변형된 Log-Rank test : 2.37 + 2.43 = 4.80 > 3.84
10/209            # 여자 사망률 4.78
22/208            # 남자 사망률 10.58
(22/208)/(10/209) # 단순 사망률 2.21배
prop.test(c(22,10), c(208, 209))

#### 다. Cox proportional hazard model ####
summary(fit$Gender.coxph <- coxph(Surv(Lenfol, Fstate)~sex, data))
ggforest(fit$Gender.coxph, data)



#### 4. 나이에 따른 생존분석 ####
#### 가. Kaplan-Meier ####
library(survival); library(survminer)
summary(fit$Age <- survfit(Surv(Lenfol, Fstate)~Age, data))
plot(fit$Age)

print(data <- data %>% mutate(AgeCut=ifelse(Age<20, "10이하", ifelse(Age<30, "20대",
              ifelse(Age<40, "30대", ifelse(Age<50, "40대", ifelse(Age<60, "50대",
              ifelse(Age<70, "60대", ifelse(Age<80, "70대", "80이상")))))))) %>% 
              mutate(AgeCut=as.factor(AgeCut)))
summary(fit$AgeCut <- survfit(Surv(Lenfol, Fstate)~AgeCut, data))
ggsurvplot(fit$AgeCut, data, pval=T, pval.coord=c(15, 0.5))
ggsurvplot(fit$AgeCut, data, pval=T, fun="event", pval.coord=c(25, 0.3))

library(maxstat)
plot(fit$maxstat <- maxstat.test(Surv(Lenfol, Fstate)~Age, data,
                                   smethod="LogRank",pmethod="condMC",B=999))
print(fit$Cutpoint <- fit$maxstat$estimate)
data$AgeCut <- ifelse(data$Age>fit$Cutpoint, 1, 0)

summary(fit$AgeCut <- survfit(Surv(Lenfol, Fstate)~AgeCut, data))
ggsurvplot(fit$AgeCut, data, pval=T, pval.coord=c(15, 0.5))
ggsurvplot(fit$AgeCut, data, pval=T, fun="event", pval.coord=c(15, 0.2))

#### 나. Log-Rank test ####
survdiff(Surv(Lenfol, Fstate)~AgeCut, data)
# 이하 데이터도 필요한 경우 위 코드 결과를 보고 수정하세요.
13/361           #67세 이하는 3.6%
19/56            #67세 초과은 33.9%
(19/56)/(13/361) # 9.4배

#### 다. Cox proportional hazard model ####
summary(fit$CutAge.coxph <- coxph(Surv(Lenfol, Fstate)~AgeCut, data))
ggforest(fit$CutAge.coxph, data)

summary(fit$Age.coxph <- coxph(Surv(Lenfol, Fstate)~Age, data))
ggforest(fit$Age.coxph, data)




#### 5. 성별과 나이에 따른 생존분석 ####
summary(fit$total <- survfit(Surv(Lenfol, Fstate)~sex+AgeCut, data))
ggsurvplot(fit$total, data, pval=T, pval.coord=c(15, 0.5), legend="right")
ggsurvplot(fit$total, data, pval=T, fun="event", pval.coord=c(15, 0.2), legend="right")

survdiff(Surv(Lenfol, Fstate)~sex+Age, data)
survdiff(Surv(Lenfol, Fstate)~sex+AgeCut, data)

summary(fit$total.coxph <- coxph(Surv(Lenfol, Fstate)~sex+Age, data))
summary(fit$total.coxph <- coxph(Surv(Lenfol, Fstate)~sex+Age+sex*Age, data))
summary(fit$total.coxph <- coxph(Surv(Lenfol, Fstate)~sex+AgeCut, data))
summary(fit$total.coxph <- coxph(Surv(Lenfol, Fstate)~sex+AgeCut+sex*AgeCut, data))
ggforest(fit$total.coxph, data)

library(riskRegression)
summary(fit$model1 <- coxph(Surv(Lenfol, Fstate)~sex+Age, data, x=T))
summary(fit$model2 <- coxph(Surv(Lenfol, Fstate)~sex+AgeCut, data, x=T))
plotROC(Score(list(model1=fit$model1, model2=fit$model2), Hist(Lenfol, Fstate)~1, data=data,
              times=5, plots="roc", metrics="auc"))





