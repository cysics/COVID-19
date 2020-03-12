#### 1. 2월 26일 오전 9시 기준 ####
#### 가. 가장 손쉬운 분석 ####
# 한국, 중국 비교
prop.test(c(11, 2715), c(1146, 78064))

# 한국, 중국, 일본 비교
prop.test(c(11, 2715, 4), c(1146, 78064, 855))
pairwise.prop.test(c(11, 2715, 4), c(1146, 78064, 855))

# 한국, 중국, 일본, 홍콩, 대만 비교
prop.test(c(11, 2715, 4, 2, 1), c(1146, 78064, 855, 85, 31))
pairwise.prop.test(c(11, 2715, 4, 2, 1), c(1146, 78064, 855, 85, 31))


#### 나. 조금 복잡하지만 보다 정확한 분석 ####
#### _1) 한국, 중국, 일본 비교 ####
print(data <- matrix(c(11, 2715, 4, 
                       1146-11, 78064-2715, 855-4), nrow=3, 
                     dimnames=list(c("한국","중국","일본"), c("사망","생존"))))
addmargins(data)
prop.table(data, margin=1)
round(prop.table(data, margin=1), 4)
round(prop.table(data, margin=1), 4)*100

chisq.test(data)

library(rcompanion)
print(chisq.ph <- pairwiseNominalIndependence(data))
pairwise.prop.test(c(11, 2715, 4), c(1146, 78064, 855), p.adjust.method="fdr")
pairwise.prop.test(c(11, 2715, 4), c(1146, 78064, 855), p.adjust.method="holm")
pairwise.prop.test(c(11, 2715, 4), c(1146, 78064, 855), p.adjust.method="bonferroni")

print(chisq.ph <- pairwiseNominalIndependence(data, method="holm"))

library(dplyr)
print(chisq.ph <- pairwiseNominalIndependence(data, method="holm") %>% 
        mutate_if(is.numeric, round, 5))
cldList(p.adj.Chisq ~ Comparison, chisq.ph)


#### _2) 한국, 중국, 일본, 홍콩, 대만 비교 ####
print(data <- matrix(c(11, 2715, 4, 2, 1,
                       1146-11, 78064-2715, 855-4, 85-2, 31-1), nrow=5, 
    dimnames=list(c("한국","중국","일본", "홍콩", "대만"), c("사망","생존"))))
addmargins(data)
round(prop.table(data, margin=1), 4)*100

chisq.test(data)
fisher.test(data)
fisher.test(data, simulate.p.value=T)

library(rcompanion)
print(chisq.ph <- pairwiseNominalIndependence(data, method="holm"))
cldList(p.adj.Fisher ~ Comparison, chisq.ph)

#### _3) 한국, 일본, 홍콩, 대만 비교 ####
print(data <- matrix(c(11, 4, 2, 1,
                       1146-11, 855-4, 85-2, 31-1), nrow=4, 
   dimnames=list(c("한국","일본", "홍콩", "대만"), c("사망","생존"))))
addmargins(data)
round(prop.table(addmargins(data), margin=1)*2, 4)*100

chisq.test(data)
fisher.test(data)
fisher.test(data, simulate.p.value=T)


