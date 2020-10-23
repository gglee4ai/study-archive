########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제4부 제4부 기타 고급통계기법과 다층모형###############
#02장 문항반응이론(Item Response Theory) 모형###########
########################################################
# 라이브러리 구동 
library('lme4')
library('lmerTest')
library('tidyverse')

data(VerbAgg)
head(VerbAgg)

dim(VerbAgg)[1]
length(unique(VerbAgg$id))
length(unique(VerbAgg$item))

# 종속변수를 이분변수로 
VerbAgg$y <- ifelse(VerbAgg$r2=='Y',1,0)
# 기본모형 추정 
irt.m0 <- glmer(y ~ 1+(1|id)+(1|item),
                data=VerbAgg, 
                family=binomial(link='logit'))
summary(irt.m0)

# 개별 응답자의 랜덤효과 
person.property <- data.frame(ranef(irt.m0)$id)
person.property$personid <- rownames(person.property)
colnames(person.property)[1] <- 'random.effect'
# 개별 문항의 랜덤효과 
item.property <- data.frame(ranef(irt.m0)$item)
item.property$itemid <- rownames(item.property)
colnames(item.property)[1] <- 'random.effect'
item.property[order(item.property$random.effect),]

# 개인수준 데이터 사전처리 
VerbAgg$female <- ifelse(VerbAgg$Gender=='M',0,1)
temp <- group_by(VerbAgg,id) %>% 
  summarise(anger=mean(Anger))
temp <- mutate(ungroup(temp),am.anger=anger-mean(anger))
VerbAgg <- inner_join(select(ungroup(temp),-anger),
                      VerbAgg,by='id')

# 개인 응답자 수준 독립변수 투입 
irt.m1 <- glmer(y ~ female+am.anger+(1|id)+(1|item),
            data=VerbAgg, 
            family=binomial(link='logit'))
print(summary(irt.m1),correlation=FALSE)

# 개별 문항 수준 독립변수 투입 
irt.m2 <- glmer(y ~ btype+situ+mode+female+am.anger+(1|id)+(1|item),
                data=VerbAgg, 
                family=binomial(link='logit'))
print(summary(irt.m2),correlation=FALSE)

# 오차감소비율(PRE)
re0 <- data.frame(VarCorr(irt.m0))$vcov
re2 <- data.frame(VarCorr(irt.m2))$vcov
round((re0-re2)/re0, 4)

# 랜덤효과 비교 
summary(ranef(irt.m0)$item)
summary(ranef(irt.m2)$item)

# 데이터의 형태전환 
library('eRm')
temp <- select(VerbAgg,id,item,y)
VerbAgg.wide <- spread(temp,item,y)
# 1PL IRT 실시 
res <- RM(select(VerbAgg.wide,-id))
summary(res)

# 결과의 비교 
temp <- cbind(data.frame(ranef(irt.m0)$item),data.frame(res$betapar))
colnames(temp) <- c('b.lme4','b.eRm')
cor.test(temp$b.eRm,temp$b.lme4)


