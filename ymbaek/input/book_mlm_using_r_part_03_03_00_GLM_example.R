########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제3부 다층모형 구성 및 추정############################
#GLM ###################################################
########################################################

setwd("D:/data")
mydata <- read.csv("GLM_example.csv",header=TRUE)
head(mydata)
#정규분포가 아닌 변수의 경우: 이항분포
table(mydata$y.dich1)
#정규분포가 아닌 변수의 경우: 포아송 분포
mean(mydata$y.pois1); sd(mydata$y.pois1)

#종속변수가 정규분포인 경우 
myglm.norm <- glm(y.norm ~ x, mydata, 
                  family=gaussian('identity'))
summary(myglm.norm)
# 다음과 동일한 결과
summary(lm(y.norm ~ x, mydata))

#로지스틱 회귀분석 
myglm.dich1 <- glm(y.dich1~x,mydata,family=binomial(link='logit'))
summary(myglm.dich1)

#포아송 회귀분석 
myglm.pois1 <- glm(y.pois1~x,mydata,family=poisson(link = "log"))
summary(myglm.pois1)
