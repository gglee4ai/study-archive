########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제3부 다층모형 구성 및 추정############################
#03장 종속변수가 비정규분포인 경우의 다층모형###########
#01절 종속변수가 이분변수인 경우 #######################
########################################################
library('lme4')
library('lmerTest')
library('tidyverse')
#데이터 불러오기 
setwd("D:/data")
L2.dich <- read.csv("my2level_dich.csv",header=T)
head(L2.dich)
#하위수준 사례수 
dim(L2.dich)
#상위수준 사례수 
length(unique(L2.dich$gid))

#종속변수의 형태를 살펴보자. 
table(L2.dich$y)

#개인수준 독립변수에 대한 집단평균 중심화변환 
L2.dich <- group_by(L2.dich,gid) %>% 
  mutate(gm.ix1=ix1-mean(ix1),gm.ix2=ix2-mean(ix2))
round(L2.dich,2)

#집단수준 독립변수에 대한 전체평균 중심화변환 
temp <- group_by(L2.dich,gid) %>%  
  summarise(gx=mean(gx),gsize=length(gid),
            gmix1=mean(ix1),gmix2=mean(ix2))
temp <- mutate(temp,am.gx=gx-mean(gx),
               am.gsize=gsize-mean(gsize),
               am.gmix1=gmix1-mean(gmix1),
               am.gmix2=gmix2-mean(gmix2))
round(temp,2)

#개인수준 데이터와 집단수준 데이터 통합
temp <- select(temp,-gx)
L2.dich <- inner_join(L2.dich,temp,by='gid')
round(L2.dich,2)

#기술통계치 정리 
#개인수준 
mysummary <- function(myobject,myvariable){
  myresult <- summarise(myobject,size=length(myvariable),
                        MEAN=mean(myvariable),SD=sd(myvariable),
                        MIN=min(myvariable),MAX=max(myvariable))
  round(myresult,3)
}
mysummary(ungroup(L2.dich),ungroup(L2.dich)$y)
mysummary(ungroup(L2.dich),ungroup(L2.dich)$ix1)
mysummary(ungroup(L2.dich),ungroup(L2.dich)$ix2)

#집단수준 
L22.dich <- group_by(L2.dich,gid) %>% 
  summarise_all(mean) 
mysummary(L22.dich,L22.dich$gsize)
mysummary(L22.dich,L22.dich$gmix1)
mysummary(L22.dich,L22.dich$gmix2)
mysummary(L22.dich,L22.dich$gx)

#기본모형
gmlm.dich.m0 <- glmer(y~1+(1|gid),
                      data=L2.dich,binomial(link="logit"))
summary(gmlm.dich.m0)
#랜덤효과 추출
var.cov <- data.frame(VarCorr(gmlm.dich.m0))
var.cov


#이분변수인 경우의 ICC 
myICC2.dich <- function(gmlm.dichotomous){
  var.cov <- data.frame(VarCorr(gmlm.dichotomous))
  icc2.dich <- var.cov$vcov/(var.cov$vcov+((pi^2)/3))
  icc2.dich 
}
myICC2.dich(gmlm.dich.m0)

#개인수준 변수의 고정효과 투입
gmlm.dich.m1a <- glmer(y~gm.ix1+gm.ix2+(gm.ix1|gid),
                       data=L2.dich,binomial(link="logit"))
summary(gmlm.dich.m1a)

gmlm.dich.m1b <- glmer(y~gm.ix1+gm.ix2+(gm.ix2|gid),
                       data=L2.dich,binomial(link="logit"))
summary(gmlm.dich.m1b)

gmlm.dich.m1c <- glmer(y~gm.ix1+gm.ix2+(gm.ix1+gm.ix2|gid),
                       data=L2.dich,binomial(link="logit"))
summary(gmlm.dich.m1c)

gmlm.dich.m1d <- glmer(y~gm.ix1+gm.ix2+(gm.ix1+gm.ix2||gid),
                       data=L2.dich,binomial(link="logit"))
summary(gmlm.dich.m1d)

#Footnote
anova(gmlm.dich.m0, gmlm.dich.m1b)
anova(gmlm.dich.m1b, gmlm.dich.m1c)

#Footnote 
gmlm.dich.m1b <- glmer(y~gm.ix1+gm.ix2+(gm.ix2|gid),
                       data=L2.dich,binomial(link="logit"),
                       control=glmerControl(optimizer = c("Nelder_Mead")))
summary(gmlm.dich.m1b)

#집단수준의 독립변수 투입 
gmlm.dich.m2a <- glmer(y~am.gx+gm.ix1+gm.ix2+
                         (gm.ix2|gid),
                       data=L2.dich,binomial(link="logit"),
                       control=glmerControl(optimizer = c("Nelder_Mead")))
summary(gmlm.dich.m2a)

gmlm.dich.m2b <- glmer(y~am.gx*gm.ix1+gm.ix2+
                         (gm.ix2|gid),
                       data=L2.dich,binomial(link="logit"),
                       control=glmerControl(optimizer = c("Nelder_Mead")))
summary(gmlm.dich.m2b)

gmlm.dich.m2c <- glmer(y~gm.ix1+am.gx*gm.ix2+
                         (gm.ix2|gid),
                       data=L2.dich,binomial(link="logit"),
                       control=glmerControl(optimizer = c("Nelder_Mead")))
summary(gmlm.dich.m2c)

gmlm.dich.m2d <- glmer(y~am.gx*(gm.ix2+gm.ix1)+
                         (gm.ix2|gid),
                       data=L2.dich,binomial(link="logit"),
                       control=glmerControl(optimizer = c("Nelder_Mead")))
summary(gmlm.dich.m2d)

vcov.m1b <- data.frame(VarCorr(gmlm.dich.m1b))$vcov
vcov.m2c <- data.frame(VarCorr(gmlm.dich.m2c))$vcov
(vcov.m1b[1]-vcov.m2c[1])/vcov.m1b[1]
(vcov.m1b[2]-vcov.m2c[2])/vcov.m1b[2]


#gm.ix1 변수의 주효과 
#각 집단별로 ix1의 수준별로 데이터 생성
fig.data <- group_by(L2.dich,gid,ix1) %>% 
  summarise_all(mean)
#모형에 투입된 나머지 변수들의 경우 통제 (즉 0을 부과)
fig.data$gm.ix2 <- 0
fig.data$am.gx <- 0

#아래와 같이 하면 로짓변환 값이 추출된다. 
fig.data$logit <- predict(gmlm.dich.m2c,fig.data)
#확률값을 위한다면 다음과 같이 해야 한다. 
fig.data$predy <- predict(gmlm.dich.m2c,fig.data,type='response')

#전체집단의 헌혈참여 확률 평균을 구함
xrange <- quantile(fig.data$gm.ix1,0.1*(1:9))
mycoef <- fixef(gmlm.dich.m2c)
mylogit <- mycoef[1]+xrange*mycoef[2]
myresponse <- 1/(1+exp(-1*mylogit))
fig.data.pop <- fig.data[1:9,]
fig.data.pop$gm.ix1 <- xrange
fig.data.pop$predy <- myresponse

#랜덤효과와 표본전체의 패턴을 같이 제시
ggplot(data=fig.data,aes(x=gm.ix1,y=predy)) +
  geom_line(aes(y=predy,group=gid),
            alpha=0.5,linetype=3,size=0.5) +
  geom_line(data=fig.data.pop,aes(y=predy),linetype=1,size=1.5) +
  labs(x='헌혈에 대한 응답자태도\n(집단평균중심화 변환된 ix1 변수)',
       y='예측된 헌혈참여확률')

#gm.ix2 변수와 am.gx 변수의 상호작용효과 
fig.data <- group_by(L2.dich,gid,gm.ix2) %>% 
  summarise(am.gx=mean(am.gx),gx=mean(gx))
#gm.ix1 변수 통제 
fig.data$gm.ix1 <- 0 
#그래프 가독성을 위해 텍스트형 변수생성 
mylabel <- c('매우낮음','낮음','다소낮음','중간','다소높음','높음','매우높음')
fig.data$gx.label <- factor(mylabel[fig.data$gx],levels=mylabel)

#모형4를 이용해 그래프에 제시될 예측값 추정
fig.data$predy <- predict(gmlm.dich.m2c,fig.data,type='response')

#각 집단별로 패시팅을 적용한 후 상호작용 효과를 그래프로 그림 
ggplot(fig.data,aes(y=predy,x=gm.ix2))+
  geom_point(size=1)+
  geom_line(aes(y=predy,group=gid))+
  labs(x='헌혈에 대한 응답자태도\n(집단평균중심화 변환된 ix2 변수)',
       y='예측된 헌혈참여확률')+
  facet_grid(~gx.label)
