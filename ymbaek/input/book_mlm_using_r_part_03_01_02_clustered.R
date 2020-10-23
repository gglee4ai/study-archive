#######################################################
#R를 이용한 다층모형 분석##############################
#######################################################

#######################################################
#제3부 다층모형 구성 및 추정###########################
#01장 2층모형##########################################
#02절 군집형 데이터의 경우 ############################
#######################################################
#다층모형 추정 및 사전처리를 위한 라이브러리 구동
library('lme4')
library('lmerTest')
library('tidyverse')
#2수준 군집형 데이터 불러오기 
setwd("D:/data")
clus2 <- read.csv("my2level_cluster.csv",header=TRUE)
head(clus2)

#상위수준(집단): 평균중심화 변환 
#상위수준 데이터로 집산 
clus22 <- group_by(clus2,gid) %>% summarise(gx1=mean(gx1))
clus22
#평균중심화 변환 실시 
clus22 <- mutate(clus22,am.gx1=gx1-mean(gx1))
clus22

#상위수준 독립변수의 평균중심화 변환결과를 통합데이터로 합침
clus2 <- inner_join(select(clus2,-gx1),clus22,by='gid')
#하위수준(개인) 독립변수는 집단평균중심화 변환을 실시
clus2 <- group_by(clus2,gid) %>% 
  mutate(gm.ix1=ix1-mean(ix1),gm.ix2=ix2-mean(ix2))
clus2

#표를 이용하는 방법
#집단별 ix1,ix2 변수와 y변수의 관계를 살펴보자. 
myresult <- data.frame(matrix(NA,nrow=33,ncol=4))
for (i in 1:33){
  temp.subset <- subset(clus2,gid==i)
  temp <- lm(y~ix1+ix2,temp.subset)
  myresult[i,1] <- i
  myresult[i,2:4] <- round(temp$coefficient,3)
}
colnames(myresult) <- c('gid','intercept','coef.ix1','coef.ix2')
head(myresult)
mean(myresult$intercept);sd(myresult$intercept)
mean(myresult$coef.ix1);sd(myresult$coef.ix1)
mean(myresult$coef.ix2);sd(myresult$coef.ix2)

#그림으로 그려보면 이해가 쉽다. 
ggplot(data=clus2,aes(y=y,x=ix1))+
  geom_point(size=1.5,alpha=0.2,colour='red')+
  geom_smooth(method='lm')+
  facet_wrap(~gid)+
  labs(x='ix1 변수',y='종속변수')+
  ggtitle(label='ix1 변수와 y변수 관계')

ggplot(data=clus2,aes(y=y,x=ix2))+
  geom_point(size=1.5,alpha=0.2,colour='red')+
  geom_smooth(method='lm')+
  facet_wrap(~gid)+
  labs(x='ix2 변수',y='종속변수')+
  ggtitle(label='ix2 변수와 y변수 관계')

#집단크기 구하기/독립변수의 평균값 구하기
clus2 <- group_by(clus2,gid) %>% 
  mutate(gsize=length(y),gmix1=mean(ix1),gmix2=mean(ix2))
round(clus2,3)

#위에서 얻은 변수는 모두 전체평균 중심화변환이 필요함 
#상위수준(집단): 평균중심화 변환 
#상위수준 데이터로 집산 
clus22a <- group_by(clus2,gid) %>% 
  summarise(gsize=mean(gsize),gmix1=mean(gmix1),gmix2=mean(gmix2))
clus22a
#평균중심화 변환 실시 
clus22a <- mutate(clus22a,
                  am.gsize=gsize-mean(gsize),
                  am.gmix1=gmix1-mean(gmix1),
                  am.gmix2=gmix2-mean(gmix2))
clus2 <- inner_join(clus2,select(clus22a,-gsize,-gmix1,-gmix2),by='gid')

#개인수준에서 측정된 변수들의 기술통계
mydescriptive <- function(myvariable){
  mysize <- length(myvariable)
  mymean <- round(mean(myvariable),3)
  mysd <- round(sd(myvariable),3)
  mymin <- round(min(myvariable),3)
  mymax <- round(max(myvariable),3)
  mydes <- matrix(c(mysize,mymean,mysd,mymin,mymax),ncol=5)
  colnames(mydes) <- c('n','mean','sd','min','max')
  mydes
}
mydescriptive(clus2$y)
mydescriptive(clus2$ix1)
mydescriptive(clus2$ix2)

#집단수준에서 측정된 변수들의 기술통계
#집산 후 집단크기의 기술통계치 계산 
clus22 <- group_by(clus2,gid) %>% 
  summarise(gsize=mean(gsize),
            mn.ix1=mean(ix1),mn.ix2=mean(ix2),mean(gx1))
mydescriptive(clus22$gsize)
mydescriptive(clus22$mn.ix1)
mydescriptive(clus22$mn.ix2)

#모형0: 기본모형-랜덤효과는 절편에서만
clus2.model0 <- lmer(y~1+(1|gid),clus2)
summary(clus2.model0)
#오차항과 랜덤효과 추출,ICC 계산 
var.cov <- data.frame(VarCorr(clus2.model0))
var.cov
var.cov$vcov[1]/sum(var.cov$vcov)
#정보기준지수들
AIC(clus2.model0);BIC(clus2.model0)

#모형1A: 랜덤효과는 절편과 gm.ix1의 기울기에서 발생
clus2.model1a <- lmer(y~gm.ix1+gm.ix2+(gm.ix1|gid),clus2)
summary(clus2.model1a)
#오차항과 랜덤효과 추출
var.cov <- data.frame(VarCorr(clus2.model1a))
var.cov
round(var.cov$vcov,5)
#정보기준지수들
AIC(clus2.model1a);BIC(clus2.model1a)

#모형1B: 랜덤효과는 절편과 gm.ix2의 기울기에서 발생
clus2.model1b <- lmer(y~gm.ix1+gm.ix2+(gm.ix2|gid),clus2)
summary(clus2.model1b)
#오차항과 랜덤효과 추출 
var.cov <- data.frame(VarCorr(clus2.model1b))
var.cov
round(var.cov$vcov,5)
#정보기준지수들
AIC(clus2.model1b);BIC(clus2.model1b)

#모형1C: 랜덤효과는 절편과 gm.ix1, gm.ix2의 기울기에서 모두 발생
clus2.model1c <- lmer(y~gm.ix1+gm.ix2+(gm.ix1+gm.ix2|gid),clus2)
summary(clus2.model1c)
#오차항과 랜덤효과 추출 
var.cov <- data.frame(VarCorr(clus2.model1c))
var.cov
round(var.cov$vcov,5)
#정보기준지수들
AIC(clus2.model1c);BIC(clus2.model1c)

#모형1D: 랜덤효과 행렬 중 분산만 추정
clus2.model1d <- lmer(y~gm.ix1+gm.ix2+(gm.ix1+gm.ix2||gid),clus2)
summary(clus2.model1d)
#정보기준지수들
AIC(clus2.model1d);BIC(clus2.model1d)

#모형2A: am.gx1 변수만 투입
clus2.model2a <- lmer(y~am.gx1*(gm.ix1+gm.ix2)+
                       (gm.ix2|gid),clus2)
print(summary(clus2.model2a),correlation=FALSE)
#랜덤효과 & 오차항분산
var.cov <- data.frame(VarCorr(clus2.model2a))
round(var.cov$vcov,5)
#모형적합도: 정보기준지수
AIC(clus2.model2a);BIC(clus2.model2a)

#모형2B: am.gx1, am.gsize 변수들 투입
clus2.model2b <- lmer(y~(am.gx1+am.gsize)*(gm.ix1+gm.ix2)+
                       (gm.ix2|gid),clus2)
print(summary(clus2.model2b),correlation=FALSE)
#랜덤효과 & 오차항분산
var.cov <- data.frame(VarCorr(clus2.model2b))
round(var.cov$vcov,5)
#모형적합도: 정보기준지수
AIC(clus2.model2b);BIC(clus2.model2b)

#모형2C: am.gx1, am.gsize, am.gmix1 변수들 투입
clus2.model2c <- lmer(y~(am.gx1+am.gsize+am.gmix1)*(gm.ix1+gm.ix2)+
                       (gm.ix2|gid),clus2)
print(summary(clus2.model2c),correlation=FALSE)
#랜덤효과 & 오차항분산
var.cov <- data.frame(VarCorr(clus2.model2c))
round(var.cov$vcov,5)
#모형적합도: 정보기준지수
AIC(clus2.model2c);BIC(clus2.model2c)

#모형2D: am.gx1, am.gsize, am.gmix1, am.gmix2 변수들 투입
clus2.model2d <- lmer(y~(am.gx1+am.gsize+am.gmix1+am.gmix2)*(gm.ix1+gm.ix2)+
                       (gm.ix2|gid),clus2)
print(summary(clus2.model2d),correlation=FALSE)
#랜덤효과 & 오차항분산
var.cov <- data.frame(VarCorr(clus2.model2d))
round(var.cov$vcov,5)
#모형적합도: 정보기준지수
AIC(clus2.model2d);BIC(clus2.model2d)

#수준별 PRE
var.cov.model1b <- data.frame(VarCorr(clus2.model1b))
var.cov.model2a <- data.frame(VarCorr(clus2.model2a))
(var.cov.model1b$vcov[1]-var.cov.model2a$vcov[1])/var.cov.model1b$vcov[1]
(var.cov.model1b$vcov[2]-var.cov.model2a$vcov[2])/var.cov.model1b$vcov[2]
(var.cov.model1b$vcov[4]-var.cov.model2a$vcov[4])/var.cov.model1b$vcov[4]


#gm.ix1의 랜덤기울기 효과도 고려한 후 am.gsize*gm.ix1 상호작용을 살펴보자.
clus2.model5a <- lmer(y~(am.gx1+am.gsize)*(gm.ix1+gm.ix2)+
                        (gm.ix1+gm.ix2|gid),clus2)
print(summary(clus2.model5a),correlation=FALSE)

##다층모형 추정결과 그래프로 나타내기 
fig.data <- group_by(clus2,gid) %>% 
  summarise(x.min=min(gm.ix2),x.max=max(gm.ix2),
            am.gx1=mean(am.gx1),gx1=mean(gx1))
fig.data <- gather(fig.data,"value",gm.ix2,-gid,-am.gx1,-gx1)
fig.data$range <- ifelse(fig.data$value=='x.min',0,1)
table(fig.data$gx1)
#gm.ix1 변수 통제 
fig.data$gm.ix1 <- 0 
#그래프 가독성을 위해 텍스트형 변수생성 
mylabel <- c('매우낮음','낮음','다소낮음','중간','다소높음','높음','매우높음')
fig.data$gx1.label <- factor(mylabel[fig.data$gx1],levels=mylabel)

#모형2A를 이용해 그래프에 제시될 예측값 추정
fig.data$predy <- predict(clus2.model2a,fig.data)

#각 집단별로 패시팅을 적용한 후 상호작용 효과를 그래프로 그림 
ggplot(fig.data,aes(y=predy,x=gm.ix2))+
  geom_point(size=2)+
  geom_line(aes(y=predy,group=gid))+
  labs(x='타자신뢰도\n(집단평균중심화 변환된 ix2 변수)',
       y='예측된 기부의도')+
  facet_grid(~gx1.label)

#전체집단의 예측값 평균을 구함
temp <- fig.data
temp$gm.ix2 <- ifelse(temp$range==0,-2,2)
fig.data.pop <- group_by(temp,gm.ix2,am.gx1) %>%
  select(-value,-gx1.label) %>%
  summarise_all(mean)
fig.data.pop$gx1.label <- factor(mylabel[fig.data.pop$gx1],
                                 levels=mylabel)

#랜덤효과와 표본전체의 패턴을 같이 제시
ggplot(data=fig.data,aes(x=gm.ix2,y=predy,colour=gx1.label)) +
  geom_point(aes(group=gid),size=2,alpha=0.8) +
  geom_line(aes(y=predy,group=gid),
            alpha=0.8,linetype=3,size=1) +
  geom_line(data=fig.data.pop,aes(y=predy),linetype=1,size=2) +
  labs(x='타자신뢰도\n(집단평균중심화 변환된 ix2 변수)',
       y='예측된 기부의도',col='집단신용도')

