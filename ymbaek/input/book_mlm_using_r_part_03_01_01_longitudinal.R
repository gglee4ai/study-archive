#######################################################
#R를 이용한 다층모형 분석##############################
#######################################################

#######################################################
#제3부 다층모형 구성 및 추정###########################
#01장 2층모형##########################################
#01절 시계열 데이터의 경우 ############################
#######################################################
#아래의 두 라이브러리가 필요
library('lme4')
library('lmerTest')
#사전처리/그래프 작성을 위해 다음의 라리브러리 설치 
library('tidyverse')
#2수준 시계열 데이터 불러오기 
setwd("D:/data")
rpt2 <- read.csv("my2level_repeat.csv",header=TRUE)
summary(rpt2)
#상위수준(개인): 가변수의 경우 비교코딩(contrast coding)
rpt2$cc.female <- ifelse(rpt2$female==0,-1,1)
table(rpt2$cc.female,rpt2$female)
#넓은 형태 -> 긴형태 
rpt2long <- reshape(rpt2,idvar='pid',varying=list(3:7),
                    v.names = "y",direction='long')
#집단평균 중심화 변환 실시 
rpt2long  <- group_by(rpt2long,pid) %>% 
  mutate(gm.time = time-mean(time))
#성별에 따른 변화패턴을 살펴보자. 
time.change <- group_by(rpt2long,female,time) %>% 
  summarise(mean(y))
round(time.change,3)
#그림으로 그리면 보다 이해가 쉽다
ggplot(time.change,aes(x=time,y=`mean(y)`,shape=as.factor(female)))+
  geom_line(stat='identity')+
  geom_point(size=4)+
  labs(x='시점',y='호감도',shape='성별')+
  scale_shape_manual(values=0:1,labels=c('남','녀'))+
  scale_y_continuous(limits=c(2,4))

#무작위로 20명의 사례를 선정하였음 
rpt2long.20 <- arrange(rpt2long,pid)
rpt2long.20$myselect <- rep(sample(1:480,size=480,replace=FALSE),each=5)
rpt2long.20 <- filter(rpt2long.20,myselect<21) %>% arrange(pid,time)

ggplot(rpt2long.20,aes(x=time,y=y))+
  geom_line(stat='identity')+
  geom_point(size=2)+
  geom_smooth(se = FALSE, method = "lm")+
  labs(x='시점',y='호감도')+
  scale_y_continuous(limits=c(0.5,5.5))+
  facet_wrap(~pid)

# 기술통계치 계산: 시간수준
level1.y <- summarise(ungroup(rpt2long),
          length(y),mean(y),sd(y),min(y),max(y))
level1.time <- summarise(ungroup(rpt2long),
          length(time),mean(time),sd(time),min(time),max(time))
round(level1.y,3)
round(level1.time,3)
# 기술통계치 계산: 개인수준
level2.female <- group_by(rpt2long,pid) %>% 
  summarise_all(mean) %>% 
  summarise(length(female),mean(female),sd(female),min(female),max(female))
round(level2.female,3)


#모형0: 기본모형 
rpt2.model0 <- lmer(y~1+(1|pid),rpt2long)
summary(rpt2.model0)
#ICC
.04476/(.04476+.50194)

#모형1A: 일차항만 투입
rpt2.model1a <- lmer(y~gm.time+(gm.time|pid),rpt2long)
summary(rpt2.model1a)
#오차항과 랜덤효과 추출 
var.cov <- data.frame(VarCorr(rpt2.model1a))
var.cov
round(var.cov$vcov,5)

#모형1B: 이차항을 추가로 투입
#이차항은 I(gm.time2^2)과 같이 넣어도 되지만, 다음과 같이 하는 것이 더 깔끔함
rpt2long$gm.time2 <- rpt2long$gm.time^2
rpt2.model1b <- lmer(y~gm.time+gm.time2+(gm.time+gm.time2|pid),rpt2long)
summary(rpt2.model1b)
#고정효과 추출
round(summary(rpt2.model1b)$coefficients,3)
#오차항과 랜덤효과 추출 
var.cov <- data.frame(VarCorr(rpt2.model1b))
var.cov
round(var.cov$vcov,5)

#모형적합도(goodness-of-fit) 비교 
AIC(rpt2.model0); BIC(rpt2.model0)
AIC(rpt2.model1a); BIC(rpt2.model1a)
AIC(rpt2.model1b); BIC(rpt2.model1b)

#랜덤효과 부분은 model1A, 고정효과 부분은 model1B
rpt2.model1c <- lmer(y~gm.time+gm.time2+(gm.time|pid),rpt2long)
AIC(rpt2.model1c); BIC(rpt2.model1c)
#오차항과 랜덤효과 추출 
var.cov <- data.frame(VarCorr(rpt2.model1c))
var.cov
round(var.cov$vcov,5)
summary(rpt2.model1c)

#상위수준(개인수준)의 독립변수를 추가로 투입
rpt2.model2a <- lmer(y~cc.female*(gm.time+gm.time2)+
                      (gm.time|pid),rpt2long)
print(summary(rpt2.model2a),correlation=FALSE)
AIC(rpt2.model2a); BIC(rpt2.model2a)

#고정효과
round(summary(rpt2.model2a)$coefficients,3)
#랜덤효과 
var.cov <- data.frame(VarCorr(rpt2.model2a))
var.cov
round(var.cov$vcov,5)

#분산들을 추출
var.cov3 <- data.frame(VarCorr(rpt2.model1c))
var.cov4 <- data.frame(VarCorr(rpt2.model2a))
#전체분산
(total.var3 <- sum(var.cov3$vcov))
(total.var4 <- sum(var.cov4$vcov))
#level-1분산
(level1.var3 <- var.cov3$vcov[4])
(level1.var4 <- var.cov4$vcov[4])
#level-2분산
(level2.var3 <- sum(var.cov3$vcov[1:3]))
(level2.var4 <- sum(var.cov4$vcov[1:3]))

(pre.level1 <- (level1.var3 - level1.var4)/level1.var3)
(pre.level2 <- (level2.var3 - level2.var4)/level2.var3)
(pre.total <- (total.var3 - total.var4)/total.var3)

#다층모형 추정결과 그래프
#전체 데이터를 대상으로 예측값을 저장
rpt2long$predy <- predict(rpt2.model2a,rpt2long)
#랜덤효과가 0이라고 가정된 경우(population average)
rpt2pop <- group_by(rpt2long,gm.time,cc.female) %>%
  summarise_all(mean)
#랜덤효과를 통제한 후 표본전체의 패턴을 제시 
ggplot(data=rpt2pop,aes(x=time,y=predy,shape=as.factor(female))) +
  geom_line(stat='identity',size=1)+
  geom_point(size=3)+
  scale_shape_manual(values=0:1,labels=c('남','녀'))+
  labs(x='시점',y='호감도',shape='성별')+
  theme(legend.position="top")

#랜덤효과를 제시하되 성별에 따라 색을 달리 표현함
ggplot(data=rpt2long,aes(x=time,y=predy,colour=factor(female))) +
  geom_point(size=1,alpha=0.1) +  
  geom_line(aes(y=predy,group=pid),alpha=0.1) +
  scale_colour_manual(values=c('0'='blue','1'='red'),labels=c("남성","여성"))+  
  labs(x='시점',y='호감도',col='성별')

#랜덤효과와 표본전체의 패턴을 같이 제시
ggplot(data=rpt2long,aes(x=time,y=predy,colour=factor(female))) +
  geom_point(size=1,alpha=0.1) +
  geom_line(aes(y=predy,group=pid,size="응답자"),alpha=0.1) +
  geom_line(data=rpt2pop,aes(y=predy,size="집단전체")) +
  scale_size_manual(name="예측선",values=c("응답자"=0.5,"집단전체"=2))+
  scale_colour_manual(values=c('0'='blue','1'='red'),labels=c("남성","여성"))+  
  labs(x='시점',y='호감도',col='성별')

#그림의 가독성을 높이기 위해 라벨작업 
rpt2long$fem.label <- ifelse(rpt2long$female==0,'남성','여성')
rpt2pop$fem.label <- ifelse(rpt2pop$female==0,'남성','여성')
#남성과 여성을 패시팅을 이용해 별개로 그래프로 제시하는 방법
ggplot(data=rpt2long,aes(x=time,y=predy))+
  geom_point(size=1,alpha=0.1,colour='grey50')+
  geom_line(aes(y=predy,group=pid,size="응답자"),alpha=0.1,colour='grey50') +
  geom_line(data=rpt2pop,aes(y=predy,size="집단전체")) +
  scale_size_manual(name="예측선",values=c("응답자"=0.3,"집단전체"=1))+
  labs(x='시점',y='호감도',col='성별')+
  facet_grid(~fem.label)

