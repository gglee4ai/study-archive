########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제3부 다층모형 구성 및 추정############################
#02장 3층모형###########################################
#01절 군집형 시계열 데이터(clustered longitudinal data)#
########################################################
#다층모형 추정 및 사전처리를 위한 라이브러리 구동
library('lme4')
library('lmerTest')
library('tidyverse')
#3층 군집형 데이터 불러오기 
setwd("D:/data")
#3수준 데이터 
#군집형 시계열 데이터의 경우 
level3 <- read.csv("my3level_repeat.csv",header=TRUE)
head(level3)
#수준별 사례수를 살펴보자. 
length(unique(level3$pid))
length(unique(level3$gid))

#ix 변수에 대해 집단평균 중심화변환 실시
level3 <- group_by(level3,gid) %>% 
  mutate(gm.ix = ix - mean(ix))
round(level3,2)

#집단수준변수의 경우 가변수이기 때문에 비교코딩 실시
level3$cc.gx <- ifelse(level3$gx==0,-1,1)
table(level3$gx,level3$cc.gx)
#집단크기변수, 집단별 ix 평균 을 위해 집산  
temp <- group_by(level3,gid) %>% 
  summarise(gsize=length(gid),gmix=mean(ix))
temp <- mutate(temp,am.gsize=gsize-mean(gsize),
               am.gmix=gmix-mean(gmix))
#집단수준변수를 데이터에 통합
level3 <- inner_join(level3,temp,by='gid')

#이제 넓은 형태 데이터를 긴형태 데이터로 바꾸자. 
clus.rpt <- reshape(data.frame(level3),idvar='pid',
                    varying=list(3:5),v.names = "y",
                    direction='long')
round(head(clus.rpt),2)
#시간수준변수의 경우 집단평균 중심화변환 
clus.rpt <- group_by(clus.rpt,pid) %>% 
  mutate(gm.time=time-mean(time))
round(clus.rpt,2)

#시간에 따른 변화패턴: 무작위로 20명의 사례를 선정하였음 
clus.rpt.20 <- arrange(clus.rpt,pid)
length.pid <- length(unique(clus.rpt$pid))
clus.rpt.20$myselect <- rep(sample(1:length.pid,
                                   size=length.pid,replace=FALSE),each=3)
clus.rpt.20 <- filter(clus.rpt.20,myselect<21) %>% arrange(pid,time)
clus.rpt.20
ggplot(clus.rpt.20,aes(x=time,y=y))+
  geom_line(stat='identity')+
  geom_point(size=2)+
  geom_smooth(se = FALSE, method = "lm")+
  labs(x='시점',y='이직의도')+
  facet_wrap(~gx+pid)

#집단내의 개인차에 따른 변화패턴: 무작위로 20개의 집단을 선정하였음 
group.select <- data.frame(1:90,sample(1:90,size=90,replace=FALSE))
colnames(group.select) <- c('gid','myrandom')
clus.rpt.20 <- inner_join(clus.rpt,group.select,by='gid')
clus.rpt.20 <- filter(clus.rpt.20,myrandom<21)
ggplot(data=subset(clus.rpt.20,time==1),aes(y=y,x=ix))+
  geom_point(size=1.5,alpha=0.2,colour='red')+
  geom_smooth(method='lm')+
  facet_wrap(~gid)+
  labs(x='주관적 노동강도 인식',y='이직의도(t=1)')
ggplot(data=subset(clus.rpt.20,time==2),aes(y=y,x=ix))+
  geom_point(size=1.5,alpha=0.2,colour='red')+
  geom_smooth(method='lm')+
  facet_wrap(~gid)+
  labs(x='주관적 노동강도 인식',y='이직의도(t=2)')
ggplot(data=subset(clus.rpt.20,time==3),aes(y=y,x=ix))+
  geom_point(size=1.5,alpha=0.2,colour='red')+
  geom_smooth(method='lm')+
  facet_wrap(~gid)+
  labs(x='주관적 노동강도 인식',y='이직의도(t=3)')

#기술통계치
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
#시간수준
mydescriptive(clus.rpt$y)
mydescriptive(clus.rpt$time)
#개인수준
clus.rpt2 <- group_by(clus.rpt,pid) %>% 
  summarise(y=mean(y),
            ix=mean(ix),gid=mean(gid),gx=mean(gx),
            gsize=mean(gsize),am.gmix=mean(am.gmix))
mydescriptive(clus.rpt2$y)
mydescriptive(clus.rpt2$ix)
#집단수준
clus.rpt3 <- group_by(clus.rpt2,gid) %>% 
  summarise(gx=mean(gx),gsize=mean(gsize),
            am.gmix=mean(am.gmix))
mydescriptive(clus.rpt3$gx)
mydescriptive(clus.rpt3$gsize)
mydescriptive(clus.rpt3$am.gmix)

#모형0: ICC계산 
CR.model0 <- lmer(y~1+(1|pid)+(1|gid),data=clus.rpt)
summary(CR.model0)
#랜덤표과 항들 추출 
var.cov <- data.frame(VarCorr(CR.model0))
var.cov
(ICC.pid <- var.cov$vcov[1]/sum(var.cov$vcov))
(ICC.gid <- var.cov$vcov[2]/sum(var.cov$vcov))

#개인수준에서만 랜덤기울기효과 추정 
CR.model1a <- lmer(y~1+gm.time+
                    (gm.time|pid)+(1|gid),
                  data=clus.rpt)
summary(CR.model1a)

#개인수준, 집단수준에서 랜덤기울기효과 추정 
CR.model1b <- lmer(y~1+gm.time+
                    (gm.time|pid)+(gm.time|gid),
                  data=clus.rpt)
summary(CR.model1b)

#개인수준 독립변수 모형2에 투입: 주효과
CR.model2a <- lmer(y~gm.ix+gm.time+
                     (gm.time|pid)+(gm.time|gid),
                   data=clus.rpt)
summary(CR.model2a)

#개인수준 독립변수 모형2에 투입: 상호작용효과
CR.model2b <- lmer(y~gm.ix*gm.time+
                     (gm.time|pid)+(gm.time|gid),
                   data=clus.rpt)
summary(CR.model2b)

#개인수준의 독립변수 기울기에 대한 랜덤효과 추정
CR.model3 <- lmer(y~gm.ix+gm.time+
                    (gm.time|pid)+(gm.ix+gm.time|gid),
                  data=clus.rpt)
summary(CR.model3)
AIC(CR.model3);BIC(CR.model3)

#공분산을 0으로 가정해도 여전히 모형3이 제일 좋음  
CR.model3b <- lmer(y~gm.ix+gm.time+
                     (gm.time|pid)+(gm.ix+gm.time||gid),
                   data=clus.rpt)
summary(CR.model3b)
AIC(CR.model3b);BIC(CR.model3b)

#집단수준 독립변수 투입: 주효과만 
CR.model4a <- lmer(y~(cc.gx+am.gsize+am.gmix)+(gm.time+gm.ix)+
                     (gm.time|pid)+(gm.time|gid),
                   data=clus.rpt)
summary(CR.model4a)

#집단수준 독립변수 투입: gm.time과의 상호작용효과만
CR.model4b <- lmer(y~(cc.gx+am.gsize+am.gmix)*gm.time+gm.ix+
                     (gm.time|pid)+(gm.time|gid),
                   data=clus.rpt)
summary(CR.model4b)


#집단수준 독립변수 투입: gm.ix와의 상호작용효과만
CR.model4c <- lmer(y~(cc.gx+am.gsize+am.gmix)*gm.ix+gm.time+
                     (gm.time|pid)+(gm.time|gid),
                   data=clus.rpt)
summary(CR.model4c)

#집단수준 독립변수 투입: gm.time, & gm.ix와의 상호작용효과만
CR.model4d <- lmer(y~(cc.gx+am.gsize+am.gmix)*(gm.ix+gm.time)+
                     (gm.time|pid)+(gm.time|gid),
                   data=clus.rpt)
summary(CR.model4d)

##3층모형 추정결과 그래프로 나타내기 
fig.data <- group_by(clus.rpt,pid,gid) %>% 
  summarise(gm.time.min=min(gm.time),gm.time.max=max(gm.time),
            cc.gx=mean(cc.gx))
fig.data <- gather(fig.data,"value",gm.time,-gid,-pid,-cc.gx) 
fig.data <- select(fig.data,-value)
#나머지 변수들은 통제함 
fig.data$gm.ix <- 0
fig.data$am.gsize <- 0
fig.data$am.gmix <- 0
fig.data

#모형4B를 기반으로 예측값을 도출함
fig.data$predy <- predict(CR.model4b,fig.data)
fig.data$time <- 2+ fig.data$gm.time
#랜덤효과도 통제한 전체표본의 예측값 평균을 구함
fig.data.pop <- group_by(fig.data,cc.gx,gm.time) %>% 
  summarise_all(mean)

#랜덤효과와 표본전체의 패턴을 같이 제시
ggplot(data=fig.data,aes(x=time,y=predy,colour=factor(cc.gx))) +
  geom_line(aes(y=predy,group=pid,size="응답자"),alpha=0.05) +
  geom_line(data=fig.data.pop,aes(x=time,y=predy,size="집단전체")) +
  scale_size_manual(name="예측선",values=c("응답자"=0.5,"집단전체"=2))+
  scale_colour_manual(values=c('-1'='red','1'='blue'),
                      labels=c("서비스직","생산직"))+  
  labs(x='시점',y='이직의도',col='업무영역')

#가독성을 위해 업무영역을 텍스트 데이터로 변환 
fig.data$gx.label <- ifelse(fig.data$cc.gx==-1,"서비스직","생산직")
fig.data.pop$gx.label <- ifelse(fig.data.pop$cc.gx==-1,"서비스직","생산직")
fig.data$gx.label <- factor(fig.data$gx.label,
                            levels=c("서비스직","생산직"))
fig.data.pop$gx.label <- factor(fig.data.pop$gx.label,
                                levels=c("서비스직","생산직"))
#랜덤효과와 표본전체의 패턴을 같이 제시
ggplot(data=fig.data,aes(x=time,y=predy)) +
  geom_line(aes(y=predy,group=pid,size="응답자"),alpha=0.05) +
  geom_line(data=fig.data.pop,aes(x=time,y=predy,size="집단전체")) +
  scale_size_manual(name="예측선",values=c("응답자"=0.5,"집단전체"=2))+
  labs(x='시점',y='이직의도')+
  facet_grid(~gx.label)
