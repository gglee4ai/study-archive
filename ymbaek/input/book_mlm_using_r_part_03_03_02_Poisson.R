########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제3부 다층모형 구성 및 추정############################
#03장 종속변수가 비정규분포인 경우의 다층모형###########
#02절 02절 종속변수가 포아송분포의 변수인 경우 #########
########################################################
library('lme4')
library('lmerTest')
library('tidyverse')
setwd("D:/data")
L2.pois <- read.csv("my2level_pois.csv",header=T)
head(L2.pois)

#사례수 계산
dim(L2.pois)

#개인수준 독립변수에 대한 비교코딩 리코딩
L2.pois$cc.ix <- ifelse(L2.pois$ix==0,-1,1)
#넓은 형태 -> 긴형태 
pois.long <- reshape(L2.pois,idvar='pid',varying=list(2:4),
                     v.names = "y",direction='long')

#시간수준 독립변수에 대한 집단평균 중심화변환 
pois.long <- group_by(pois.long,pid) %>% 
  mutate(gm.time=time-mean(time))

group_by(pois.long,time) %>% 
  summarise(length(y),mean(y),sd(y),min(y),max(y))
#기술통계치
summarise(ungroup(pois.long),
          length(y),mean(y),sd(y),min(y),max(y))
summarise(ungroup(pois.long),
          length(time),mean(time),sd(time),min(time),max(time))
group_by(pois.long,pid) %>% 
  summarise(ix=mean(ix)) %>% 
  summarise(length(ix),mean(ix),sd(ix),min(ix),max(ix))


#기본모형
gmlm.pois.m0 <- glmer(y~1+(1|pid),
                      data=pois.long,
                      family=poisson(link="log"))
summary(gmlm.pois.m0)

myICC2.pois <- function(gmlm.poisson){
  var.cov <- data.frame(VarCorr(gmlm.poisson))
  icc2.pois <- var.cov$vcov/(var.cov$vcov+1)
  icc2.pois
}
myICC2.pois(gmlm.pois.m0)

#시간수준 변수의 고정효과 투입
gmlm.pois.m1 <- glmer(y~gm.time+(gm.time|pid),
                      data=pois.long,
                      family=poisson(link="log"))
summary(gmlm.pois.m1)

#개인수준의 독립변수 투입 
gmlm.pois.m2a <- glmer(y~cc.ix+gm.time+(gm.time|pid),
                       data=pois.long,
                       family=poisson(link="log"))
summary(gmlm.pois.m2a)

gmlm.pois.m2b <- glmer(y~cc.ix*gm.time+(gm.time|pid),
                       data=pois.long,
                       family=poisson(link="log"))
summary(gmlm.pois.m2b)

m1.var <- data.frame(VarCorr(gmlm.pois.m1))$vcov
m2b.var <- data.frame(VarCorr(gmlm.pois.m2b))$vcov
(m1.var - m2b.var)/m1.var


#포아송 분포의 종속변수에 대한 다층모형 추정결과 그래프
fig.data <- group_by(pois.long,pid,gm.time) %>% 
  summarise(time=mean(time),cc.ix=mean(cc.ix),ix=mean(ix))
#전체 데이터를 대상으로 예측값을 저장
fig.data$predy <- predict(gmlm.pois.m2b,fig.data,type='response')

#그래프 가독성을 위해 텍스트형 변수생성 
fig.data$ix.label <- ifelse(fig.data$ix==0,'내성적','외향적')

#개인 응답자의 외향성 수준에 따른 변화패턴 
ggplot(data=fig.data,aes(x=time,y=predy)) +
  geom_point(size=1,alpha=0.2) +  
  geom_line(aes(y=predy,group=pid),alpha=0.2) +
  labs(x='시점',y='소속 커뮤니티 개수')+
  facet_grid(~ix.label)

#전체표본의 평균변화도 같이 제시  
fig.data.pop <- group_by(fig.data,ix,gm.time) %>% 
  summarise(time=mean(time),predy=mean(predy),cc.ix=mean(cc.ix))
fig.data.pop$ix.label <- ifelse(fig.data.pop$ix==0,'내성적','외향적')
ggplot(data=fig.data,aes(x=time,y=predy)) +
  geom_line(aes(y=predy,group=pid,size="응답자"),alpha=0.2) +
  geom_line(data=fig.data.pop,aes(y=predy,size="집단전체")) +
  scale_size_manual(name="예측선",values=c("응답자"=0.3,"집단전체"=2))+
  labs(x='시점',y='소속 커뮤니티 개수')+
  facet_grid(~ix.label)
