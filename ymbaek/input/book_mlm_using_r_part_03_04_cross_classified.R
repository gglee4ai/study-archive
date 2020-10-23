########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제3부 다층모형 구성 및 추정############################
#04장 교차 다층모형 ####################################
########################################################
#라이브러리 구동 및 데이터 불러오기 
library('lme4')
library('lmerTest')
library('tidyverse')
setwd("D:/data")
mycross <- read.csv("crosslevel.csv",header=T)
head(mycross)
dim(mycross)

length(unique(mycross$g1id))
length(unique(mycross$g2id))

table(mycross$g1id,mycross$g2id)[1:5,]

#기술통계치 
#개인수준
summarise(ungroup(mycross),
          length(ix),mean(ix),sd(ix),min(ix),max(ix))
summarise(ungroup(mycross),
          length(y),mean(y),sd(y),min(y),max(y))
#각 집단별 수준
group_by(mycross,g1id) %>% 
  summarise(g1x=mean(g1x)) %>% 
  summarise(length(g1x),mean(g1x),sd(g1x),min(g1x),max(g1x))
group_by(mycross,g2id) %>% 
  summarise(g2x=mean(g2x)) %>% 
  summarise(length(g2x),mean(g2x),sd(g2x),min(g2x),max(g2x))

#기본모형 
cross.m0 <- lmer(y~1+(1|g1id)+(1|g2id)+(1|g1id:g2id),
                 data=mycross)
summary(cross.m0)

iccs <- data.frame(VarCorr(cross.m0))$vcov
icc_rc <- iccs[1]/sum(iccs)
icc_r <- iccs[2]/sum(iccs)
icc_c <- iccs[3]/sum(iccs)
icc_r; icc_c; icc_rc

#랜덤효과항
cross.m1a <- lmer(y~ix+(1|g1id)+(1|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m1a)

cross.m1b <- lmer(y~ix+(ix|g1id)+(1|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m1b)

cross.m1c <- lmer(y~ix+(1|g1id)+(ix|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m1c)

cross.m1d <- lmer(y~ix+(ix|g1id)+(ix|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m1d)

#집단수준의 독립변수 투입
cross.m2a <- lmer(y~ix+g1x+g2x+(1|g1id)+(1|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m2a)

cross.m2b <- lmer(y~ix*g1x+g2x+(1|g1id)+(1|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m2b)

cross.m2c <- lmer(y~g1x+ix*g2x+(1|g1id)+(1|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m2c)

cross.m2d <- lmer(y~ix*(g1x+g2x)+(1|g1id)+(1|g2id)+(1|g1id:g2id),
                  data=mycross)
summary(cross.m2d)

#그래프
fig.data <- group_by(mycross,g2id,g1id) %>%
  summarise(x.min=min(ix),x.max=max(ix),g1x=mean(g1x),g2x=mean(g2x))
fig.data <- gather(fig.data,"value",ix,
                   -g1id,-g2id,-g1x,-g2x)
fig.data$predy <- predict(cross.m2d,fig.data)
fig.data$g12id <- 100*fig.data$g1id + fig.data$g2id 
#전체표본의 패턴
temp1 <- group_by(subset(fig.data,value=='x.min'),g1x,g2x) %>% 
  summarise(predy=mean(predy),ix=min(ix))
temp2 <- group_by(subset(fig.data,value=='x.max'),g1x,g2x) %>% 
  summarise(predy=mean(predy),ix=max(ix))
fig.data.pop <- rbind(temp1,temp2)
#그래프 가독성을 위해 텍스트형식의 변수생성
fig.data$g1x.label <- ifelse(fig.data$g1x==0,"공기업","사기업")
fig.data$g2x.label <- ifelse(fig.data$g2x==0,"외곽","도심")
fig.data$g1x.label <- factor(fig.data$g1x.label,levels=c("공기업","사기업"))
fig.data$g2x.label <- factor(fig.data$g2x.label,levels=c("외곽","도심"))
fig.data.pop$g1x.label <- ifelse(fig.data.pop$g1x==0,"공기업","사기업")
fig.data.pop$g2x.label <- ifelse(fig.data.pop$g2x==0,"외곽","도심")
fig.data.pop$g1x.label <- factor(fig.data.pop$g1x.label,
                                 levels=c("공기업","사기업"))
fig.data.pop$g2x.label <- factor(fig.data.pop$g2x.label,
                                 levels=c("외곽","도심"))

#개별패턴과 전체패턴을 같이 그리는 경우 
ggplot(fig.data,aes(y=predy,x=ix))+
  geom_line(aes(y=predy,group=g12id),alpha=.1)+
  geom_line(data=fig.data.pop,aes(y=predy),linetype=1,col='red') +
  labs(x='노동강도 인식',y='이직의도')+
  facet_grid(g2x.label~g1x.label)

#전체패턴만 그리는 경우 
ggplot(fig.data.pop,aes(y=predy,x=ix))+
  geom_line(size=1)+
  labs(x='노동강도 인식',y='이직의도')+
  facet_grid(g2x.label~g1x.label)

