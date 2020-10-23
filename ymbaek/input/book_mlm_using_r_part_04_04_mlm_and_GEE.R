########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제4부 제4부 기타 고급통계기법과 다층모형###############
#04장 일반화추정방정식(GEE) ############################
########################################################

# 라이브러리 구동 
library('lme4')
library('lmerTest')
library('tidyverse')
# 정규분포인 경우 
setwd("D:/data")
clus <- read.csv("my2level_cluster.csv",header=T)
# 데이터 사전처리 
clus <- group_by(clus,gid) %>% 
  mutate(gm.ix1=ix1-mean(ix1),gm.ix2=ix2-mean(ix2),
         gmix1=mean(ix1),gmix2=mean(ix2),gsize=length(y))
temp <- group_by(clus,gid) %>% 
  summarise(gx1=mean(gx1),gsize=mean(gsize),
            gmix1=mean(gmix1),gmix2=mean(gmix2))
temp <- mutate(temp,am.gx1=gx1-mean(gx1),
               am.gsize=gsize-mean(gsize),
               am.gmix1=gmix1-mean(gmix1),
               am.gmix2=gmix2-mean(gmix2))
temp <- select(temp,gid,am.gx1,am.gsize,am.gmix1,am.gmix2)
clus2 <- inner_join(clus,temp,by='gid')
# MLM 추정 
mlm.identity <- lmer(y ~ am.gx1*(gm.ix1+gm.ix2)+(1|gid),
                     data=clus2,REML = FALSE)
summary(mlm.identity)$coefficients
# GEE 추정: independence 옵션  
library('gee')
gee.norm <- gee(y ~ am.gx1*(gm.ix1+gm.ix2),id=gid,data=clus2,
                 family=gaussian(link='identity'),
                 corstr='independence')
summary(gee.norm) #Correlation 자료가 매우 많이 나옴
summary(gee.norm)$coefficients

mysummary <- cbind(summary(mlm.identity)$coefficients[,c(1,4)],
summary(gee.norm)$coefficients[,c(1,3)],
summary(gee.norm)$coefficients[,c(1,5)])
round(mysummary,3)

# 이항분포인 이분변수가 종속변수인 경우 
L2.dich <- read.csv("my2level_dich.csv",header=T)
head(L2.dich)
# 데이터의 사전처리 
L2.dich <- group_by(L2.dich,gid) %>% 
  mutate(gm.ix1=ix1-mean(ix1),gm.ix2=ix2-mean(ix2),
         gmix1=mean(ix1),gmix2=mean(ix2),gsize=length(y))
temp <- group_by(L2.dich,gid) %>% 
  summarise(gx=mean(gx),gsize=mean(gsize),
            gmix1=mean(gmix1),gmix2=mean(gmix2))
temp <- mutate(temp,am.gx=gx-mean(gx),
               am.gsize=gsize-mean(gsize),
               am.gmix1=gmix1-mean(gmix1),
               am.gmix2=gmix2-mean(gmix2))
temp <- select(temp,gid,am.gx,am.gsize,am.gmix1,am.gmix2)
L2.dich <- inner_join(L2.dich,temp,by='gid')
# GEE 추정 
gee.logit <- gee(y ~ am.gx*(gm.ix1+gm.ix2),id=gid,
                 data=L2.dich,
                 family=binomial(link='logit'),
                 corstr='independence')
summary(gee.logit)$coefficients
# MLM 추정
mlm.logit <- glmer(y ~ am.gx*(gm.ix1+gm.ix2)+(1|gid),
                      data=L2.dich,
                   family=binomial(link='logit'))
summary(mlm.logit)$coefficients


