#######################################################
#R를 이용한 다층모형 분석##############################
#######################################################

#######################################################
#제2부 다층모형 추정을 위한 데이터 사전처리############
#01장 군집화 데이터의 사전처리#########################
#######################################################
#데이터 불러오기 
setwd("D:/data")
clus21 <- read.csv("my2level_L1.csv",header=TRUE)
head(clus21)
summary(clus21)
clus22 <- read.csv("my2level_L2.csv",header=TRUE)
head(clus22)
summary(clus22)

#gid 변수를 중심으로 clus21, clus22 데이터 합치기
clus2 <- merge(clus21,clus22,by='gid') 
head(clus2)
summary(clus2)

#상위수준 데이터로 다시 집산 
temp <- aggregate(gx1~gid,clus2,mean)
#아래에서 확인할 수 있듯 clus22와 동일데이터다 
all(temp == clus22)

#타이디데이터 접근, inner_join() 함수 사용 
library('tidyverse')
clus2 <- inner_join(clus21,clus22,by='gid')
head(clus2)
summary(clus2)
#다음과 같이 group_by()와 summarise()를 동시에 사용하면 됨
temp = group_by(clus2, gid) %>% summarise(gx1 = mean(gx1))
head(temp)
all((temp) == clus22)

#######################################################
#제2부 다층모형 추정을 위한 데이터 사전처리############
#02장 시계열 데이터의 사전처리#########################
#######################################################

#시계열 데이터: 넓은 형태 데이터 
rpt2 <- read.csv("my2level_repeat.csv",header=TRUE)
#넓은 형태 -> 긴 형태 데이터로
rpt2long <- reshape(rpt2,,idvar='pid',varying=list(3:7),
                    v.names = "y",direction='long')

#타이디데이터 접근 
library('tidyverse')
#넓은형태 데이터 -> 긴형태 데이터 
#만약 긴형태 데이터를 넓은형태로 바꾸려면 spread() 함수 이용
rpt2long <- gather(rpt2,time,y,-pid,-female)
head(rpt2long)
#time 변수를 수치형으로 바꾸는 과정
library('stringr')
#숫자앞의 y를 삭제
rpt2long$time <- str_replace(rpt2long$time,'y','')
is.numeric(rpt2long$time)
#문자형 데이터를 수치형 데이터로 전환 
rpt2long$time <- as.numeric(rpt2long$time)
is.numeric(rpt2long$time)
summary(rpt2long)

#######################################################
#제2부 다층모형 추정을 위한 데이터 사전처리############
#03장 집단평균 중심화 변환#############################
#######################################################
#R베이스 함수이용
#clus2 데이터: 1단계 
clus2 <- read.csv("my2level_cluster.csv",header=TRUE)
groupmean <- aggregate(ix1~gid,clus2,mean)
colnames(groupmean)[2] <- 'groupmean.ix1'
#2단계 
clus2 <- merge(clus2,groupmean,by='gid')
#3단계
clus2$gm.ix1 <- clus2$ix1 - clus2$groupmean.ix1

#rpt2long 데이터: 1단계 
rpt2 <- read.csv("my2level_repeat.csv",header=TRUE)
rpt2long <- reshape(rpt2,,idvar='pid',varying=list(3:7),
                    v.names = "y",direction='long')
groupmean <- aggregate(time~pid,rpt2long,mean)
colnames(groupmean)[2] <- 'groupmean.time'
#2단계 
rpt2long <- merge(rpt2long,groupmean,by='pid')
#3단계
rpt2long$gm.time <- rpt2long$time - rpt2long$groupmean.time

#타이디데이터 접근법
#rpt2long 데이터
library('tidyverse')
rpt2 <- read.csv("my2level_repeat.csv",header=TRUE)
rpt2long <- reshape(rpt2,,idvar='pid',varying=list(3:7),
                    v.names = "y",direction='long')
rpt2long  <- group_by(rpt2long,pid) %>% 
  mutate(gm.time = time-mean(time)) %>%
  arrange(pid)
rpt2long  

#clus2 데이터
clus2 <- read.csv("my2level_cluster.csv",header=TRUE)
clus2 <- group_by(clus2,gid) %>% 
  mutate(gm.ix1 = ix1-mean(ix1),gm.ix2 = ix2-mean(ix2)) %>%
  arrange(gid)
clus2

#집단별 독립변수의 평균을 추가로 저장
clus2 <- mutate(clus2, gmix1 = mean(ix1),gmix2 = mean(ix2))
#집단크기 변수를 추가로 저장
clus2 <- mutate(clus2, group.size=length(y))
#집단수준 변수만을 추출하여 기술통계치를 구하려면 다음과 같이 
clus22 <- summarise(clus2,mean(gx1),mean(group.size))
dim(clus22)
summary(clus22)
