#######################################################
#R를 이용한 다층모형 분석##############################
#######################################################

#######################################################
#제1부 제1부 일반선형모형과 다층모형###################
#02장 위계적 데이터 분석시 일반선형모형 사용의 문제점##
#######################################################

#다층모형, 왜 필요한가?
setwd("D:/data")
mydata <- read.csv("primer.csv",header=TRUE)
head(mydata)
#Plotting
plot(mydata$x,mydata$y,ylab='평량평균',xlab='학습시간',
     xlim=c(0.5,5.5),ylim=c(3,4.3),type='n')
points(mydata$x[mydata$gid==1],mydata$y[mydata$gid==1],type='p',pch=19)
points(mydata$x[mydata$gid==2],mydata$y[mydata$gid==2],type='p',pch=17)
points(mydata$x[mydata$gid==3],mydata$y[mydata$gid==3],type='p',pch=15)
points(mydata$x[mydata$gid==4],mydata$y[mydata$gid==4],type='p',pch=13)
points(mydata$x[mydata$gid==5],mydata$y[mydata$gid==5],type='p',pch=11)
legend('topright',legend=paste('gid = ',1:5,sep=''),
       bty="n",pch=c(19,17,15,13,11))
#다층데이터에 일반선형모형을 적용시킬 경우 어떤 문제가 발생하는가? 
summary(lm(y~x,mydata))
#집단별로 구분하는 것도 하나의 방법이지만 검증력에 문제가 발생함. 
for (i in 1:5){
  print(paste("when gid is ",i,sep=''))
  print(summary(lm(y~x,subset(mydata,gid==i))))
}

#위계적 데이터는 다층모형을 사용
library('lme4')
summary(lmer(y ~ x+(1|gid),mydata))

#######################################################
#제1부 제1부 일반선형모형과 다층모형###################
#03장 다층 모형 이해를 위한 개념들#####################
#######################################################

#군집형 데이터: 집단에 개인이 배속됨
clus2 <- read.csv("my2level_cluster.csv",header=TRUE)
head(clus2)
#개인의 수와 집단의 수를 구하면 다음과 같다. 
dim(clus2)
length(unique(clus2$gid))
#각 집단에 몇 명의 개인들이 배속되어 있는지 살펴보자. 
ind_per_grp <- aggregate(y ~ gid, clus2, length)
table(ind_per_grp$y)

#시계열 데이터: 동일 사례에서 반복적으로 데이터를 얻음
rpt2 <- read.csv("my2level_repeat.csv",header=TRUE)
head(rpt2)
dim(rpt2)
#넓은 형태 -> 긴 형태 데이터로
rpt2long <- reshape(rpt2,,idvar='pid',varying=list(3:7),
                    v.names = "y",direction='long')
#개인 식별번호 순서로 정렬
rpt2long <- rpt2long[order(rpt2long$pid),]
dim(rpt2long)
length(unique(rpt2long$pid))
head(rpt2long)

##Footnote: 해당부분은 제2부의 본문에서도 찾을 수 있다. 
#타이디데이터 접근 
library('tidyverse')
#넓은형태 데이터 -> 긴형태 데이터 
#만약 긴형태 데이터를 넓은형태로 바꾸려면 spread() 함수 이용
rpt2long <- gather(rpt2,time,y,-pid,-female)
#time 변수를 수치형으로 바꾸는 과정
library('stringr')
#숫자앞의 y를 삭제
rpt2long$time <- str_replace(rpt2long$time,'y','')
#문자형 데이터를 수치형 데이터로 전환 
rpt2long$time <- as.numeric(rpt2long$time)
summary(rpt2long)

#랜덤효과의 이해 
mydata <- read.csv("primer.csv",header=TRUE)
int_slope <- data.frame(matrix(NA,nrow=5,ncol=3))
for (i in 1:5){
  int_slope[i,1] <- i
  int_slope[i,2:3] <- lm(y~x,subset(mydata,gid==i))$coef
}
colnames(int_slope) <- c('gid','intercept','slope')
int_slope
var(int_slope$intercept)
var(int_slope$slope)

#평균중심화 변환의 이해 
mydata <- read.csv("mean_centering.csv",header=TRUE)
round(cor(mydata),3)

#평균중심화 변환을 실시
mydata$mc.x1 <- mydata$x1 - mean(mydata$x1)
mydata$mc.x2 <- mydata$x2 - mean(mydata$x2)

#y와 x1의 원점수, 평균중심화 변환이 적용된 x1의 관계
par(mfrow=c(2,1))
plot(mydata$x1,mydata$y,xlim=c(-0.7,1.2),ylim=c(-0.2,1.2),
     pch=19,col='lightblue',ylab='Y',xlab='x, 원점수',
     main='원점수를 그대로 사용한 경우')
#절편값을 덧붙임
abline(h=lm(y~x1,mydata)$coef[1],lty=2)
#x1 원점수의 평균을 덧붙임
abline(v=mean(mydata$x1),lty=2)
#두 변수의 OLS회귀예측선을 덧붙임
abline(lm(y~x1,mydata),lty=1)
plot(mydata$mc.x1,mydata$y,xlim=c(-0.7,1.2),ylim=c(-0.2,1.2),
     pch=19,col='lightblue',ylab='Y',xlab='x, 평균중심화 변환점수',
     main='평균중심화 변환을 적용한 경우')
abline(h=lm(y~mc.x1,mydata)$coef[1],lty=2)
abline(v=mean(mydata$mc.x1),lty=2)
abline(lm(y~mc.x1,mydata),lty=1)
#OLS 회귀모형
summary(lm(y~x1,mydata))
summary(lm(y~mc.x1,mydata))
mean(mydata$y)

library('car') #vif 계산을 위해 
m1.raw <- lm(y~x1+x2,mydata)
summary(m1.raw)
vif(m1.raw)
m2.raw <- lm(y~x1*x2,mydata)
summary(m2.raw)
vif(m2.raw)

m1.mc <- lm(y~mc.x1+mc.x2,mydata)
summary(m1.mc)
vif(m1.mc)
m2.mc <- lm(y~mc.x1*mc.x2,mydata)
summary(m2.mc)
vif(m2.mc)

#문제1
temp <- mydata[1:2,]
#x2가 0인 경우 
temp$x2 <- c(0,0) 
#x1이 0에서 1로 변함
temp$x1 <- c(0,1) 
predict(m2.raw,temp)
#문제2
temp <- mydata[1:2,]
#x2가 x2의 평균인 경우 
temp$x2 <- c(mean(mydata$x2),mean(mydata$x2)) 
#x1이 0에서 1로 변함
temp$x1 <- c(0,1) 
predict(m2.raw,temp)

#문제1
temp <- mydata[1:2,]
#x2가 0인 경우 
temp$x2 <- c(0,0) 
temp$mc.x2 <- temp$x2 - mean(mydata$x2)
#x1이 0에서 1로 변함
temp$x1 <- c(0,1) 
temp$mc.x1 <- temp$x1 - mean(mydata$x1)
predict(m2.mc,temp)
#문제2
temp <- mydata[1:2,]
#x2가 x2의 평균인 경우 
temp$x2 <- c(mean(mydata$x2),mean(mydata$x2)) 
temp$mc.x2 <- temp$x2 - mean(mydata$x2)
#x1이 0에서 1로 변함
temp$x1 <- c(0,1) 
temp$mc.x1 <- temp$x1 - mean(mydata$x1)
predict(m2.mc,temp)

#평균 중심화변환의 두 가지 방법
mydata <- read.csv("primer.csv",header=TRUE)
#전체평균(grand-mean) 중심화변환  
mydata$am.x <- mydata$x - mean(mydata$x)
#집단평균(group-mean) 중심화변환  
#먼저 집단별 x변수의 평균을 구한다
groupmean <- aggregate(x~gid,mydata,mean)
colnames(groupmean)[2] <- 'groupmean.x'
mydata <- merge(mydata,groupmean,by='gid')
mydata$gm.x <- mydata$x - mydata$groupmean.x
head(mydata,11)
#세 종류의 독립변수들의 상관계수를 구해보자. 
round(cor(mydata[,c('x','am.x','gm.x')]),3)
#집단별로 구분한 후 세 변수의 상관계수를 구해보자. 
for (i in 1:5){
  print(paste("when gid is ",i,sep=''))
  print(round(cor(mydata[mydata$gid==i,c('x','am.x','gm.x')]),3))
}

#Plotting: 4개의 패널에 3개의 그림을 배치하고, 마지막 패널에는 범례를 붙임
par(mfrow=c(2,2))
#원점수를 사용한 경우 
plot(mydata$x,mydata$y,ylab='Y',xlab='x, 원점수',
     ,xlim=c(0.5,5.5),ylim=c(3,4.2),type='n',
     main='원점수를 그대로 사용한 경우')
for (i in 1:5){
  points(mydata$x[mydata$gid==i],mydata$y[mydata$gid==i],
         type='p',pch=2*i)
}
#전체평균 중심화 변환을 사용한 경우 
plot(mydata$am.x,mydata$y,ylab='Y',xlab='x, 전체평균 중심화 변환',
     ,xlim=c(-2.5,2.5),ylim=c(3,4.2),type='n',
     main='전체평균(grand-mean) 중심화 변환을 사용한 경우')
for (i in 1:5){
  points(mydata$am.x[mydata$gid==i],mydata$y[mydata$gid==i],
         type='p',pch=2*i)
}
#집단평균 중심화 변환을 사용한 경우 
plot(mydata$gm.x,mydata$y,ylab='Y',xlab='x, 집단평균 중심화 변환',
     ,xlim=c(-.05,0.12),ylim=c(3,4.2),type='n',
     main='집단평균(group-mean) 중심화 변환을 사용한 경우')
for (i in 1:5){
  points(mydata$gm.x[mydata$gid==i],mydata$y[mydata$gid==i],
         type='p',pch=2*i)
}
plot(mydata$x,mydata$y,type='n',axes=F,xlab='',ylab='')
legend('top',legend=paste('gid = ',1:5,sep=''),
       bty="n",pch=2*(1:5))
