# ::  머신러닝 실전 - R을 활용한 예측과 이상패턴 탐지
# ============================
#   
#   ......... 전용준. 리비젼컨설팅 대표. 경영학박사
# ......... xyxonxyxon@empal.com
# 
# 
# >> 개요 :  기업에서 머신러닝을 적용하는 과정을 실전적으로 짚어본다. 
# R을 사용해서 
# 데이터준비 및 탐색적 분석을 거쳐, 
# 앙상블 머신러닝 기반의 예측 모델을 만들고 그 결과를 해석하고 평가하는 
# 실전 프로세스를 직접 참여하여 밟아본다. 
# 비즈니스 실전 주제의 데이터를 사용해 미래행동예측과 
# 비정상(anomaly) 의심패턴을 발견하는 과정을 직접 "체험" 해 본다.
# 
# Take aways:
#   -머신러닝의 작동 원리와 특성 이해
# -머신러닝을 기업 실전 업무에 적용 시 고려해야 할 사항
# 
# Target Audience
# - 머신러닝 작동의 실체를 이해하고자 하는 비즈니스 현업 실무자 및 관리자
# - 머신러닝 적용을 준비하거나 기획하려는 IT Staff
# 

# 실습 진행 순서

#---[A] 예측모델 개발 (using Random Forests) ---------------------
#-----[A1] 분석용 데이터 (모바일 Game 이용이력) 로딩 
#-----[A2] 탐색적 분석 EDA 및 고객 행동 데이터 집계 
#-----[A3] 기본 변수 데이터셋 생성과 탐색적분석 EDA 
#-----[A4] 추가 파생변수 생성 및 데이터 정제 
#-----[A5] 예비적 모델 개발 
#-----[A6] 데이터 분할과 모델 평가 
#-----[A7] 추가적 파생변수 생성  

#---[B] 이상패턴 탐지 (anomaly detection) ---------------------
#-----[B1] data set 로딩 
#-----[B2] data partitioning (과거 Vs. 미래) 
#-----[B3] 기본적인 집계변수 생성 
#-----[B4] Anomaly Detection 모델 생성 
#-----[B5] Anomaly Score 의미 이해 
#-----[B6] 변수 설계 재검토 
#-----[B7] 로그변환한 변수를 사용한 AD 모델 생성 
#-----[B8] 추가적인 파생변수들 생성 후 재분석 



# 설치 필요한 라이브러리 리스트
# install.packages("party")
# install.packages("randomForest")
# install.packages("ROCR")
# install.packages("IsolationForest", repos="http://R-Forge.R-project.org")
# 


#---[A] 예측모델 개발 (unsing Random Forests) ---------------------
#--- Random Forests for Predictive Modleing 


#-----[A1] 분석용 데이터 (모바일 Game 이용이력) 로딩 --------

# 데이터 불러오기
# dau <- read.csv("daub.csv")
# user <- read.csv("userb.csv")

# [ 분석 데이터 : Game Log 데이터 ] - 파일 온라인링크
#  :: http://blog.daum.net/revisioncrm/389

# 사용자 기본 정보 데이터
userb <-read.csv("http://cfile240.uf.daum.net/attach/996FE63359E42A3D1F20F7")
# 로그인 이력 데이터 
daub <- read.csv("http://cfile233.uf.daum.net/attach/9910C03359E42A3526C6C9")


#-----[A2] 탐색적 분석 EDA 및 고객 행동 데이터 집계 --------

#---- 사용자 기본정보 데이터 구조 파악 EDA -------

str(userb)
nrow(userb)

plot(userb$gender)
table(userb$gender)
table(userb$gender, userb$generation)
plot(table(userb$gender, userb$generation))

# 막대그래프 stacked
barplot(table(userb$gender, userb$generation))
# beside
barplot(table(userb$gender, userb$generation), beside=T)

# 디바이스 유형별 분포
plot(userb$device_type)

# 일별 설치건수 추이
daily_inst <- as.data.frame(table(as.character(userb$install_date)))
names(daily_inst) <- c("date", "Freq")
head(daily_inst)
plot(daily_inst$Freq, type="l", main="Install Freq by date")


# EDA(Sample): 성별로 비교해서 인스톨 건수를 플롯으로 비교?

daily_inst_gend <- as.data.frame(table(as.character(userb$install_date), userb$gender))
names(daily_inst_gend) <- c("date", "gender", "Freq")

plot(daily_inst_gend[daily_inst_gend$gender=="F",]$Freq, type="l", col="red",
     main="installation count over time by gender [red: Female]")
lines(daily_inst_gend[daily_inst_gend$gender=="M",]$Freq, type="l", col="blue")

# 성별 디바이스 유형(OS)별 분포
barplot(table(userb$gender, userb$device_type), beside=T, 
        legend.text=T)


#---- dau (이용이력) 데이터 특성/구조 파악 ----

str(daub)
nrow(daub)
length(unique(daub$log_date))

min(as.character(daub$log_date))
max(as.character(daub$log_date))
range(as.character(daub$log_date))

# 일자별 건수 집계 - 이후 집계변수 생성을 위해 재사용
tblog_date <- as.data.frame(table(as.character(daub$log_date)))
names(tblog_date) <- c("log_date", "Freq")
head(tblog_date)

# 일자순의 건수 분포
plot(tblog_date$Freq, type="b")
# 건수 크기 순의 건수 분포
plot(sort(tblog_date$Freq), type="l", ylim=c(0, max(tblog_date$Freq)))

# x축 라벨 변경 ( --> 일자 Date)
# y축의 기준점을 0 부터로 변경
plot(tblog_date$Freq,  xaxt='n', type="b", main="일자별 로그인 건수", 
     ylim=c(0, max(tblog_date$Freq)*1.1), xlab="Date", cex=0.6)
axis(1, at=1:nrow(tblog_date), labels=tblog_date$log_date)

# app_name 컬럼 값 구성분포 확인
length(unique(daub$app_name))
unique(daub$app_name)
table(daub$app_name)
# 모두 같은 이름이기 (= game-01 만 존재) 때문에 의미 없는 컬럼

# user_id 컬럼 확인
length(unique(as.character(daub$user_id)))
min(daub$user_id)
max(daub$user_id) 
# 건너 뛰고 부여된 번호들이 존재

tbuser_id <- as.data.frame(table(as.character(daub$user_id)))
names(tbuser_id) <- c("user_id", "Freq")
rbind(head(tbuser_id), tail(tbuser_id))

plot(tbuser_id[order(as.numeric(tbuser_id$user_id)), ]$Freq, type="l", main="Freq by user_id")
# Alternatively
plot(as.numeric(tbuser_id$user_id), tbuser_id$Freq, type="l")

# user_id가 순서대로 부여된 영향이 Freq에 나타남
# = 가입기간이 짧으면 누적방문건수가 많기 어려움

plot(sort(tbuser_id$Freq), type="l", ylim=c(0, max(tbuser_id$Freq)))

user_date <- as.data.frame(table(daub$user_id, daub$log_date))
# 동일한 일자가 두 번이상 로그인 날짜로 기록된 건이 있는지 확인
nrow(head(user_date[user_date$Freq>1,]))



#-----[A3] 기본 변수 데이터셋 생성과 탐색적분석 EDA --------------------
#---- 예측분석을 위한 테이블 생성 ----


# 참고 :: 예측분석 수행절차(예시)
# ....................
# 1. 무엇을 예측할 것인지 정의 (다음분기 해지가능성)
# 1.1 기간 설정 (언제 기간의 데이터로 언제를 예측?)
# 2. 대상의 범위 설정 (예: 휴면상태가 아닌 개인 고객 중 모바일 채널 이용 경험 고객)
# 3. 필요한 데이터 확보
# 4. 확보한 데이터에서 대상자 추출(예: 대상자 학번)
# 5. 변수(X)를 만들 데이터 분리
# 6. 타겟(Y)을 만들 데이터 분리

# (중간중간) 탐색적으로 데이터 구조, 분포, 관계 파악
#  === 탐색적 데이터 분석 (EDA)

# 7. 변수 설계 (아마도 여러 개. 100개이상???)
# 8. 변수 생성 (하나씩 하나씩. NA처리, 정제 포함)
# 9. 생성된 변수와 대상자 리스트 결합
# 10. 타겟 생성
# 11. 타겟을 생성된 변수들의 Set와 결합

# 12. 모델 개발 (변수 선택, 추가변수 반영 포함)
# 13. 모델 평가 Assessment (잘맞는 것인지)
#-------------


# 일자별 건수 확인
head(tblog_date[order(tblog_date$log_date),], 10)
tail(tblog_date[order(tblog_date$log_date),], 10)

# 분석방향 결정: 미래에 게임을 할 것인지 예측하는 모델을 생성
# 최종 7일 즉 1주일간의 데이터는 분리 --> 예측 대상(Y)으로 사용 

# 최종 7일을 제외한 일자 리스트 추출
ulog0 <- head(tblog_date[order(tblog_date$log_date),], nrow(tblog_date)-7)

ulog0$Freq <- 0 # 기간이 달라 의미없으므로 임시 생략 조치
str(daub)
# X 컬럼은 필요 없음
daub <- daub[,2:4]
str(daub)

# 로그인 이력에서 최종 7일을 제외한 일자 해당 내역만 추출
daup <- daub[daub$log_date %in% ulog0$log_date, c(1,3)]

uuserp <- as.data.frame(unique(daup$user_id))
# 컬럼 하나 뿐인 string vector 이지만 df로
# 나머지 컬럼들을 생성하는대로 여기에 결합할 것이기 때문
names(uuserp)[1] <- "user_id"

# user별로 이용 일수를 집계
user_days <- aggregate(log_date ~ user_id, data = daup, length)
head(user_days)
names(user_days)[2] <- "log_days"
plot(user_days$user_id, user_days$log_days)
# 의미는? 최근 가입한 사용자가 누적 로그인 일수가 더 크다?


# 최초, 최근 이용일 날짜를 산출
user_initial <- aggregate(log_date ~ user_id, data = daup, function(x) min(as.character(x)))
head(user_initial)
names(user_initial)[2] <- "log_init"

user_recent <- aggregate(log_date ~ user_id, data = daup, function(x) max(as.character(x)))
names(user_recent)[2] <- "log_rcnt"

# 하나의 데이터 프레임으로 결합
uuserp1 <- merge(uuserp, user_days, by="user_id", all.x=T)
uuserp1 <- merge(uuserp1, user_initial, by="user_id", all.x=T)
uuserp1 <- merge(uuserp1, user_recent, by="user_id", all.x=T)
head(uuserp1)

# initial과 recent에 대한 기간일수 산출
lastdayp <- max(as.character(daup$log_date))
uuserp1$use_length <- as.numeric(as.Date(lastdayp, format="%Y-%m-%d")- as.Date(uuserp1$log_init, format="%Y-%m-%d") + 1)
uuserp1$recency <- as.numeric(as.Date(lastdayp, format="%Y-%m-%d")- as.Date(uuserp1$log_rcnt, format="%Y-%m-%d") + 1)

plot(uuserp1$recency, uuserp1$use_length)
# 분석기간 중 최초 일자는 마지막 이용 일자보다는 이전인 관계
cor(uuserp1$recency, uuserp1$use_length)
# 최초이용시점이 멀수록 최근 로그인 일자도 멀다
# = 이용중단 (=휴면화 or 이탈)이 발생한다

plot(jitter(uuserp1$recency), jitter(uuserp1$use_length),
     cex=0.7, pch=19, col=rgb(0,0,1,0.1))


plot(uuserp1$log_days, uuserp1$recency)
# 밀도를 통해 크게  skew된 관계임을 파악

plot(jitter(uuserp1$log_days), jitter(uuserp1$recency),
     cex=0.7, pch=19, col=rgb(0,0,1,0.1))
lines(lowess(uuserp1$recency ~ uuserp1$log_days),
      col="red", lwd=2)


#---- 미래 기간 :: 종속변수 생성 ------

dauf <- daub[!(daub$log_date %in% ulog0$log_date), c(1,3)]
uuserf <- as.data.frame(unique(dauf$user_id))
names(uuserf)[1] <- "user_id"

# user별 로그인 일수 계산
user_daysf <- aggregate(log_date ~ user_id, data = dauf, length)
head(user_daysf)
names(user_daysf)[2] <- "log_daysf"

# 과거(X) 와 미래(Y) 데이터 결합
uuserp1 <- merge(uuserp1, user_daysf, by="user_id", all.x=T)

# NA 값을 모두 0으로 대체
uuserp1$log_daysf[is.na(uuserp1$log_daysf)] <- 0
# 1보다 큰 값을 모두 1로 변환 - binary 형식의 종속변수 후보
uuserp1$loggedf <- ifelse(uuserp1$log_daysf>=1, 1, 0) 


# 미래 사용일수에 대한 초기 회귀분석 실시
lm1 <- lm(log_daysf~log_days + use_length + recency , data=uuserp1)
summary(lm1)

# 시각적 관계 확인 EDA

# 독립변수와 종속변수 1:1 비교
plot(jitter(uuserp1$log_days), jitter(uuserp1$log_daysf))
abline(lm(uuserp1$log_daysf~uuserp1$log_days), 
       col="red", lwd=2)
lines(lowess(uuserp1$log_daysf~uuserp1$log_days), 
      col="green", lwd=2)

plot(jitter(uuserp1$use_length), jitter(uuserp1$log_daysf))

col_tgt <- (uuserp1$log_daysf)/7
hist(col_tgt)
plot(uuserp1$log_days, uuserp1$use_length,
     col=rgb(col_tgt,0, 1-col_tgt,0.5), pch=19, cex=0.5)

# 이용주기 cycle로 변환 후 관계 확인
plot(jitter(uuserp1$log_days), jitter(uuserp1$use_length/uuserp1$log_days),
     col=rgb(col_tgt,0, 1-col_tgt,0.3), pch=19, cex=0.5)

plot(jitter(uuserp1$log_days), jitter(uuserp1$recency),
     col=rgb(col_tgt,0, 1-col_tgt,0.3), pch=19, cex=0.5)

# 변수간 상관관계 확인
cor(uuserp1[,c(2,5:7)])
# pairs(uuserp1[,c(2,5:7)])


#-----[A4] 추가 파생변수 생성 및 데이터 정제 ----------
# Feature Engineering

# 평균적인 이용주기 파생변수 생성
uuserp1$log_cycle <- uuserp1$use_length / uuserp1$log_days

plot(jitter(uuserp1$log_cycle), jitter(uuserp1$use_length), 
     col=grey(1- uuserp1$log_daysf/max(uuserp1$log_daysf)),
     pch=19, cex=0.6, xlim=c(0,30))

plot(jitter(uuserp1$log_cycle), jitter(uuserp1$use_length), 
     col=rgb( .5, (1- uuserp1$log_daysf/max(uuserp1$log_daysf)), .5, alpha=.6),
     pch=19, cex=0.6)

plot(jitter(uuserp1$log_cycle), jitter(uuserp1$recency), 
     col=rgb( uuserp1$log_daysf/max(uuserp1$log_daysf), 0, 
              (1- uuserp1$log_daysf/max(uuserp1$log_daysf)), alpha=.3),
     pch=19, cex=0.3)


# 이용자 기본 개인정보 demographics 구조 파악
str(userb)
length(unique(userb$user_id))
length(unique(userb$user_id, userb$install_date))

# user별 install date 확인
user_instfrq <- aggregate(install_date ~ user_id, data = userb, length)
head(user_instfrq)
names(user_instfrq)[2] <- "instfrq"

plot(user_instfrq$instfrq)
range(user_instfrq$instfrq)
# 설치일자는 모두 한 번

# user 항목 결합
uuserp1 <- merge(uuserp1, userb, by="user_id", all.x=T)

# inst_age == 설치후 몇일이나 되었는지
uuserp1$inst_age <- as.numeric(as.Date(lastdayp, format="%Y-%m-%d")- as.Date(uuserp1$install_date, format="%Y-%m-%d") + 1)
plot(uuserp1$inst_age)
range(uuserp1$inst_age)

names(userb)
names(uuserp1)
# 필요없는 컬럼 X 제거
uuserp1 <- uuserp1[,!(names(uuserp1) %in% c("X"))]
head(uuserp1)


#---- 완성된 데이터 셋 분포 확인 ---------
mean(uuserp1$log_daysf)

# 가설: 과거에 많이 이용했다면 미래 이용도 많을 것
plot(uuserp1$log_daysf, uuserp1$log_days)
plot(uuserp1$log_days, uuserp1$log_daysf)

plot(uuserp1$log_days, jitter(uuserp1$log_daysf))
plot(jitter(uuserp1$log_daysf)~uuserp1$log_days)
plot(jitter(uuserp1$log_daysf)~ jitter(uuserp1$log_days),
     cex=0.5)

plot(uuserp1$log_days, uuserp1$log_daysf)
# 겹침으로 인해 분포의 의미 파악 불가능

plot(jitter(uuserp1$log_days), jitter(uuserp1$log_daysf))
abline(lm(uuserp1$log_daysf~uuserp1$log_days), col="red",
       lwd=2)
cor(uuserp1$log_daysf, uuserp1$log_days)

# 폴리노미얼 추세선 ( locally-weighted polynomial regression ) 추가
# scatterplot Smoothing을 위한 함수 LOWESS
lines(lowess(uuserp1$log_days, uuserp1$log_daysf), col="blue", lwd=2) 

boxplot(uuserp1$log_days~ uuserp1$log_daysf)
boxplot(uuserp1$log_daysf~uuserp1$log_days, 
        xlab="log_days", ylab="log_daysf")

# 기간을 10일 단위로 끊어서 비교
boxplot(uuserp1$log_daysf~floor(uuserp1$log_days/10), 
        xlab="10일단위 이용일수 - 독립변수기간", 
        ylab="1일단위 이용일수 (=미래 1주일 = 종속변수기간)",
        col="steelblue")
# floor :: 소수점 이하 버림
grid()

# 성별
boxplot(uuserp1$log_daysf~uuserp1$gender,
        ylab="log_daysf")
# 연령별
boxplot(uuserp1$log_daysf~uuserp1$generation,
        ylab="log_daysf")
plot(jitter(uuserp1$generation), jitter(uuserp1$log_daysf), cex=0.1)
cor(uuserp1$generation, uuserp1$log_daysf)

# 디바이스유형별
boxplot(uuserp1$log_daysf~uuserp1$device_type,
        ylab="log_daysf")

# warm up period - 설치 후 사용까지의 기간
wuperiod <- uuserp1$inst_age - uuserp1$use_length
plot(density(wuperiod))
polygon(density(wuperiod), col="steelblue")
plot(sort(wuperiod))
# 상당수가 설치 당일 처음 사용 개시

plot(jitter(wuperiod), jitter(uuserp1$log_daysf), cex=0.5)
lines(lowess(uuserp1$log_daysf~wuperiod), 
      lwd=2, col="red")
cor(wuperiod, uuserp1$log_daysf)
boxplot(wuperiod~ uuserp1$log_daysf)


#-----[A5] 예비적 모델 개발 -----------

# demographic 변수와 추가 파생변수 반영

# 다중선형회귀분석
lm2 <- lm(log_daysf~log_days + use_length + recency + gender + generation + device_type + inst_age + log_cycle, data=uuserp1)
summary(lm2)


#--- EDA device_type 의미 파악 :: EDA ----

boxplot(uuserp1$log_daysf~ uuserp1$device_type)
grid(2,15)

boxplot(uuserp1$log_daysf~ uuserp1$device_type, ylim=c(0,2))
grid(10)

# 디바이스 유형별 미래기간 이용자 비율
nrow(uuserp1[ uuserp1$log_daysf>=1 & uuserp1$device_type=="Android",]) / nrow(uuserp1[  uuserp1$device_type=="Android",])
nrow(uuserp1[ uuserp1$log_daysf>=1 & uuserp1$device_type=="iOS",]) / nrow(uuserp1[  uuserp1$device_type=="iOS",])

# 디바이스 유형별 미래기간 이용일수 평균값
mean(uuserp1[ uuserp1$device_type=="Android",]$log_daysf) 
mean(uuserp1[ uuserp1$device_type=="iOS",]$log_daysf) 


plot(sort(uuserp1[ uuserp1$device_type=="iOS",]$log_daysf), type="l")
lines(sort(uuserp1[ uuserp1$device_type=="Android",]$log_daysf), col="red", lty=2)


# 미래 로그인 일수 분포상에 디바이스 유형 구별 시각화
plot(jitter(jitter(uuserp1$log_daysf)), pch=19, cex=0.7,
     col=ifelse(uuserp1$device_type=="Android","blue","red"))

plot(table(uuserp1$log_daysf, uuserp1$device_type)[,1] *100
     / (table(uuserp1$log_daysf, uuserp1$device_type)[,1] 
        + table(uuserp1$log_daysf, uuserp1$device_type)[,2]), 
     type="b", main="Proportion of Android by log_daysf" ,
     xaxt='n' , xlab="Days", ylab="%")
axis(1, at=1:8, labels=0:7)

log_daysf <- c(0:7)
andr_cnt <- table(uuserp1$log_daysf, uuserp1$device_type)[,1]
all_cnt <- table(uuserp1$log_daysf, uuserp1$device_type)[,1] + table(uuserp1$log_daysf, uuserp1$device_type)[,2]
log_daysf_dvc_type <- data.frame(log_daysf, andr_cnt, all_cnt)
log_daysf_dvc_type$andr_r <- log_daysf_dvc_type$andr_cnt / log_daysf_dvc_type$all_cnt

# 미래이용 일수별 구성비와 안드로이드 비율
plot(log_daysf_dvc_type$log_daysf, log_daysf_dvc_type$andr_r *100, 
     type="b", 
     main="미래이용 일수별 구성비와 안드로이드 비율")
points(log_daysf_dvc_type$log_daysf, log_daysf_dvc_type$all_cnt*100 /
         sum(log_daysf_dvc_type$all_cnt), type="l", lty=2)



#---- 예비적 의사결정나무 모델 개발 --------

# NA 존재여부 확인
anyNA(uuserp1)

# 트리나 랜덤포리스트를 위한 명목형 변수들은 factor 형식으로 변환 필요

library(party)
t1 <- ctree(log_daysf~log_days + recency , data=uuserp1, 
            controls = ctree_control(maxdepth = 4, minbucket=10))
plot(t1)

t1 <- ctree(log_daysf~log_days + use_length + recency + gender + generation +
              device_type + inst_age , data=uuserp1, 
            controls = ctree_control(maxdepth = 6, minbucket=100))
plot(t1)

plot(jitter(ifelse(uuserp1$device_type=="iOS",1,0)), uuserp1$log_days,
     col=rgb(log_daysf/7,0,1-(log_daysf/7),0.1), pch=19, cex=0.5)


#---- 의사결정나무 모델의 세부정보 검토

t1 # tree rule 출력
# weights --> observation count
# 집단의 값 (평균, 중위수 ... ) 

# 값대신 노드를 predict
pred <- predict(t1, data=uuserp1, type="node")
uuserp1_y <- uuserp1[,c("user_id","log_daysf")]
uuserp1_y$node <- pred
med_log_daysf_by_node <- aggregate(log_daysf ~ node, data = uuserp1_y, median)
mean_log_daysf_by_node <- aggregate(log_daysf ~ node, data = uuserp1_y, mean)
cnt_log_daysf_by_node <- aggregate(log_daysf ~ node, data = uuserp1_y, length)

plot(cnt_log_daysf_by_node$log_daysf, med_log_daysf_by_node$log_daysf,
     , main="트리 노드별 observation count와 log_daysf 중위수")
text(cnt_log_daysf_by_node$log_daysf, 
     med_log_daysf_by_node$log_daysf, labels=cnt_log_daysf_by_node$node,
     pos=2)


# binary 타겟 변수 (하루라도 로그인이 있었는지 여부) 사용하는 경우
t2 <- ctree(loggedf~log_days + use_length + recency + gender + generation + device_type + inst_age , data=uuserp1, controls = ctree_control(maxdepth = 3, minbucket=100))
plot(t2)

plot(jitter(uuserp1$log_days), jitter(uuserp1$recency), 
     col=rgb( .3, uuserp1$loggedf, .7, alpha=.3),
     pch=19, cex=0.5)

#  시각적으로 충분히 패턴을 확인하기 어렵다면
#  파생변수 생성을 통해 확인 필요 

t3 <- ctree(loggedf~log_days + use_length + recency + gender + generation
            + device_type + inst_age , 
            data=uuserp1, 
            controls = ctree_control(maxdepth = 10, minsplit=10, minbucket=2))


#---- 랜덤포리스트를 활용한 예비적 모델 개발 --------

library(randomForest)
rf1 <- randomForest(log_daysf~ log_days + use_length + recency + gender +    
                      generation + device_type + inst_age, data=uuserp1,
                    do.trace=10, ntree=400, importance=T)
plot(rf1)

rf1
# (1- %Var(y)) == (% Var explained)

# 상대적 변수중요도
varImpPlot(rf1)

# %IncMSE 와 IncNodePurity 는 보완적 지표. 반드시 일치하지는 않음
plot(importance(rf1)[,1], importance(rf1)[,2],
     pch=19, col=rgb(0,0,1,0.5),
     xlab=dimnames(importance(rf1))[[2]][1],
     ylab=dimnames(importance(rf1))[[2]][2])
text(importance(rf1)[,1], importance(rf1)[,2], 
     labels=rownames(importance(rf1)), pos=3)


# 가장 중요한 변수를 하나 제외 시켜본다면
rf2 <- randomForest(log_daysf ~ log_days + use_length + gender + generation 
                    + device_type + inst_age, data=uuserp1,
                    do.trace=20, ntree=200, importance=T)
plot(rf2)
varImpPlot(rf2)
rf2

# 종속변수 (Y, 타겟) 를 변형해본 경우
# binary 타겟 사용 -- 타겟 변수를 숫자형으로 설정한 경우
rf3 <- randomForest(loggedf~ log_days + use_length + recency + gender +    
                      generation + device_type + inst_age, data=uuserp1,
                    do.trace=20, ntree=200, importance=T)
plot(rf3)
rf3
varImpPlot(rf3)


# binary 타겟 사용 -- 타겟 변수가 명목형 설정일 경우 (=classification)
uuserp2 <- uuserp1
uuserp2$loggedf <- as.factor(uuserp2$loggedf)

rf4 <- randomForest(loggedf~ log_days + use_length + recency + gender +    
                      generation + device_type + inst_age, data=uuserp2,
                    do.trace=20, ntree=600, importance=T)
plot(rf4)
rf4
varImpPlot(rf4)


# OOB에러 : 전체 ; class별 에러
# Confusion matrix 확인 필요
# trace에서 1, 2는 각각 1번, 2번 클래스에 대한 오차율


# 예측값 생성 - Scoring (=inference)
fitted.results.rf <- predict(rf4, newdata=uuserp2, type="prob")
head(fitted.results.rf)

# type="prob" 옵션은 타겟이 클래스일 경우(=classification) 클래스별 확률 출력


# 의사결정나무 - Scoring
fitted.results.dt <- predict(t3, newdata=uuserp2, type="prob")
head(unlist(fitted.results.dt))

# 의사결정나무와 RF의 예측확률 비교
col_loggedf <- as.numeric(uuserp2$loggedf) -1
plot(jitter(unlist(fitted.results.dt)), fitted.results.rf[,2],
     xlab="prob of DT", 
     ylab="prob of RF",
     col=rgb(col_loggedf, 0, 1-col_loggedf, 0.3),
     pch=19, cex=0.5 )
abline(h=0.5, lty=3)
abline(v=0.5, lty=3)


#----- 연속형 타겟의 실제값과 예측값 관계 확인 --------

plot(jitter(uuserp2$log_daysf), jitter(predict(t1, data=uuserp2)),
     main="Predicted Vs. Actual")
# 실제값과 예측오차값 관계 확인
# 이상적인 선은 대각선
abline(a=0,b=1, lty=3, col="green")
aa <- uuserp1$log_daysf-predict(t1, data=uuserp2)
# 선형회귀 추세선 추가
abline(lm(aa~uuserp2$log_daysf), col="red")
# 폴리노미얼 추세선 추가
lines(lowess(uuserp2$log_daysf, aa), col="blue")

plot(sort(uuserp2$log_daysf-predict(t1, data=uuserp2)), 
     main="Residual distribution", ylab="Residual")
abline(0,0) # 기준선 추가

uuserp2$predicted <- predict(t1, data=uuserp2)
plot(jitter(uuserp2$log_daysf), jitter(uuserp2$predicted-uuserp2$log_daysf))
abline(h=0, lty=2) # 오차 0인 경우
# 실제값이 클 수록 음의 방향으로 오차 크게 발생

# 좀 더 정확한 확인 boxplot
boxplot((uuserp2$predicted-uuserp2$log_daysf)~uuserp2$log_daysf,
        xlab="actual", ylab="prediction error",
        main="prediction errors by actual value",
        col="steelblue")
abline(h=0, lty=2)

plot(jitter(uuserp2$predicted), 
     jitter(jitter(uuserp2$log_daysf)-jitter(uuserp2$predicted)), 
     main="Residual Vs. Fitted")


#---- 모델별 평균 절대 오차 (MAE, MSE) 계산 ----

# 모델의 예측 결과를 저장 (target : log_daysf )
uuserp2$actual <- uuserp2$log_daysf
uuserp2$predicted_dt <- predict(t1, data=uuserp2)[,1]
uuserp2$predicted_rf <- predict(rf1, data=uuserp2)

# MAE (mean absolute error)
mean(abs(uuserp2$actual - uuserp2$predicted_dt)) # DT모델 오차
mean(abs(uuserp2$actual - uuserp2$predicted_rf)) # 랜덤포리스트모델 오차
# ntree=1000 --> 0.3827947

# MSE (mean sequred error)
mean((uuserp2$actual - uuserp2$predicted_dt)^2) # DT모델 오차
mean((uuserp2$actual - uuserp2$predicted_rf)^2) # 랜덤포리스트



#-----[A6] 데이터 분할과 모델 평가 ----------------

#------- [모델 검증]-----------------

# 데이터 분할 partitioning
# data partitioning - using random sampling
# 80%를 훈련용으로 (모델생성에) 사용
smp_size <- floor(0.8 * nrow(uuserp2))

# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(uuserp2)), size = smp_size)
uuserp2train <- uuserp2[train_ind, ]
uuserp2test <- uuserp2[-train_ind, ]

nrow(uuserp2train) ; nrow(uuserp2test)

barplot(c(nrow(uuserp2train), nrow(uuserp2test) ))
axis(side=1, at=1:2, labels=c("Train", "Test"))



#--- 예측력 확인과 ROC 챠트 생성 -------

#--- binary target (loggedf) 예측 트리 모델 생성 후 평가
t4 <- ctree(loggedf ~ log_days + use_length + recency + gender + generation +
              device_type + inst_age , data=uuserp2train, 
            controls = ctree_control(maxdepth = 4, minbucket=50))
# 트리 깊이를 4 까지로 설정
plot(t4)

fitted.results <- predict(t4, newdata=uuserp2test, type="response")

# test set으로 모델 평가
# Confusion matrix 생성
addmargins(table(fitted.results, uuserp2test$loggedf))
misClasificError <- mean(fitted.results != uuserp2test$loggedf)
print(paste('Accuracy', as.integer((1-misClasificError)*10000)/100, '%' ))


fitted.results <- predict(t4, newdata=uuserp2test, type="prob")
# 트리를 사용한 예측결과(리스트)에서 클래스별 확률값을 추출
trnt1 <- unlist(fitted.results)
trnt2<- data.frame(flg = rep(1:2,nrow(uuserp2test)), trnt1 = trnt1)
prob_y <- trnt2[trnt2$flg==2,]$trnt1

# 예측된 확률값의 분포 확인
plot(sort(prob_y))

# 예측한 확률이 0.5보다 큰 건의 비율
mean(ifelse(prob_y>0.5,1,0))
# 실제값에서 이용한 경우(1) 의 비율
mean(ifelse(uuserp2test$loggedf==1,1,0))


# ROC 챠트 작성
# Receiver operating characteristic == ROC
# (binary) classifier 진단을 위한 도구
# install.packages("ROCR")
library(ROCR)

pr <- prediction(prob_y, ifelse(uuserp2test$loggedf=="1",1,0))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize=T)
abline(a=0,b=1,lwd=1,lty=2, col="grey")

auc <- performance(pr, measure = "auc")@y.values[[1]]
auc  # Ratio of Area under the ROC curve


#--- randomForest 모델 생성 후 평가

library(randomForest)
rf5 <- randomForest(loggedf ~ log_days + use_length + recency + gender 
                    + generation + device_type + inst_age, data=uuserp2train, 
                    do.trace=50, ntree=1000, importance=T)
rf5
plot(rf5)
varImpPlot(rf5)

# 결과에 출력된 Confusion matrix 확인 필요
# trace에서 1, 2는 각각 1번, 2번 (factor level) 클래스에 대한 오차율

fitted.results <- predict(rf5, newdata=uuserp2test, type="prob")
head(fitted.results)

# take prob of yes
pr <- prediction(fitted.results[,2], ifelse(uuserp2test$loggedf=="1",1,0))
# ROC 챠트 생성
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize=T, main="ROC of RF")
abline(a=0,b=1,lwd=1,lty=2,col="gray")

auc <- performance(pr, measure = "auc")@y.values[[1]]
auc # area under curve - 1에 가까울수록 바람직함. 0.5 이상의 값을 가짐

# Lift 챠트 작성
prf <- performance(pr,"lift","rpp")
plot(prf, main="lift curve", colorize=T)

# [ rule of thumb ] 상위 10% 지점, 통상 3 이상은 Lift가 되어야
abline(v=0.1, lty=2) 
abline(h=3, lty=2)

# 정확도 산출
fitted.results <- predict(rf5, newdata=uuserp2test, type="response")
table(fitted.results, ifelse(uuserp2test$loggedf=="1",1,0))
misClasificError <- mean(fitted.results != uuserp2test$loggedf)
print(paste('Accuracy', as.integer((1-misClasificError)*10000)/100, '%' ))



#-----[A7] 추가적 파생변수 생성  -------

# 최근 활동 기간별 변수 생성  

# 최종 7일 즉 1주일간의 데이터(미래) 이전의 14일(2주) 행동을 집계

ulog1 <- tail(head(tblog_date[order(tblog_date$log_date),], nrow(tblog_date)-7), 14)
ulog1$Freq <- 0 # 임시 생략 조치 - 혼동 방지
daup1 <- daub[daub$log_date %in% ulog1$log_date,c(1,3)]

# user별로 최근2주간 이용 일수를 집계
user_days1 <- aggregate(log_date ~ user_id, data = daup1, function(x) length(x))
names(user_days1)[2] <- "log_days_r2w"
head(user_days1)
plot(user_days1$user_id, user_days1$log_days_r2w)
uuserp2 <- merge(uuserp2, user_days1, by="user_id", all.x=T)
uuserp2$log_days_r2w[is.na(uuserp2$log_days_r2w)] <- 0 # NA값 0으로 대체

# 최종 7일 즉 1주일간의 데이터(미래) 이전의 7일(1주) 행동을 집계

ulog2 <- tail(head(tblog_date[order(tblog_date$log_date),], nrow(tblog_date)-7), 7)
ulog2$Freq <- 0 # 임시 생략 조치 - 혼동 방지
daup2 <- daub[daub$log_date %in% ulog2$log_date,c(1,3)]

# user별로 최근 1주간 이용 일수를 집계
user_days2 <- aggregate(log_date ~ user_id, data = daup2, function(x) length(x))
names(user_days2)[2] <- "log_days_r1w"
head(user_days2)
plot(user_days2$user_id, user_days2$log_days_r1w)
uuserp2 <- merge(uuserp2, user_days2, by="user_id", all.x=T)
uuserp2$log_days_r1w[is.na(uuserp2$log_days_r1w)] <- 0 # NA값 0으로 대체

# 최종 7일 즉 1주일간의 데이터(미래) 이전의 3일 행동을 집계

ulog3 <- tail(head(tblog_date[order(tblog_date$log_date),], nrow(tblog_date)-7), 3)
ulog3$Freq <- 0 # 임시 생략 조치 - 혼동 방지
daup3 <- daub[daub$log_date %in% ulog3$log_date,c(1,3)]

# user별로 최근 3일간 이용 일수를 집계
user_days3 <- aggregate(log_date ~ user_id, data = daup3, function(x) length(x))
names(user_days3)[2] <- "log_days_r3d"
head(user_days3)
plot(user_days3$user_id, user_days3$log_days_r1w)
uuserp2 <- merge(uuserp2, user_days3, by="user_id", all.x=T)
uuserp2$log_days_r3d[is.na(uuserp2$log_days_r3d)] <- 0 # NA값 0으로 대체

# 최종 7일 즉 1주일간의 데이터(미래) 이전의 28일(4주) 행동을 집계

ulog4 <- tail(head(tblog_date[order(tblog_date$log_date),], nrow(tblog_date)-7), 28)
ulog4$Freq <- 0 # 임시 생략 조치 - 혼동 방지
daup4 <- daub[daub$log_date %in% ulog4$log_date,c(1,3)]

# user별로 최근 4주간 이용 일수를 집계
user_days4 <- aggregate(log_date ~ user_id, data = daup4, function(x) length(x))
names(user_days4)[2] <- "log_days_r4w"
head(user_days4)
plot(user_days4$user_id, user_days4$log_days_r4w)
uuserp2 <- merge(uuserp2, user_days4, by="user_id", all.x=T)
uuserp2$log_days_r4w[is.na(uuserp2$log_days_r4w)] <- 0 # NA값 0으로 대체


# 이용일자간 간격 관련 파생변수 생성

# get max interval between log dates

ulog5 <- tail(tblog_date[order(tblog_date$log_date),], 7)
ulog5$Freq <- 0 # 임시 생략 조치 - 혼동 방지
daup5 <- daub[!(daub$log_date %in% ulog5$log_date), c(1,3)]

tmpc <- daup5[order(daup5$user_id, daup5$log_date),]
tmpc1 <- tmpc[2:nrow(tmpc),]
tmpc2 <- cbind(tmpc[1:(nrow(tmpc)-1),], tmpc1)
names(tmpc2) <-c('log_date', 'user_id', 'log_date0', 'user_id0')
tmpc3 <- tmpc2[tmpc2$user_id==tmpc2$user_id0,]
tmpc3$interval <- as.Date(tmpc3$log_date0)-as.Date(tmpc3$log_date)
tmpc4 <- aggregate(interval~user_id, data=tmpc3, FUN=max)
tmpc4$interval <- as.numeric(tmpc4$interval)
names(tmpc4) <- c('user_id', 'max_interval')
df_maxi <- tmpc4
# 간격의 sd
tmpc5 <- aggregate(interval~user_id, data=tmpc3, FUN=sd)
tmpc5$interval <- as.numeric(tmpc5$interval)
names(tmpc5) <- c('user_id', 'sd_interval')
df_maxi <- merge(tmpc4, tmpc5, by="user_id", all.x=T) 
df_maxi$sd_interval <- ifelse(is.na(df_maxi$sd_interval),0,df_maxi$sd_interval)

# 간격의 최소
tmpc6 <- aggregate(interval~user_id, data=tmpc3, FUN=min)
tmpc6$interval <- as.numeric(tmpc6$interval)
names(tmpc6) <- c('user_id', 'min_interval')
df_maxi <- merge(df_maxi, tmpc6, by="user_id", all.x=T) 
df_maxi$min_interval <- ifelse(is.na(df_maxi$min_interval),0,df_maxi$min_interval)


uuserp2 <- merge(uuserp2, df_maxi, by="user_id", all.x=T)
uuserp2$max_interval[is.na(uuserp2$max_interval)] <- 0 # NA값 0으로 대체
uuserp2$sd_interval[is.na(uuserp2$sd_interval)] <- 0 # NA값 0으로 대체
uuserp2$min_interval[is.na(uuserp2$min_interval)] <- 0 # NA값 0으로 대체

# 설치한 날자의 요일을 변수로 추가
uuserp2$install_wd <- as.factor(weekdays(as.Date(uuserp2$install_date)))

# partition again using the same index
uuserp2train <- uuserp2[train_ind,]
uuserp2test <- uuserp2[-train_ind,]




#------ 파생변수를 추가한 후 모델 생성 (=> 평가 => 반복적 보완 )-------

t5 <- ctree(loggedf ~ log_days + use_length + recency + gender + generation + 
              device_type + inst_age + log_days_r4w  + log_days_r2w + log_days_r1w + log_days_r3d
            + install_wd + max_interval + sd_interval + min_interval,  
            data=uuserp2train, 
            controls = ctree_control(maxdepth = 8, minbucket=30))
plot(t5) 

# 정확도 확인
fitted.results <- predict(t5, newdata=uuserp2test, type="response")
table(fitted.results, ifelse(uuserp2test$loggedf=="1",1,0))
misClasificError <- mean(fitted.results != uuserp2test$loggedf)
print(paste('Accuracy', as.integer((1-misClasificError)*10000)/100, '%' ))

# 핵심 변수 두 가지를 사용한  scatter plot 작성
# 최근 1주일간 이용일수와 최근 3일간 이용일수 
# 최근 1주일간 이용일수 > 최근 3일간 이용일수 

plot(jitter(uuserp2$log_days_r1w), jitter(uuserp2$log_days_r3d), 
     col=rgb( .3, (1- uuserp1$log_daysf/max(uuserp1$log_daysf)), .7, alpha=.5),
     pch=19, cex=0.5)


rf6 <- randomForest(loggedf ~ log_days + use_length + recency + gender 
                    + generation + device_type + inst_age + log_days_r4w + log_days_r2w + log_days_r1w + 
                      log_days_r3d  + install_wd + max_interval + sd_interval + min_interval,  
                    data=uuserp2train, 
                    do.trace=100, ntree=2000, importance=T)
plot(rf6)
varImpPlot(rf6)
rf6

# 정확도 확인
fitted.results <- predict(rf6, newdata=uuserp2test, type="response")
table(fitted.results, ifelse(uuserp2test$loggedf=="1",1,0))
misClasificError <- mean(fitted.results != uuserp2test$loggedf)
print(paste('Accuracy', as.integer((1-misClasificError)*10000)/100, '%' ))

# 주요 영향변수 사후 확인
plot(jitter(uuserp2$log_days_r2w), jitter(uuserp2$recency), 
     col=rgb( .3, (1- uuserp1$log_daysf/max(uuserp1$log_daysf)), .7, alpha=.5),
     pch=19, cex=0.5)

plot(jitter(uuserp2$recency), jitter(uuserp2$use_length), 
     col=rgb( .3, (1- uuserp1$log_daysf/max(uuserp1$log_daysf)), .7, alpha=.5),
     pch=19, cex=0.5)

plot(jitter(uuserp2$recency), jitter(uuserp2$recency/uuserp2$use_length), 
     col=ifelse(uuserp2$loggedf==1,"red","blue"),
     pch=19, cex=0.5)

plot(uuserp2$sd_interval, uuserp2$log_days, 
     col=ifelse(uuserp2$loggedf==1,"red","blue"))



#---- End of Script [A]-----------#




#----[B] 이상패턴 탐지 (anomaly detection) ---------------------

# Isolation Forest 알고리즘을 활용하여 
# 여러 변수가 존재하는 상황에서의 이상 패턴을 자동 발견
# 이상 패턴의 특성과 의미 파악


### data : B2B online shopping for AD
# data source : UCI 머신러닝  data repository
# http://archive.ics.uci.edu/ml/datasets/online+retail

# a UK-based and registered non-store online retail.
# the company mainly sells unique all-occasion gifts. 
# many customers of the company are wholesalers.


#----[B1] data set 로딩 -------
# 500 고객만 무작위 추출한 sample 활용
# ret1 <- read.csv("Sample_OnRet.csv")
# 블로그(링크)로부터 바로 data import
ret1 <- read.csv("http://cfile227.uf.daum.net/attach/99CE763F5A619427119FB7") 
length(unique(ret1$CustomerID))

# 데이터 기간 확인 --> 약 1년간의 거래 내역
range(unique(substr(as.character(ret1$InvoiceDate),1,10)))


#----[B2] data partitioning (과거 Vs. 미래) -------

# 2011-11-01 이후 데이터를 구분 (12개월 중 마지막 1개월 가량)
ret2 <- ret1[substr(as.character(ret1$InvoiceDate),1,10)<="2011-10-31",]
ret3 <- ret1[substr(as.character(ret1$InvoiceDate),1,10)>"2011-10-31",]


#----[B3] 기본적인 집계변수 생성 ----------

# 먼저 취소/반품건 포함여부 확인

plot(sort(ret2$Quantity))
# ==> 취소/반품 건이 존재하므로 해당 건은 제외하고 변수 생성

plot(sort(ret2$UnitPrice))
range(sort(ret2$UnitPrice))
# ==> 단가가 0인 건 존재하므로 제외

nrow(ret2)
ret21 <- ret2[ret2$Quantity>0 & ret2$UnitPrice>0 ,]
nrow(ret21)

# get Recency (R)
agg1 <-aggregate(substr(as.character(InvoiceDate),1,10) ~ CustomerID, 
                 FUN=max, na.rm=TRUE, data=ret21)
head(agg1)

# R계산을 위한 최종일 +1일 기준
# 날짜-시간 형식이므로 날짜만 추출해서 계산
agg1$R <- as.Date("2011-12-10")- as.Date(agg1[,2])
head(agg1)
hist(as.numeric(agg1$R)) 


# get Frequency (F)
# 날짜가 같다면 (=하루에 여러건 구매했어도) Frequency는 1로 count
df_F <- unique(data.frame(CustomerID = ret21$CustomerID, 
                          InvoiceDate = substr(as.character(ret21$InvoiceDate),1,10) ))

agg2 <-aggregate(df_F$InvoiceDate, by=list(df_F$CustomerID), 
                 FUN=length)

names(agg2) <- c("CustomerID", "InvoiceFrequency")
head(agg2)
hist(agg2$InvoiceFrequency)

# 극단적으로 skewed -- 좀 더 촘촘하게
hist(agg2$InvoiceFrequency, breaks=100)


# get Monetary (M)

df_M <- data.frame(CustomerID = ret21$CustomerID, 
                   Amt = ret21$Quantity * ret21$UnitPrice)
agg3 <-aggregate(Amt~CustomerID,  FUN=sum, data=df_M)

hist(agg3$Amt, breaks=1000)

# 대부분의 고객이 포함된 구간만 Zoom-In
hist(agg3$Amt[agg3$Amt<=10000], breaks=50)


# merge R, F, M

# create base table
df_RFM <- aggregate(InvoiceNo~CustomerID, data=ret2, FUN=length)
nrow(df_RFM)
head(df_RFM)

df_RFM <- merge(df_RFM, agg1[c(1,3)], by="CustomerID", all.x=T)
df_RFM <- df_RFM[,-2] # InovoiceNo 제거
df_RFM$R <- as.numeric(df_RFM$R) # 숫자형식으로 변경

df_RFM <- merge(df_RFM, agg2, by="CustomerID", all.x=T)
names(df_RFM)[3] <-  "F"

df_RFM <- merge(df_RFM, agg3, by="CustomerID", all.x=T)
names(df_RFM)[4] <-  "M"

# Null 존재 여부 확인
colMeans(df_RFM[,2:4])

# Null인 값의 처리
# 1년기간 중 정상주문이 없다면 R을 최대값으로
df_RFM$R <- ifelse(is.na(df_RFM$R), max(df_RFM$R, na.rm=T), df_RFM$R)
# Null은 0으로 치환
df_RFM$F <- ifelse(is.na(df_RFM$F), 0, df_RFM$F)
df_RFM$M <- ifelse(is.na(df_RFM$M), 0, df_RFM$M)

# Null 존재 여부 다시 확인
colMeans(df_RFM[,2:4])

# 변수간 관계 시각적 분석 - EDA (Scatter plot)
plot(df_RFM$R, jitter(df_RFM$F))
abline(lm(df_RFM$F~df_RFM$R), col="red")
cor(as.numeric(df_RFM$R), df_RFM$F)

# 양변에 log를 취해서 관계 확인
plot(log(as.numeric(df_RFM$R)), log(df_RFM$F+1))
cor(log(as.numeric(df_RFM$R)), log(df_RFM$F+1))
# 좀 더 강한 log linear 상관관계 (negative)

plot(jitter(df_RFM$F), df_RFM$M)
abline(lm(df_RFM$M~df_RFM$F), col="red")

# 값이 아닌 순서 개념이 유용한지 확인
plot(jitter(rank(df_RFM$F, ties.method="random")), rank(df_RFM$R), 
     col=ifelse(rank(df_RFM$M)>=400,"blue","grey"))
abline(lm(rank(df_RFM$R)~rank(df_RFM$F, ties.method="random")), col="red")
cor(rank(df_RFM$F, ties.method="random"), rank(df_RFM$R)) 


#---- 미래기간 고객별 구매일수 집계 (optional) -----

# 응용: 과거행동에서 패턴이 급격하게 달라진 고객 탐지?

# 먼저 취소/반품건 제외
ret31 <- ret3[ret3$Quantity>0 & ret3$UnitPrice>0 ,]
nrow(ret3) ; nrow(ret31)

# 역시 날짜가 같다면 (=하루에 여러건 구매했어도) Frequency는 1로 count
df_Ftr <- unique(data.frame(CustomerID = ret31$CustomerID, 
                            InvoiceDate = substr(as.character(ret31$InvoiceDate),1,10) ))

agg6 <-aggregate(df_Ftr$InvoiceDate, by=list(df_Ftr$CustomerID), 
                 FUN=length)

names(agg6) <- c("CustomerID", "InvoiceFreq_Ftr")
head(agg6)

# Null 값 채우기
df_RFM_Ftr <- merge(df_RFM, agg6, by="CustomerID", all.x=T)
head(df_RFM_Ftr)
df_RFM_Ftr$InvoiceFreq_Ftr <- ifelse(is.na(df_RFM_Ftr$InvoiceFreq_Ftr), 0, 
                                     df_RFM_Ftr$InvoiceFreq_Ftr)
head(df_RFM_Ftr)


#-----[B4] Anomaly Detection 모델 생성 -------

# IsolationForest 패키지 설치
# install.packages("IsolationForest", repos="http://R-Forge.R-project.org")
# [주의] 최신버전의 R에서만 정상 작동

#    [ 오픈소스 R을 사용한 anomaly detection 예제를 통한  
#      탐색적 분석(EDA) Tutorial  전용준2018. 리비젼컨설팅 ] 
#      R anomaly detection Practice EDA 전용준.2018

# 참고: python에도 sklearn.ensemble.IsolationForest 존재


library(IsolationForest)

df_RFM1 <- df_RFM
# 변수 R을 수치형으로 변경
df_RFM1$R <- as.numeric(df_RFM1$R)

# train a model of Isolation Forest
itr1 <- IsolationTrees(df_RFM1[, c("R","F","M")], ntree=100, 
                       rFactor=0.5, nmin=1)

# compute anomaly score
as<-AnomalyScore(df_RFM1, itr1)

# show anomaly score
plot(density(as$outF ))
sort(as$outF, decreasing=T)[1:2]


#-----[B5] Anomaly Score 의미 이해 -------

df_RFM1$outF <- as$outF 

# decision tree를 활용한 시각화
library(party)
ctv1 <- ctree(outF~., data=df_RFM1[, c("R","F","M","outF")],
              controls = ctree_control(maxdepth = 4, minbucket=10))
plot(ctv1)

# DT 분기의 주요 변수를 활용한 scatter plot 시각화
plot(df_RFM1$R, df_RFM$F,
     cex=0.5, col=rgb(0,0,1,0.5), pch=19)
abline(v=c(53,99), lty=3)

# 랜덤포리스트를 활용한 변수별 Anomaly Score 생성 영향도 파악
# install.packages("randomForest")
library(randomForest)
rfv1 <- randomForest(outF~., data=df_RFM1[,!(names(df_RFM1) %in% "CustomerID")], 
                     nodesize=2, ntree=3000)
# 변수 중요도 검토
varImpPlot(rfv1)


#-----[B6] 변수 설계 재검토 -------

hist(df_RFM1$M)
plot(density(df_RFM1$M))

# skewed 분포가 당연한 것이라면?
plot(density(log(df_RFM1$M))) # log

plot(density(log(log(df_RFM1$M+1)+1))) # double log
abline(v=mean(log(log(df_RFM1$M+1)+1)), lty=3)
abline(v=mean(log(log(df_RFM1$M+1)+1)) + (sd(log(log(df_RFM1$M+1)+1))*c(-3,3)), lty=3, col="red")
# 거래가 전혀 없었던(취소/반품 이외) 고객 일부를 제외하면 거의 정상 범위?

plot(density(df_RFM1$F+1))
plot(density(log(df_RFM1$F+1)))

plot(density(log(log(df_RFM1$F+1)+1)))
# 로그변환을 두 번 해본다면?
abline(v=mean(log(log(df_RFM1$F+1)+1)), lty=3)
abline(v=mean((log(log(df_RFM1$F+1)+1))) + (sd(log(log(df_RFM1$F+1)+1))*c(-3,3)), lty=3, col="red")


#-----[B7] 로그변환한 변수를 사용한 AD 모델 생성 -------
#---- AD after log transformation 

# log 변환 후를 전제로 이상치를 찾아 본다면?

# UDF for rescale anomaly score
# 스코어의 스케일을 0~1 사이로 변환하기 위한 사용자정의함수
# 시각화 scatter plot에 활용
minmaxscl <- function(x) {
  x1 <- (x-min(x)) / (max(x)-min(x))
  return(x1)
}

# 로그변환 적용
df_RFM1$lR <- log(df_RFM1$R)
df_RFM1$lF <- log(df_RFM1$F+1)
df_RFM1$lM <- log(df_RFM1$M+1)
plot(density(df_RFM1$lM))

# 모델 생성
library(IsolationForest)
# train a model of Isolation Forest
itr2 <- IsolationTrees(df_RFM1[,c("lR","lF","lM")], ntree=1000, 
                       rFactor=0.5, nmin=1)
as <- AnomalyScore(df_RFM1[,c("lR","lF","lM")], itr2)

# show anomaly score
plot(density(as$outF ))
sort(as$outF, decreasing=T)[1:2]

df_RFM1$soutF <- as$outF

head(df_RFM1)
# log 취한 변수를 사용한 모델과 이전 모델의 anomaly score 비교
plot(df_RFM1$outF, df_RFM1$soutF)

# 트리 사용 시각적 검토
library(party)
ctv2 <- ctree(soutF~., data=df_RFM1[,c("soutF","lR","lF","lM")],
              controls = ctree_control(maxdepth = 4, minsplit=2, minbucket=1))
plot(ctv2)

# scatter plot을 활용한 score 의미 검토
mmScl_outF <- minmaxscl(df_RFM1$soutF)
plot(df_RFM1$lM, df_RFM1$lF,
     cex=0.5, col=rgb(mmScl_outF,0,1-mmScl_outF,0.5), pch=19)
abline(v=2.32, lty=3)
abline(h=c(0,3.09), lty=3)


# 랜덤포리스트(변수중요도)를 활용한 변수별 Anomaly Score 생성 영향도 파악
# install.packages("randomForest")
library(randomForest)
rfv1 <- randomForest(soutF~., data=df_RFM1[,c("soutF","lR","lF","lM")], 
                     nodesize=2, ntree=1000)
varImpPlot(rfv1)


# [추가검토 Point]
# 취소반품건 처리 방식에 따라 다른 결과, 추가변수 사용 가능
# 변수 수가 작다면 시각적으로도 조합별 구분 파악 가능
# 그러나 변수가 많다면 (예: 100개) ?


#-----[B8] 추가적인 파생변수들 생성 후 재분석 -------
#---- add derived variables ------

# 건별 수량에 대한 중위수 집계
agg8 <-aggregate(Quantity~CustomerID,  FUN=median, data=ret21)

plot(density(agg8$Quantity))
head(sort(agg8$Quantity))
# 분포를 보고 로그변환하기로 결정
agg8$Quantity <- log(agg8$Quantity+1)
names(agg8) <- c("CustomerID","lmedQuantity")

agg9 <-aggregate(UnitPrice~CustomerID,  FUN=median, data=ret21)
plot(density(agg9$UnitPrice))
agg9$UnitPrice <- log(agg9$UnitPrice+1)
names(agg9) <- c("CustomerID","lmedUnitPrice")

agg10 <-aggregate(Quantity~CustomerID,  FUN=sd, data=ret21)
agg10$Quantity[is.na(agg10$Quantity)] <- 0
plot(density(agg10$Quantity))
plot(density((agg10$Quantity+0.001)/mean(ret21$Quantity)))
agg10$Quantity <- log((agg10$Quantity+0.001)/mean(ret21$Quantity))
names(agg10) <- c("CustomerID","lcvQuantity")

agg11 <-aggregate(UnitPrice~CustomerID,  FUN=sd, data=ret21)
agg11$UnitPrice[is.na(agg11$UnitPrice)] <- 0
plot(density(agg11$UnitPrice))
plot(density((agg11$UnitPrice+0.001)/mean(ret21$UnitPrice)))
agg11$UnitPrice <- log((agg11$UnitPrice+0.001)/mean(ret21$UnitPrice))
names(agg11) <- c("CustomerID","lcvUnitPrice")


df_RFM2 <- df_RFM1

df_RFM2 <- merge(df_RFM2, agg8, by="CustomerID", all.x=T)
# df_RFM2$lmedQuantity <- ifelse(is.na(df_RFM2$lmedQuantity), 0,df_RFM2$lmedQuantity)
df_RFM2 <- merge(df_RFM2, agg9, by="CustomerID", all.x=T)
df_RFM2 <- merge(df_RFM2, agg10, by="CustomerID", all.x=T)
df_RFM2 <- merge(df_RFM2, agg11, by="CustomerID", all.x=T)

df_RFM2$lmedQuantity <- ifelse(is.na(df_RFM2$lmedQuantity), min(df_RFM2$lmedQuantity, na.rm=T), df_RFM2$lmedQuantity)
df_RFM2$lmedUnitPrice <- ifelse(is.na(df_RFM2$lmedUnitPrice), min(df_RFM2$lmedUnitPrice, na.rm=T), df_RFM2$lmedUnitPrice)
df_RFM2$lcvQuantity <- ifelse(is.na(df_RFM2$lcvQuantity), min(df_RFM2$lcvQuantity, na.rm=T),  df_RFM2$lcvQuantity)
df_RFM2$lcvUnitPrice <- ifelse(is.na(df_RFM2$lcvUnitPrice), min(df_RFM2$lcvUnitPrice, na.rm=T), df_RFM2$lcvUnitPrice)


# 최근 기간의 행동만을 반영한 추가 파생변수 생성
# create a variable representing rescent activness
ret5 <- ret1[substr(as.character(ret1$InvoiceDate),1,10)<="2011-10-31" &
               substr(as.character(ret1$InvoiceDate),1,10)>="2011-08-01" &
               ret1$Quantity>0 & ret1$UnitPrice >0 ,]
agg12 <-aggregate(Quantity~CustomerID,  FUN=length, data=ret5)
names(agg12)[2] <- 'rmonF'
hist(agg12$rmonF)

df_RFM21 <- df_RFM2[,c("CustomerID", "F")]
df_RFM21 <- merge(df_RFM21, agg12, by="CustomerID", all.x=T)
head(df_RFM21)

df_RFM21$rmonF[is.na(df_RFM21$rmonF)] <- 0
df_RFM21$rmonF <- log(df_RFM21$rmonF+1)
df_RFM21 <- df_RFM21[,c("CustomerID","rmonF")]
df_RFM2 <- merge(df_RFM2, df_RFM21, by="CustomerID", all.x=T)

df_RFM3 <- df_RFM2[,c("CustomerID","lR", "lF", "lM", "lmedQuantity", "lmedUnitPrice",
                      "lcvQuantity", "lcvUnitPrice", "rmonF")]

# 국가 관련 변수 생성
# create Country tag variable
# 거래 발생 국가가 복수인 경우가 존재하므로 건수가 많은 쪽으로 대표로 지정
d5 <- aggregate(InvoiceNo~CustomerID+Country, data=ret2, FUN=length)
d6 <- aggregate(InvoiceNo~CustomerID, data=d5, FUN=max)
d7 <- merge(d6, d5)[,c("CustomerID", "Country")]
head(d7)
df_RFM3 <- merge(df_RFM3, d7, by="CustomerID", all.x=T)

d8 <- unique(ret2[,c("CustomerID", "Country")])
d9 <- aggregate(Country~CustomerID, data=d8, FUN=length)
names(d9) <- c("CustomerID","nCntry")
df_RFM3 <- merge(df_RFM3, d9, by="CustomerID", all.x=T)

# df_RFM3 <- df_RFM3[df_RFM3$lF>0 & df_RFM3$lM>0, ]
# check if any NaN
colMeans(df_RFM3[,!(names(df_RFM3) %in% c("CustomerID", "outF", "Country"))])


# AD 모델 생성
itr3 <- IsolationTrees(df_RFM3[,!(names(df_RFM3) %in% c("CustomerID", "outF"))], 
                       ntree=1000, 
                       rFactor=0.5, nmin=1)

# compute anomaly score
as <- AnomalyScore(df_RFM3, itr3)

# show anomaly score distribution
plot(density(as$outF ))
sort(as$outF, decreasing=T)[1:2]

# check what matters the most 

df_RFM3$outF <- as$outF 

library(party)
ctv1 <- ctree(outF~., data=df_RFM3,
              controls = ctree_control(maxdepth = 4, minsplit=3, minbucket=1))
plot(ctv1)

# 색상에 활용하기 위해 0~1 구간으로 스코어 분포를 변경
mmScl_outF <- minmaxscl(df_RFM3$outF)


# 국가 관련 특이한 건들을 구분하고 나머지만으로 수치형 변수 분포 확인
ctv1 # 분기 규칙확인 = 분리 대상 특이 국가 리스트 확인

df_RFM4 <- df_RFM3[df_RFM3$Country %in% 
                     c("EIRE", "Israel", "Norway", "Portugal", "Spain", "United Arab Emirates", 
                       "United Kingdom", "Unspecified")
                   & df_RFM3$nCntry==1,]

plot(jitter(df_RFM4$lmedQuantity), jitter(df_RFM4$lcvUnitPrice),
     cex=0.5, col=rgb(mmScl_outF,0,1-mmScl_outF,0.5), pch=19)
abline(v=c(3.367), lty=3)
abline(h=-8.13, lty=3)


# 랜덤포리스트를 활용한 변수별 Anomaly Score 생성 영향도 파악
# install.packages("randomForest")
library(randomForest)
rfv1 <- randomForest(outF~., data=df_RFM3[,2:12], 
                     nodesize=2, ntree=3000)
varImpPlot(rfv1)
rfv1

plot(df_RFM3$outF, predict(rfv1, data=df_RFM3[,2:11]),
     xlim=c(0.5,0.75), ylim=c(0.5,0.75))
abline(a=0,b=1, lty=3)
lines(lowess(predict(rfv1, data=df_RFM3[,2:11])~df_RFM3$outF),
      col="red")

# 국가별 anomaly 분포 확인
asCntr <- aggregate(outF~Country, data=df_RFM3, FUN=mean)
barplot(asCntr$outF, names.arg=asCntr$Country, cex.names=0.5,
        ylab="mean anomaly score")
abline(h=min(asCntr$outF), lty=3)

boxplot(outF~Country, data=df_RFM3, cex.axis=0.5)


# 국가관련 변수를 제외한 후의 랜덤포리스트 - 변수중요도 확인
# 상대적 중요도 이므로 수치가 달라짐
rfv1 <- randomForest(outF~., data=df_RFM3[,c(2:9,12)], 
                     nodesize=2, ntree=3000)
varImpPlot(rfv1)
rfv1


plot(df_RFM3$lmedQuantity, jitter(df_RFM3$lcvUnitPrice), 
     cex=0.5, col=rgb(mmScl_outF,0,1-mmScl_outF,0.5), pch=19)
abline(v=c(3.367), lty=3)
abline(h=-8.13, lty=3)

# get list of anomaly
df_RFM4 <- cbind(df_RFM3, df_RFM2[,2:4])
df_RFM4a <- df_RFM4[df_RFM4$outF > 0.68,]
df_RFM4a[order(-df_RFM4a$outF),]


# 변수의 추가, 변환에 따라 anomaly 의미가 달라짐
# 대상을 좁혀가게 되면 anomaly case가 달라짐
# unsupervised ML의 기본적 특성
# (anomaly != bad) anomaly는 비정상(=특이) 패턴일 뿐
# 가치판단(=바람직한 경우인가)은 별개의 과제

#-------- end of script  [B]-----------#


