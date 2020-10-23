########################################################
#R를 이용한 다층모형 분석###############################
########################################################

########################################################
#제4부 제4부 기타 고급통계기법과 다층모형###############
#03장 메타분석 (Meta-analysis) #########################
########################################################

# 라이브러리 구동 
library('metafor')
# 예시데이터 로드
# metafor 패키지에 들어 있는 데이터이며, 다음의 문헌을 참조
# Colditz et al., (1994). Efficacy of BCG vaccine in the prevention of tuberculosis: 
# meta-analysis of the published literature. JAMA, 271(9), 698-702.
data(dat.bcg)
dat.bcg

# 2X2 표를 기반으로 승산비(OR)를 효과크기로 함. 
# 연구의 차이에 따른 랜덤효과 반영 
result.ma0 <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg,
                  data=dat.bcg,
                  measure="OR",
                  method="REML")
summary(result.ma0)

# 상위수준(개별연구수준)의 독립변수를 투입모형-I 
result.ma1 <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg,
                  data=dat.bcg,
                  mods=dat.bcg[, "ablat"],
                  measure="OR",
                  method="REML")
summary(result.ma1)

# 상위수준(개별연구수준)의 독립변수를 투입모형-II 
dat.bcg$notrandom <- ifelse(dat.bcg$alloc == "random",0,1)
result.ma2 <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg,
                  data=dat.bcg,
                  mods=dat.bcg[, c("ablat","notrandom")],
                  measure="OR",
                  method="REML")
summary(result.ma2)



