
작업용 Dataframe 구축 및 컬럼 삭제
```{r}
ttsdb <- TTSDB # 작업용 dataframe
ttsdb$Epri_Plant <- NULL # 불필요 정보
ttsdb$OrigHeatID <- NULL # HEAT_ID 사용
ttsdb$EFP_HRS <- NULL    # Hrs_used 사용 
ttsdb$F1_RATE  <- NULL   # Flux_used 사용
ttsdb$UTT30.1  <- NULL   # UTT30 사용
ttsdb$ITT30.1 <- NULL    # ITT30 사용
ttsdb$DTT30.1 <- NULL    # DTT30 사용
ttsdb$DTT30.2 <- NULL    # DTT30 사용
ttsdb$MN <- NULL         # MnGeneric 사용
ttsdb$Orig.VESSEL_MFG <- NULL   # VESSEL_MFG 사용
ttsdb$Reference <-NULL   # 불필요 정보

# Eason, Data not used in 1-06 calibration or validation
# irradiated in both BWR & PWR at diffrent flux
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "PMON01" & ttsdb$Hrs_used==78366),]
# unusally high flux(highest inthe Database), well above expected application range
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "PBR_01" & ttsdb$Hrs_used==18290),] 
# irradiated in 2 PWRs at different Tc
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WTP301" & ttsdb$Hrs_used==121107),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WZN101" & ttsdb$Hrs_used==125829),]
# Chauvenet outliers in previous fitting efforts
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WCK101" & ttsdb$Hrs_used==80556),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WSQ201" & ttsdb$Hrs_used==25528),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WCK101" & ttsdb$Hrs_used==11111),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WTM201" & ttsdb$Hrs_used==46608),]
# Chauvenet outliers in 1-06 fitting efforts
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "FGIN02" & ttsdb$Hrs_used==60278),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WCL101" & ttsdb$Hrs_used==9222),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WFA201" & ttsdb$Hrs_used==56389),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WFA201" & ttsdb$Hrs_used==116062),]

names(ttsdb)
ttsdb[ttsdb == -999] <- NA #-999로 입력된 숫자를 NA로 변경
summary(ttsdb)
```

```{r}
nrg6551 <- subset(ttsdb, Dataset=="NRG6551")
pairs(nrg6551[c("DTT30","CU", "NI", "P", "Tc", "CAP_F1", "Hrs_used")])
summary(nrg6551)
```


#CR3391DB$fl <- CR3391DB$fluence / 1e19
#CR3391DB$dTp <- with(CR3391DB, CR3391(form, Cu, Ni, fluence))  # predicted TTS
#CR3391DB$res <- with(CR3391DB, dTm - dTp)  # residual
CR3391DBp <- subset(CR3391DB, form=="P")
CR3391DBw <- subset(CR3391DB, form=="W")
```

Figure HEDL-3, 4, 5 재현
```{r}
with(CR3391DB, plot(Cu, res, pch=20+as.numeric(form), bg=4-as.numeric(form),   
                    xlim=c(0,0.4), ylim=c(-55,75),  
                    xlab="Cu, wt%", ylab="Residuals (Obs.-Calc.), degF"))
abline(0,0)
with(CR3391DB, plot(Ni, res, pch=20+as.numeric(form), bg=4-as.numeric(form), 
                    xlim=c(0,1.1), ylim=c(-55,75),
                    xlab="Ni, wt%", ylab="Residuals (Obs.-Calc.), degF"))
abline(0,0)
with(CR3391DB, plot(fluence, res, pch=20+as.numeric(form), bg=4-as.numeric(form), 
                    xlim=c(4e17, 1e20), ylim=c(-55,75),
                    xlab="Fluence (E>1MeV), n/cm2", ylab="Residuals (Obs.-Calc.), degF",
                    log="x"))
abline(0,0)
```

Plate model fitting (sd=17.2F)
```{r}
mp <- nls(dTm ~ (x6 + x7 * Cu + x8 * Cu * tanh(x9 * Ni / Cu)) * (fluence / 1e19) ^ (x10 + x11 * log(fluence / 1e19)), start=list(x6=-38, x7=555, x8=480, x9= 0.3, x10=0.2, x11=-0.04), data=CR3391DBp)
print(mp)
summary(mp)
plot(CR3391DBp$dTm, predict(mp), xlim=c(0,300), ylim=c(0,300))
abline(0,1, col="red")
```


R을 이용한 PR-EDB 자료 분석
========================================================
  
  
  원본 파일을 읽어옴
```{r}
TTSDB <- read.csv("~/TTSDatabase8-04R1.csv", header=TRUE)
summary(TTSDB)
```

작업용 Dataframe 구축 및 컬럼 삭제
```{r}
ttsdb <- TTSDB # 작업용 dataframe
ttsdb$Epri_Plant <- NULL # 불필요 정보
ttsdb$OrigHeatID <- NULL # HEAT_ID 사용
ttsdb$EFP_HRS <- NULL    # Hrs_used 사용 
ttsdb$F1_RATE  <- NULL   # Flux_used 사용
ttsdb$UTT30.1  <- NULL   # UTT30 사용
ttsdb$ITT30.1 <- NULL    # ITT30 사용
ttsdb$DTT30.1 <- NULL    # DTT30 사용
ttsdb$DTT30.2 <- NULL    # DTT30 사용
ttsdb$MN <- NULL         # MnGeneric 사용
ttsdb$Orig.VESSEL_MFG <- NULL   # VESSEL_MFG 사용
ttsdb$Reference <-NULL   # 불필요 정보

# Eason, Data not used in 1-06 calibration or validation
# irradiated in both BWR & PWR at diffrent flux
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "PMON01" & ttsdb$Hrs_used==78366),]
# unusally high flux(highest inthe Database), well above expected application range
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "PBR_01" & ttsdb$Hrs_used==18290),] 
# irradiated in 2 PWRs at different Tc
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WTP301" & ttsdb$Hrs_used==121107),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WZN101" & ttsdb$Hrs_used==125829),]
# Chauvenet outliers in previous fitting efforts
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WCK101" & ttsdb$Hrs_used==80556),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WSQ201" & ttsdb$Hrs_used==25528),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WCK101" & ttsdb$Hrs_used==11111),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WTM201" & ttsdb$Hrs_used==46608),]
# Chauvenet outliers in 1-06 fitting efforts
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "FGIN02" & ttsdb$Hrs_used==60278),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WCL101" & ttsdb$Hrs_used==9222),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WFA201" & ttsdb$Hrs_used==56389),]
ttsdb <- ttsdb[!(ttsdb$HEAT_ID == "WFA201" & ttsdb$Hrs_used==116062),]

names(ttsdb)
ttsdb[ttsdb == -999] <- NA #-999로 입력된 숫자를 NA로 변경
summary(ttsdb)
```

```{r}
nrg6551 <- subset(ttsdb, Dataset=="NRG6551")
pairs(nrg6551[c("DTT30","CU", "NI", "P", "Tc", "CAP_F1", "Hrs_used")])
summary(nrg6551)
```

NRG6501 TTS model 정의
```{r}
TTS <- function(form, Cu, Ni, P, Tc, fluence, ti) {
  if (form == "weld") {
    A = 1.10e-7
  } else if (form == "plate") { 
    A = 1.24e-7  
  } else if (form == "forging") {
    A = 8.98e-8
  }  
  f = (fluence / 1e19) ^ (0.4449 + 0.0597 * log10(fluence / 1e19))
  SMD = A * exp(1.906e4 / (Tc + 460)) * (1 + 57.7 * P) * f
  
  if (form == "weld") {
    B = 209
  } else if (form == "plate") { 
    B = 172
  } else if (form == "forging") {
    B = 135
  }  
  g = 0.5 + 0.5 * tanh((log10(fluence + 5.48e12 * ti) - 18.290) / 0.6)
  if (Cu <= 0.072) {
    h = 0.0
  } else if (0.072 < Cu & Cu < 0.3) {
    h = (Cu - 0.072) ^ 0.678
  } else if (Cu >= 0.3) {
    h = 0.367
  }
  CRP = B * (1 + 2.56 * Ni ^ 1.358) * h * g
  
  list(tts=SMD+CRP, smd=SMD, crp=CRP)
}
TTS(form="plate", Cu=0.2, Ni=0.5, P=0.01, Tc=550, fluence=2.5e18, ti=10000)
TTS(form="forging", Cu=0.04, Ni=0.7, P=0.01, Tc=530, fluence=1.5e19, ti=75000)
TTS(form="weld", Cu=0.3, Ni=0.6, P=0.015, Tc=550, fluence=1.5e19, ti=75000)
```



```{r}
nz <- list(form="plate",
           Cu=0.14,
           Ni=0.6,
           P=0.11,
           Tc=545,
           fluence=7.50e18,
           ti=31861)
nz$fluence <- 10 ^ seq(16, 20, length.out=200) 
res = do.call(TTS, nz)
plot(nz$fluence, res$tts, xlab="Fluence, n/cm2", ylab="TTS, degF", log="x", type="l", ylim=c(-50,250))
matlines(nz$fluence, cbind(res$smd, res$crp), col=c("blue", "red"))

```



CR6551 <- function(form, Cu, Ni, P, Tc, fluence, ti) {
  form <- tolower(as.character(form))
  a.value <- c(f=8.98e-8, p=1.24e-7, w=1.10e-7)
  A <- a.value[form]
  f = (fluence / 1e19) ^ (0.4449 + 0.0597 * log10(fluence / 1e19))
  smd = A * exp(1.906e4 / (Tc + 460)) * (1 + 57.7 * P) * f  
  
  b.value <- c(w=209, p=172, f=135) 
  B <- b.value[form]
  g = 0.5 + 0.5 * tanh((log10(fluence + 5.48e12 * ti) - 18.290) / 0.6)
  h <-  ifelse(Cu <= 0.072, 0.0,
               ifelse(Cu >= 0.3, 0.367, (Cu-0.072) ^ 0.678))
  crp = B * (1 + 2.56 * Ni ^ 1.358) * h * g
  
  smd + crp
  # list(tts=smd+crp, smd=smd, crp=crp)
}
# Example
CR6551(form="P", Cu=0.2, Ni=0.5, P=0.01, Tc=550, fluence=2.5e18, ti=10000) # 68.3
CR6551(form="F", Cu=0.04, Ni=0.7, P=0.01, Tc=530, fluence=1.5e19, ti=75000) # 39.1
CR6551(form="W", Cu=0.3, Ni=0.6, P=0.015, Tc=550, fluence=1.5e19, ti=75000) # 205.2



Build a working dataframe and remove unecessary columns
```{r}
TTSDB <- TTSDBALL # 작업용 dataframe
TTSDB$Epri_Plant <- NULL # 불필요 정보
TTSDB$OrigHeatID <- NULL # HEAT_ID 사용
TTSDB$FLUX_TYPE <- NULL # HEAT_ID 사용
TTSDB$EFP_HRS <- NULL    # Hrs_used 사용 
TTSDB$F1_RATE  <- NULL   # Flux_used 사용
TTSDB$UTT30.1  <- NULL   # UTT30 사용
TTSDB$ITT30.1 <- NULL    # ITT30 사용
TTSDB$DTT30.1 <- NULL    # DTT30 사용
TTSDB$DTT30.2 <- NULL    # DTT30 사용
TTSDB$MN <- NULL         # MnGeneric 사용
TTSDB$Orig.VESSEL_MFG <- NULL   # VESSEL_MFG 사용
TTSDB$Reference <-NULL   # 불필요 정보
names(TTSDB)
```


