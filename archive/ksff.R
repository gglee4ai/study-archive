require(latticeExtra)
source("nls.intervals.R")


# y ~ CF * x ^ PO
PowerModel1 <- deriv3(~CF * predictor ^ PO, c("CF", "PO"), function(predictor, CF, PO) {})
PowerModel1Init <- function(mCall, LHS, data) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  lm.fit <- lm(log(xy[, "y"]) ~ log(xy[, "x"]))
  coefs <- coef(lm.fit)
  cf <- exp(coefs[1])
  po <- coefs[2]
  value <- c(cf, po)
  names(value) <- mCall[c("CF", "PO")]
  value
}
SSpow1 <- selfStart(PowerModel1, PowerModel1Init, c("CF", "PO"))



load("testdata.Rdata")
load("kslc.Rdata")
xyplot(dTm ~ fl, data=data, type=c("p","smooth"))

# nonlinear regression
m1 <- nls(dTm ~ SSpow1(fl, th0, th1), data=data)
summary(m1)
plot(dTm ~ fl, data=data)
lines(data$fl, predict(m1), col="red")

# test nls.intervals function
xnew <- seq(min(data$fl), max(data$fl), length.out=200)
pred <- nls.intervals(m1, newdata=data.frame(fl=xnew), level=0.95)
#pred <- nls.intervals(m1)
head(pred)

# plotting fitted, confidence intervals and prediction intervals
plot(dTm ~ fl, data=data)
lines(xnew, pred[,1], col=1)
lines(xnew, pred[,3], col=2, lty=2)
lines(xnew, pred[,4], col=2, lty=2)
lines(xnew, pred[,6], col=3, lty=2)
lines(xnew, pred[,7], col=3, lty=2)

