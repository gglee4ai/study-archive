## function to calcuate intervals (from Prof. Wames Marques Zeviani, 2015)
#nls.inteval내의 변수를 일반적인 변수를 가지도록 바꿀 것 
nls.intervals <- function(nlsobj, nlsfunc, newdata, level=0.95) {
  arglist <- as.list(coef(nlsobj))
  arglist$fl <- newdata
  fitt <- do.call(nlsfunc, arglist)  # get fitted value and gradient, to be fixed 
  grad <- attr(fitt, "gradient")  # F
  chvc <- chol(vcov(nlsobj))  # U
  lwr <- (1 - level) / 2
  upr <- 1 - lwr
  pred <- data.frame(fl=newdata, dTp=fitt)
  
  # add se and confidence intervals
  pred$sec <- sqrt(apply(grad %*% t(chvc), 1, function(x) sum(x^2))) 
  tvalc <- qt(p=c(clwr=lwr, cupr=upr), df=df.residual(nlsobj))  
  mec <- outer(pred$sec, tvalc, "*")  ## confidence
  pred <- cbind(pred, sweep(mec, 1, pred$dTp, "+"))
  
  # add sey and prediction intervals 
  sigma <- summary(nlsobj)$sigma  
  pred$sep <- sigma + sqrt(apply(grad %*% t(chvc), 1, function(x) sum(x^2))) 
  tvalp <- qt(p=c(plwr=lwr, pupr=upr), df=df.residual(nlsobj))  
  mep <- outer(pred$sep, tvalp, "*") 
  pred <- cbind(pred, sweep(mep, 1, pred$dTp, "+"))
}


## test nls.intervals
require(latticeExtra)

# test function
pf <- deriv3(~ th0 * fl ^ th1, c("th0", "th1"), function(fl, th0, th1) {})

# data setting
set.seed(11)
n <- 1000
data <- data.frame(fl=seq(1, 10, length.out=n))
data$dTp <- pf(data$fl, 30, 0.5) + rnorm(nrow(data), 0, 20)
str(data)
xyplot(dTp ~ fl, data=data, type=c("p","smooth"))

# nonlinear regression
m1 <- nls(dTp ~ pf(fl, th0, th1), data=data, start=c(th0=30, th1=0.5))
summary(m1)
plot(dTp ~ fl, data=data)
lines(data$fl, predict(m1), col="red")

# test nls.intervals function
xnew <- seq(min(data$fl), max(data$fl), length.out=200)
pred <- nls.intervals(m1, pf, newdata=xnew, level=0.95)
head(pred)

# plotting fitted, confidence intervals and prediction intervals
plot(dTp ~ fl, data=data)
lines(xnew, pred[,2], col=1)
lines(xnew, pred[,4], col=2)
lines(xnew, pred[,5], col=2)
lines(xnew, pred[,2], col=1)
lines(xnew, pred[,7], col=3)
lines(xnew, pred[,8], col=3)
