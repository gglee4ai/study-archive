## function to calcuate intervals (from Prof. Wames Marques Zeviani, 2015)
#nls.inteval내의 변수를 일반적인 변수를 가지도록 바꿀 것 
nls.intervals <- function(object, newdata, level=0.95, ...) {
  
  ## get right-hand side of formula
  form <- as.list(object$call$formula)  # get the formula
  lhs <- form[[2]]   
  rhs <- as.list(object$call$formula)[[3]]
  expr <- as.expression(rhs)
  
  ## all variables in model
  vars <- all.vars(expr)
  
  ## coefficients
  coefs <- coef(object)
  
  ## extract predictor variable    
  pred.name <- setdiff(vars, names(coefs))  
  
  ## take fitted values, if 'newdata' is missing
  if (missing(newdata)) {
    newdata <- eval(object$data)[pred.name]
    colnames(newdata) <- pred.name
  }
  
  ## check that 'newdata' has same name as predVAR
  if (names(newdata)[1] != pred.name) stop("newdata should have name '", pred.name, "'!")
    
  args <- c(as.list(newdata), as.list(coefs))
  fitt <- eval(expr, envir=args)
  pred <- cbind(fitt, newdata)
  colnames(pred)[1] <- as.character(lhs)
      
  grad <- attr(fitt, "gradient")  # F
  chvc <- chol(vcov(object))  # U
  lwr <- (1 - level) / 2
  upr <- 1 - lwr
  
  # add se and confidence intervals
  pred$sec <- sqrt(apply(grad %*% t(chvc), 1, function(x) sum(x^2))) 
  tvalc <- qt(p=c(clwr=lwr, cupr=upr), df=df.residual(object))  
  mec <- outer(pred$sec, tvalc, "*")
  pred <- cbind(pred, sweep(mec, 1, pred$dTp, "+"))
  
  # add sey and prediction intervals 
  sigma <- summary(object)$sigma  
  pred$sep <-   sqrt(apply(grad %*% t(chvc), 1, function(x) sum(x^2)) + sigma^2) 
  
  
  tvalp <- qt(p=c(plwr=lwr, pupr=upr), df=df.residual(object))  
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
data$dTp <- pf(data$fl, 30, 0.5) + rnorm(nrow(data), 0, 10)
str(data)
xyplot(dTp ~ fl, data=data, type=c("p","smooth"))

# nonlinear regression
m1 <- nls(dTp ~ pf(fl, th0, th1), data=data, start=c(th0=30, th1=0.5))
summary(m1)
plot(dTp ~ fl, data=data)
lines(data$fl, predict(m1), col="red")

# test nls.intervals function
xnew <- seq(min(data$fl), max(data$fl), length.out=200)
pred <- nls.intervals(m1, newdata=data.frame(fl=xnew), level=0.95)
head(pred)

# plotting fitted, confidence intervals and prediction intervals
plot(dTp ~ fl, data=data)
lines(xnew, pred[,1], col=1)
lines(xnew, pred[,4], col=2)
lines(xnew, pred[,5], col=2)
lines(xnew, pred[,7], col=3)
lines(xnew, pred[,8], col=3)
