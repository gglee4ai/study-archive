require(latticeExtra)

set.seed(11)
n <- 100

pf <- deriv3(~ th0 * xx ^ th1, c("th0", "th1"), function(xx, th0, th1) {})


x <- seq(1, 100, l=n)
y <- pf(x, 30, 0.5) + rnorm(length(x), 0, 50)

df <- data.frame(x, y)
str(df)
xyplot(y ~ x, data=df, type=c("p","smooth"))

n0 <- nls(y ~ pf(x, th0, th1), data=df, start=c(th0=30, th1=0.5))
summary(n0)
xyplot(y~x, data=df)+
    layer(with(as.list(coef(n0)),
               panel.curve(th0*x^th1, col=2)))


nlrint <- function(func, xnew, coefs, sigma, df, level=0.95) {
  pred <- data.frame(x=xnew)
  pred$fit <- func(xx=xnew, th0=coefs[1], th1=coefs[2])

  F <- attr(n0, "gradient") # gradiente avaliado no novo t
  U <- chol(vcov(n0))
  lwr <- (1 - level) / 2
  upr <- 1 - lwr
 
  # confidence
  pred$sec <- sqrt(apply(F %*% t(U), 1, function(x) sum(x^2))) 
  tvalc <- qt(p=c(clwr=lwr, cupr=upr), df=df.residual(n0))  
  mec <- outer(pred$sec, tvalc, "*")  ## confidence
  pred <- cbind(pred, sweep(mec, 1, pred$fit, "+"))
  # prediction 
  pred$sep <- sigma + sqrt(apply(F %*% t(U), 1, function(x) sum(x^2))) 
  tvalp <- qt(p=c(plwr=lwr, pupr=upr), df=df.residual(n0))  
  mep <- outer(pred$sep, tvalp, "*") 
  pred <- cbind(pred, sweep(mep, 1, pred$fit, "+"))
}
pred <- nlrint(pf, xnew=x, 
               coefs=coef(n0), 
               sigma=summary(n0)$sigma, 
               df=df.residual(n0))

##-----------------------------------------------------------------------------
## Linear model as reference.

m0 <- lm(y~poly(x, degree=3), data=df)

## aux <- as.data.frame(predict(m0, newdata=pred, interval="confidence"))
aux <- as.data.frame(predict(m0, newdata=pred, interval="prediction"))

xyplot(y~x, data=df)+
    as.layer(xyplot(fit+clwr+cupr~x, data=pred,
                    type="l", col=1, lwd=c(2,1,1))) +
    as.layer(xyplot(fit+plwr+pupr~pred$x, data=pred,
                    type="l", col=2, lwd=c(2,1,1))) +  
    as.layer(xyplot(fit+lwr+upr~pred$x, data=aux, 
                    type="l", col=3, lwd=c(2,1,1), lty=2))

##-----------------------------------------------------------------------------
pred