##-----------------------------------------------------------------------------

require(latticeExtra)
require(rootSolve)
data(turk0, package="alr3")

##-----------------------------------------------------------------------------

str(turk0)
xyplot(Gain~A, data=turk0, type=c("p","smooth"))

##-----------------------------------------------------------------------------

start <- list(th0=625, th1=800-625, th2=0.1)
xyplot(Gain~A, data=turk0)+
    layer(with(start, panel.curve(th0+th1*x/(th2+x))))

##-----------------------------------------------------------------------------

n0 <- nls(Gain~th0+th1*A/(th2+A), data=turk0,
          start=start)
summary(n0)

xyplot(Gain~A, data=turk0)+
    layer(with(as.list(coef(n0)),
               panel.curve(th0+th1*x/(th2+x), col=2)))

##-----------------------------------------------------------------------------

## Var(Y)
s2 <- summary(n0)$sigma^2

##-----------------------------------------------------------------------------

f <- function(theta, xx){
  with(as.list(theta), th0+th1*xx/(th2+xx))
}

## Matriz de derivadas parciais em theta (n x p).
gradient(f, x=coef(n0), xx=c(0.0, 0.2, 0.4))

pred <- data.frame(A=seq(0, 0.5, l=100))
## pred$fit <- f(theta=coef(n1), xx=pred$A)
pred$fit <- predict(n0, newdata=pred)
der <- gradient(f, x=coef(n0), xx=pred$A)
str(der)

F <- der
U <- chol(vcov(n0))

## Confidence.
pred$se <- sqrt(apply(F%*%t(U), 1,
                      function(x) sum(x^2)))

## Prediction.
pred$sey <- sqrt(s2)+sqrt(apply(F%*%t(U), 1,
                                function(x) sum(x^2)))

tval <- qt(p=c(lwr=0.025, upr=0.975), df=df.residual(n0))
me <- outer(pred$se, tval, "*")  ## confidence
me <- outer(pred$sey, tval, "*") ## prediction
pred <- cbind(pred, sweep(me, 1, pred$fit, "+"))
str(pred)

xyplot(Gain~A, data=turk0)+
    as.layer(xyplot(fit+lwr+upr~A, data=pred,
                    type="l", col=1, lwd=c(2,1,1)))

##-----------------------------------------------------------------------------
## Linear model as reference.

m0 <- lm(Gain~poly(A, degree=2), data=turk0)

## aux <- as.data.frame(predict(m0, newdata=pred, interval="confidence"))
aux <- as.data.frame(predict(m0, newdata=pred, interval="prediction"))

xyplot(Gain~A, data=turk0)+
    as.layer(xyplot(fit+lwr+upr~A, data=pred,
                    type="l", col=1, lwd=c(2,1,1)))+
        as.layer(xyplot(fit+lwr+upr~pred$A, data=aux,
                        type="l", col=2, lwd=c(2,1,1)))

##-----------------------------------------------------------------------------
