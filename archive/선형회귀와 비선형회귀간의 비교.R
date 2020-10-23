
선형회귀와 비선형회귀간의 비교
```{r}
fl <- function(x) { # linear function
  2 + 0.5 * x  + rnorm(length(x), 0, 1.5)
}

fp <- function(x) { # power function
  2 * x ^ 0.5  * exp(rnorm(length(x), 0, 0.3))
}
set.seed(100)
xv <- seq(0, 10, 0.1)
yl <- fl(xv)
yp <- fp(xv)
```

선형회귀 테스트
```{r}
plot(xv, yl, xlim=c(0, 10), ylim=c(-2, 12))
ml <- lm(yl ~ xv)
ci <- predict(ml, interval=c("confidence"))
pi <- predict(ml, interval=c("prediction"))
abline(ml, col="red")
matlines(xv, ci[,2:3], col="blue", lty=2)
matlines(xv, pi[,2:3], col="green", lty=2)

```

power function을 log 변환후 선형회귀로 해석
```{r}

xv <- xv[xv > 0]
yp <- fp(xv)
xvl <- log(xv)
ypl <- log(yp)
plot(xvl, ypl)
mpl <- lm(ypl ~ xvl)
cpl <- predict(mpl, interval=c("confidence"))
ppl <- predict(mpl, interval=c("prediction"))
abline(mpl, col="red")
matlines(xvl, cpl[,2:3], col="blue", lty=2)
matlines(xvl, ppl[,2:3], col="green", lty=2)


xvle <- exp(xvl)
yple <- exp(ypl)
cple <- exp(cpl)
pple <- exp(ppl)
plot(xvle, yple, xlim=c(0, 10), ylim=c(-2, 12))
lines(xvle, cple[,1], col="red")
lines(xvle, cple[,2], col="blue", lty=2)
lines(xvle, cple[,3], col="blue", lty=2)
lines(xvle, pple[,2], col="green", lty=2)
lines(xvle, pple[,3], col="green", lty=2)

#power function을 비선형회귀로 해석
#plot(xv, yp, xlim=c(0, 10), ylim=c(-2, 12))
mp <- nls(yp ~ a * (xv ^ b), start=list(a=1, b=1))
cp <- confint(mp)
lines(xv, cp, col="red")
lines(xv, cp[1,1] * xv ^ cp[2,1], col="orange", lty=2)
lines(xv, cp[1,2] * xv ^ cp[2,2], col="orange", lty=2)

```

