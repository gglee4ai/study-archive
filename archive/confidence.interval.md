Calculate confidence and prediction intervals in nls
================================================================================

```r
set.seed(11)
n <- 100
pf <- deriv3(~ a * x ^ p, c("a", "p"), function(x, a, p) {})
x <- seq(1, 100, l=n)
y <- pf(x, 30, 0.5) + rnorm(length(x), 0, 50)
plot(x, y)
df <- data.frame(x, y)
m2 <- nls(y ~ a * x ^ p, data=df,  start=list(a=30, p=0.5))
co <- coef(m2)
pred <- pf(x, co[1], co[2])
grad <- attr(pred, "gradient") # gradiente avaliado no novo t
chvc <- chol(vcov(m2))
sigma <- summary(m2)$sigma
se1 <- sqrt(apply(grad %*% t(chvc), 1, function(x) sum(x^2))) # erro padrão
se2 <- sigma + se1


yvals <- c(pred)+ outer(se2, qt(c(.5, .025,.975), df=df.residual(m2)))
yvals2 <- c(pred)+ outer(se1 * sqrt(n) * sqrt(1 + 1/n), qt(c(.5, .025,.975), df=df.residual(m2)))


#plot(x, y, ylim=c(0, 150))
plot(x, y)
lines(x, yvals[,1], col=1)
lines(x, yvals[,2], col=2)
lines(x, yvals[,3], col=2)
lines(x, yvals2[,1], col=1)
lines(x, yvals2[,2], col=3)
lines(x, yvals2[,3], col=3)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 
