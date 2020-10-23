library(MASS)
data(stormer)
attach(stormer)

model <- nls(Time ~ b * Viscosity / (Wt - c),
             start = list(b = 29, c = 2))
summary(model)

plot(Viscosity, Time, pch = 16, col = 1 + as.numeric(factor(Wt)))
xv <- 0:300
yv <- predict(model, list(Wt = 20 , Viscosity = xv))
lines(xv, yv, col = 2)
yv <- predict(model, list(Wt = 50 , Viscosity = xv))
lines(xv, yv, col = 3)
yv <- predict(model, list(Wt = 100 , Viscosity = xv))
lines(xv, yv, col = 4)


n <- 10000
bv <- numeric(n)
cv <- numeric(n)
for(i in 1:n) {
  ss <- sample(1:nrow(stormer), replace = T)
  y <- Time[ss]
  x1 <- Viscosity[ss]
  x2 <- Wt[ss]
  model <- nls(y ~ b * x1 / (x2 - c), start = list(b = 29, c = 2))
  bv[i] <- coef(model)[1]
  cv[i] <- coef(model)[2]
}
quantile(bv, c(0.025, 0.975))
quantile(cv, c(0.025, 0.975))

