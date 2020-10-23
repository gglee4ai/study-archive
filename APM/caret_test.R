library(caret)

n = 1000
x = rnorm(n, mean = 3, sd = 2)
data <- data.frame(x, y = 2 * x + rnorm(n))

plot(y ~ x, data = data)

m1 <- train(y ~ x, method = "lm", data = data,
             trControl = trainControl(method  = "LOOCV"))

m1

m2 <- lm(y ~ x, data = data)
summary(m2)


output <- vector("double", n)

for (i in 1:n) {
  d <- data[-i, ]
  m <- lm(y ~ x, data = d)
  nd <- data[i, ]
  output[[i]] <- nd$y - predict(m, newdata = nd) 
}

output

plot(output)
hist(output)

hist(m2$residuals)

sqrt(sum(m2$residual^2) / m2$df)

sqrt(sum(output^2) / 1000)


require(splines)

loocv_tmp <- matrix(NA, nrow = n_train, ncol =)

transparentTheme(trans = .9)
featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))
