roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

roll2()


test <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

library(ggplot2)

qplot

x <- seq(-1, 1, by = 0.2)
x

y <- x^3

qplot(x, y)
