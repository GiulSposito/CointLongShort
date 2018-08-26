library(tidyverse)
library(tseries)
library(urca)

# Lets create one randow walk Z(t)
set.seed(42) # the life, the universe and everything else
z <- rep(0,1000)
for(i in 2:1000) z[i] <- z[i-1] + rnorm(1)
plot(z, type="l")

# lets create x and y from a linear combination
x <- y <- rep(0,1000)
x <- .3*z + rnorm(1000)
y <- .6*z + rnorm(1000)

plot(x, type="l")
plot(y, type="l")

comb <- 2*x-y
plot(comb, type="l")
adf.test(comb)

lm(x~y, tibble(x=x,y=y))$residuals %>% 
  adf.test()
