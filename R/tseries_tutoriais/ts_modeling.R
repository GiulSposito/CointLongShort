# A Complete Tutorial on Time Series Modeling in R 
# https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/

# Stationary Definition
#
# 1. the mean of the series should not be a function of time rather should be a constant
# 2. the variance of the series should not be a function of time (homoscedasticity)
# 3. the covariance of the i_th term and the (i+m)_th term should not be a function of time ("the pace")
#
# Unless your time series is stationary, you cannot build a time series model!!

# Random walks:
#
# the next position of a value is only dependent of the current position
# x(t) = x(t-1) + Er(t)
# 
# expaning:
# x(t) => x(0) + sum(Er(1)...Er(t))
#
# is the mean constant?
# E[x(t)] = E[x(0)] + sum(E[er(1)]...E[ef(t)])
#
# The "expectation" of a error is zero if error is random
# E[x(t)] = E[x(0)]
#
# Is the variance constant?
#  Var[x(t)] = Var[x(0)] + sum(Var[er(1)]...Var[ef(t)])
#  Var[x(t)] = t * Var(error) ==> Time Dependent # not stationary

# Rho Coefficient
#
# x(t) = Rho * x(t-1) + Er(t)
#
# Rho = 1   ==> not stationary
# Rho = 0.1 ==> almost stationary
# Rho = 0   ==> perfectly stationary ==> x(t) = Er(t)
#
# Thaking the Expectation
# E[x(t)] = Rho * E[x(t-1)]
# The next X (or at time point t) is being pulled down to Rho * Last value of X
# if X is 1 no force can pull the X down in the next step
#
# Dickey Fuller
#
# X(t) = Rho * X(t-1) + Er(t)
# X(t)-X(t-1) = (Rho-1)X(t-1) + Er(t) ==> delta.x = (Rho-1)lag.x + Er(t)
# we have to test if (Rho-1) is significantly different than zero
# if null hypothesis gets rejected ==> stationary


### Test Case

# data
data(AirPassengers)
class(AirPassengers)

# check
str(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)

# plot
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)), col="red")

# months
cycle(AirPassengers)

# accumulated
plot(aggregate(AirPassengers, FUN = mean))

# by month
boxplot(AirPassengers~cycle(AirPassengers))

# original data
plot(AirPassengers) # non stationary - non constant variation
plot(log(AirPassengers)) # constant variation 
plot(diff(log(AirPassengers))) # remove mean --> stationary?

# dickey-fuller
library(tseries)
library(broom)
library(urca)
library(magrittr)

# adf.test não esta funcionando?
ht <- adf.test(AirPassengers,c("stationary"),k=1) 
ht <- adf.test(diff(log(AirPassengers)),c("stationary"),k=1) 

# ur.df está!!
ht <- ur.df(AirPassengers, lags=1)
ht <- ur.df(diff(log(AirPassengers)), lags=1) 

# Phillips-Perron unit root test
ht <- ur.pp(AirPassengers,type="Z-tau",model="constant")
ht <- ur.pp(AirPassengers,type="Z-alpha",model="constant")

?adf.test

# ARIMA MODELS
acf(AirPassengers) # long decais from auto-covariation
acf(diff(log(AirPassengers)))   # q = 1 -> previous from inverion signal
pacf(AirPassengers) # partial autocorrelations
pacf(diff(log(AirPassengers)))  # p = 1

fit <- arima(log(AirPassengers), c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
pred <- exp(1) ^ predict(fit, n.ahead = 12*10)$pred
ts.plot(AirPassengers, pred, lty=c(1,3))
