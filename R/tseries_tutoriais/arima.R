# reproduzindo  tudorial em
# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

### ver essa aqui depois
#
# https://flare9xblog.com/tag/r-ornstein-uhlenbeck/
#
###

library(ggplot2)
library(forecast)
library(tseries)
library(lubridate)
library(tidyverse)

# ARIMA stands for auto-regressive integrated moving average and is specified by
# these three order parameters: (p, d, q). The process of fitting an ARIMA model is
# sometimes referred to as the Box-Jenkins method.

# An auto regressive (AR(p)) component is referring to the use of past values in the 
# regression equation for the series Y. The auto-regressive parameter p specifies the 
# number of lags used in the model. 

# The d represents the degree of differencing in the integrated (I(d)) component. 
# Differencing a series involves simply subtracting its current and previous values d times.
# Often, differencing is used to stabilize the series when the stationarity assumption is
# not met, which we will discuss below.

# A moving average (MA(q)) component represents the error of the model as a combination of 
# previous error terms et. The order q determines the number of terms to include in the model.


daily_data <- read.csv('./data/day.csv', header=TRUE, stringsAsFactors=FALSE) %>% 
  as.tibble() %>% 
  mutate(dteday=ymd(dteday))

ggplot(daily_data, aes(x=dteday, y=cnt)) +
  geom_line(color="blue") + 
  scale_x_date("month") +
  ylab("Daily Bike Checkout") +
  xlab("") +
  theme_light()


# limpando 'outliers'
count_ts = ts(daily_data$cnt)
daily_data %>% 
  mutate( clean_cnt = tsclean(count_ts) ) -> daily_data

# sao poucos os outliers elimintados
ggplot(daily_data, aes(x=dteday)) +
  geom_line(aes(y = cnt), color="blue")  + 
  geom_line(aes(y = clean_cnt), color="red")  + 
  scale_x_date("month") +
  ylab("Daily Bike Checkout") +
  xlab("") +
  theme_light()


## Moving Average

daily_data %>% 
  mutate ( 
    cnt_ma = ma(daily_data$clean_cnt, order=7), # using the clean count with no outliers
    cnt_ma30 = ma(daily_data$clean_cnt, order=30)
  ) -> daily_data

ggplot(daily_data, aes(x=dteday)) +
  geom_line(aes(x = dteday, y = clean_cnt, colour = "Counts")) +
  geom_line(aes(x = dteday, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(aes(x = dteday, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count') + 
  theme_light()

# Note that the moving average in this context is distinct from the M A(q) component in 
# the above ARIMA definition. Moving average M A(q) as part of the ARIMA framework refers 
# to error lags and combinations, whereas the summary statistic of moving average refers
# to a data smoothing technique.


## Decompose Your Data

# The building blocks of a time series analysis are seasonality, trend, and cycle. 
# These intuitive components capture the historical patterns in the series. Not every 
# series will have all three (or any) of these components, but if they are present, 
# deconstructing the series can help you understand its behavior and prepare a foundation 
# for building a forecasting model.
# 
# Seasonal component refers to fluctuations in the data related to calendar cycles. 
# For example, more people might be riding bikes in the summer and during warm weather, 
# and less during colder months. Usually, seasonality is fixed at some number; for instance, 
# quarter or month of the year.
# 
# Trend component is the overall pattern of the series: Is the number of bikes rented 
# increasing or decreasing over time?
# 
# Cycle component consists of decreasing or increasing patterns that are not seasonal. 
# Usually, trend and cycle components are grouped together. Trend-cycle component is 
# estimated using moving averages.

# Finally, part of the series that can't be attributed to seasonal, cycle, or trend
# components is referred to as residual or error.

# First, we calculate seasonal component of the data using stl()

count_ma <- ts(na.omit(daily_data$cnt_ma), frequency=1)
decomp <- stl(count_ma, s.window="periodic")
decomp2 <- decompose(count_ma)
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
plot(decomp2)
plot(deseasonal_cnt)
plot(count_ma)

## Stationarity

# Fitting an ARIMA model requires the series to be stationary. A series is said to be stationary
# when its mean, variance, and autocovariance are time invariant. This assumption makes intuitive
# sense: Since ARIMA uses previous lags of series to model its behavior, modeling stable series
# with consistent properties involves less uncertainty.

# The augmented Dickey-Fuller (ADF) test is a formal statistical test for stationarity.
# The null hypothesis assumes that the series is non-stationary. ADF procedure tests 
# whether the change in Y can be explained by lagged value and a linear trend. 
# If contribution of the lagged value to the change in Y is non-significant and there 
# is a presence of a trend component, the series is non-stationary and null hypothesis 
# will not be rejected.

adf.test(count_ma, alternative = "stationary")

# ADF test does not reject the null hypothesis of non-stationarity


# Autocorrelations and Choosing Model Order

# Autocorrelation plots (also known as ACF or the auto correlation function) are a useful
# visual tool in determining whether a series is stationary. These plots can also help to
# choose the order parameters for ARIMA model. If the series is correlated with its lags then,
# generally, there are some trend or seasonal components and therefore its statistical properties
# are not constant over time.

# ACF plots display correlation between a series and its lags. In addition to suggesting
# the order of differencing, ACF plots can help in determining the order of the M A (q) model.
# Partial autocorrelation plots (PACF), as the name suggests, display correlation between a
# variable and its lags that is not explained by previous lags. PACF plots are useful when
# determining the order of the AR(p) model.


Acf(count_ma, main='')

# R plots 95% significance boundaries as blue dotted lines. There are significant
# autocorrelations with many lags in our bike series, as shown by the ACF plot below.
# However, this could be due to carry-over correlation from the first or early lags,
# since the PACF plot only shows a spike at lags 1 and 7

Pacf(count_ma, main='')

# We can start with the order of d = 1 and re-evaluate whether further differencing is needed.

count_d1 <- diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# Next, spikes at particular lags of the differenced series can help inform
# the choice of p or q for our model

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

# There are significant auto correlations at lag 1 and 2 and beyond. 
# Partial correlation plots show a significant spike at lag 1 and 7. 
# This suggests that we might want to test models with AR or MA components of order 1, 2, or 7. 
# A spike at lag 7 might suggest that there is a seasonal pattern present, perhaps as 
# day of the week


# Evaluate and Iterate

fit <- auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit),lag.max = 45, main="Arima (1,1,1)")

# There is a clear pattern present in ACF/PACF and model residuals plots
# repeating at lag 7. This suggests that our model may be better off with a
# different specification, such as p = 7 or q = 7. 

fit2 = arima(deseasonal_cnt, order=c(1,1,7))

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
