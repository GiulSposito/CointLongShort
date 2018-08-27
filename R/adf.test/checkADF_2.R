# analysers
library(zoo)
library(urca)
library(tseries)

# data handlers
library(tidyverse)
library(lubridate)

# yahoo provider
library(BatchGetSymbols)




# ativos
tickers <- c("ITUB4","ITSA4", ) %>% paste0(".SA")

##### dados do Yahoo Finance

# get prices
prices <- BatchGetSymbols(
  tickers = tickers,  
  first.date = dmy("01012016"),
  thresh.bad.data = 0.001
)

# only the tickers x price dataset
df.tickers <- prices$df.tickers %>% 
  arrange(ref.date) %>% 
  filter(complete.cases(.)) %>% 
  as.tibble()

# check the "adjusted" price
df.tickers %>% 
  filter(ticker=="ITSA4.SA") %>% 
  select(ref.date, price.close, price.adjusted) %>% 
  gather(price, value, -ref.date) %>% 
  arrange(ref.date) %>% 
  ggplot(aes(x=ref.date, y=value, group=price)) +
  geom_line(aes(color=price), size=1) +
  theme_light()


df.tickers %>% 
  select(ref.date, ticker, price.adjusted) %>% 
  spread(key = ticker, value = price.adjusted) %>% 
  lm(ITSA4.SA ~ ITUB4.SA, .) -> m1

df.tickers %>% 
  select(ref.date, ticker, price.adjusted) %>% 
  spread(key = ticker, value = price.adjusted) %>% 
  lm(ITUB4.SA ~ ITSA4.SA, .) -> m2

urca::ur.df(m1$residuals, "drift", lags = 1) %>%
  summary()

urca::ur.df(m2$residuals, "drift", lags = 1) %>%
  summary()

adf.test(m1$residuals,alternative="stationary",k=1)

adf.test(m2$residuals,alternative="stationary",k=1)


res <- tibble(
  y = m2$residuals
) %>% 
  mutate(
    y.nor = (y-mean(y))/sd(y),
    y.lag = lag(y),
    delta.y = c(NA,diff(y))
  ) 

lm(delta.y~y.lag,res) -> m3

-log(2)/summary(m3)$coefficients[2]

