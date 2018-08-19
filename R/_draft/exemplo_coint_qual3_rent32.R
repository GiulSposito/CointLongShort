# reproduz o passo a passo da planiha de cointegracao
# do Blog do DrNickel: https://drnickel.wordpress.com/2015/04/03/long-short-atraves-de-cointegracao-parte-3/

# yahoo provider
library(BatchGetSymbols)

# datahandle
library(tidyverse)
library(lubridate)

# stat ADF test
library(tseries)

# tidy statistics result
library(broom)

# ativos
tickers <- c("QUAL3","RENT3") %>% paste0(".SA")

# get prices
prices <- BatchGetSymbols(
  tickers = tickers,  
  first.date = dmy("09102012"),
  thresh.bad.data = 0.001
)

###### excel

library(xlsx)

xls <- read.xlsx("./R/_draft/exemplos-cointegracao-qual3_rent32.xlsx",2) 

xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=QUAL3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="QUAL3.SA",
          price.close = as.numeric(as.character(price.close)) ) -> qual

xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=RENT3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="RENT3.SA",
          price.close = as.numeric(as.character(price.close)) ) -> rent

prices <- list(
  df.tickers = bind_rows(qual,rent)
)

#######

# oberview
prices$df.tickers %>% 
  as.tibble() %>% 
  glimpse()


prices$df.tickers %>% 
  as.tibble() %>% 
  filter(ref.date <= dmy("10102014")) %>% 
  filter(complete.cases(.)) -> dtset
  # mutate(price.close = ifelse(ticker=="RENT3.SA", 3*price.close, price.close)) 

# plot prices
dtset %>% 
  select(ticker, ref.date, price.close)  %>% 
  ggplot(aes(x=ref.date, y=price.close, group=ticker, color=ticker)) +
  geom_line()

# fit a LM model with one day delta price
fitLagModel <- function(dtf){
  dtf %>% 
    select( price.close ) %>% 
    mutate( price.lag = lag(price.close,1),
            price.delta = price.close - price.lag ) %>%
    filter( complete.cases(.) ) %>% 
    lm( price.lag ~ price.delta, . ) %>% 
    return()
}

# applay df test from tseries package
applyADF <- function(dtf) {
  dtf %>%
    pull(price.close) %>% 
    adf.test(alternative="stationary", k=1) %>% 
    return()
}

applyDF <- function(dtf) {
  dtf %>% 
    pull(price.close) %>%
    urca::ur.df() -> dftest
  
  bind_cols(
    dftest@teststat %>% as.tibble() %>% set_names("t.stat"),
    dftest@cval %>% as.tibble()  
  ) %>% 
    return()
}
  
dtset %>%
  group_by(ticker) %>%
  nest() %>% 
  mutate ( lagModel = map(data, fitLagModel),
           lm.coefs  = map(lagModel,tidy),  
           lm.glance = map(lagModel, glance),
           lm.anova  = map(lagModel, anova),
           lm.anova  = map(lm.anova, tidy),
           adf       = map(data, applyADF ),
           adf.eval  = map(adf, tidy),
           df.eval   = map(data, applyDF)) %>% 
  select(ticker, adf.eval) %>% unnest(adf.eval) %>% arrange(ticker) 


dtset %>%
  select(ref.date, ticker, price.close) %>% 
  arrange(ref.date)
