# data handlers
library(tidyverse)
library(lubridate)

# yahoo provider
library(BatchGetSymbols)

# ativos
tickers <- c("QUAL3","RENT3") %>% paste0(".SA")

##### dados do Yahoo Finance

# get prices
prices <- BatchGetSymbols(
  tickers = tickers,  
  first.date = dmy("09102012"),
  thresh.bad.data = 0.001
)