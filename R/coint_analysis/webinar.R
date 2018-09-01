# reproduzindo valores da planilha do Sergio Ferro
# referente ao webinar: https://www.youtube.com/watch?v=5_J95MzeeaE

library(BatchGetSymbols)
library(tidyverse)
library(lubridate)

tickers <- c("BBAS3", "CSAN3") %>% paste0(".SA")
start <- dmy(04042013)
end   <- dmy(02042014)

dataset <- BatchGetSymbols(
  tickers = tickers,
  first.date = start,
  last.date = end,
  thresh.bad.data = 0.01
)

dataset$df.control

prices <- dataset$df.tickers %>% 
  as.tibble() %>% 
  filter(complete.cases(.)) %>% 
  mutate( ticker = gsub("\\.SA","",ticker))

prices %>% 
  select(ticker, ref.date, price.close) %>% 
  spread(key = ticker, value=price.close ) %>% 
  arrange(ref.date) -> ativos

ativos %>% 
  ggplot(aes(x=BBAS3, y=CSAN3)) +
  geom_point() +
  theme_light()

regr <- lm(BBAS3~CSAN3, ativos)

regr %>% 
  summary()

tibble(
  ref.date = ativos$ref.date,
  coint    = regr$residuals
) %>% 
  ggplot(aes(x=ref.date, y=coint)) +
  geom_line() +
  theme_light()



df <- ur.df(regr$residuals, lags=1)

df@cval
df@teststat


# ter dickey-fuller acima de 95% -> OK
# em 3 de 6 períodos
# 

seq(100, 250, 20) %>% 
  


