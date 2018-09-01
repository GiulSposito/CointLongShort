# reproduzindo valores da planilha do Sergio Ferro
# referente ao webinar: https://www.youtube.com/watch?v=5_J95MzeeaE

library(BatchGetSymbols)
library(tidyverse)
library(lubridate)

tickers <- c("BBAS3", "CSAN3") %>% paste0(".SA")
start <- dmy(04052013)
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

regr <- lm(BBAS3~CSAN3+ref.date, ativos)

regr %>% 
  summary()


library(urca)
df <- ur.df(regr$residuals, lags=1)

df@cval
df@teststat

tibble(
  ref.date = ativos$ref.date,
  coint    = regr$residuals,
  ramp     = abs(coint/var)
) %>% 
  ggplot(aes(x=ref.date, y=coint)) +
  geom_line(aes(color=ramp), size=1) +
  geom_hline(yintercept = 2*var, color="red", size=1, linetype=2) +
  geom_hline(yintercept = -2*var, color="green", size=1, linetype=2) +
  scale_color_continuous(low="red",high="green") +
  theme_light()

?geom_hline

var <- sd(regr$residuals)

ggplot()

library(tseries)
adf.test(regr$residuals,k=1)

calcHalfLife(regr)
  


# ter dickey-fuller acima de 95% -> OK
# em 3 de 6 períodos
# 

seq(100, 250, 20) %>% 
  


