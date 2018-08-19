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

##### dados do Yahoo Finance

# get prices
prices <- BatchGetSymbols(
  tickers = tickers,  
  first.date = dmy("09102012"),
  thresh.bad.data = 0.001
)

###### dados do excel para reproduzir exatamente mesmos valores da planilha

library(xlsx)
xls <- read.xlsx("./R/_draft/exemplos-cointegracao-qual3_rent32.xlsx",2) 

# separa os dados da QUAL3
xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=QUAL3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="QUAL3.SA",
          price.close = as.numeric(as.character(price.close)) ) -> qual

# separa os dados da RENT3
xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=RENT3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="RENT3.SA",
          price.close = as.numeric(as.character(price.close)) ) -> rent

# cria um dataset (igual ao do yahoo) fundindo os dois ativos
prices <- list(
  df.tickers = bind_rows(qual,rent)
)

#######

# overview dos dados
prices$df.tickers %>% 
  as.tibble() %>% 
  glimpse()

# pega tabela de precos no mesmo range do exercício
prices$df.tickers %>% 
  as.tibble() %>% 
  filter(ref.date <= dmy("10102014")) %>% 
  filter(complete.cases(.)) -> dtset

# plot prices
dtset %>% 
  select(ticker, ref.date, price.close)  %>% 
  ggplot(aes(x=ref.date, y=price.close, group=ticker, color=ticker)) +
  geom_line()

# auxiliary funcition
# fit a LM model with one day delta price
fitLagModel <- function(dtf){

  # pega os datasets de precos de um ticker
  dtf %>% 
    select( price.close ) %>% # so interessa o preco de fechametno
    mutate( price.lag = lag(price.close,1), # cria um "lag" de um dia
            price.delta = price.close - price.lag ) %>%  # delta entre fechamento e lag
    filter( complete.cases(.) ) %>% # elimina valores vazios
    lm( price.lag ~ price.delta, . ) %>% # fita o modelo
    return()
}

# apply df test from tseries package
# tenta aplicar o adf.test em uma sere de precos de fechamento
applyADF <- function(dtf) {
  dtf %>%
    pull(price.close) %>% 
    adf.test(alternative="stationary", k=1) %>% 
    return()
}

# aplica o urca::df nos precos de fechamento
applyDF <- function(dtf) {
  dtf %>% 
    pull(price.close) %>%
    urca::ur.df() -> dftest # volta uma estrutura de resposta
    
  # obtem algunas infos e monta um tibble (tidy)
  bind_cols(
    dftest@teststat %>% as.tibble() %>% set_names("t.stat"),
    dftest@cval %>% as.tibble()  
  ) %>% 
    return()
}

## testes de series estacionarias
## para cada ticker fita o modelo do linear do delta ~ lag
## tidyfica alguns parametros do modelo
## aplica o ADF test --> talve esteja errado e tenha que ser nos residuos e nao na serie
dtset %>%
  group_by(ticker) %>%
  nest() %>% 
  mutate ( lagModel = map(data, fitLagModel),
           lm.coefs  = map(lagModel,tidy),  
           lm.glance = map(lagModel, glance),
           lm.anova  = map(lagModel, anova),
           lm.anova  = map(lm.anova, tidy),
           adf       = map(data, applyADF ), # talvez tenha que ser nos residuos
           adf.eval  = map(adf, tidy),       # talvez tenha que ser nos residuos
           df.eval   = map(data, applyDF)) -> stat.test


# faz a composicao para detectar a cointegração
# fitando um modelo de um ativo contra outro
dtset %>% 
  spread(key=ticker, value=price.close) %>%
  lm(QUAL3.SA ~ RENT3.SA, .) -> coint

# avalia valores obtidos no modelo
coint %>% glance()
coint %>% anova() %>% tidy()
coint %>% tidy()

# monta um dataset para avaliar os residuos
dtset %>%
  filter( ticker=="QUAL3.SA" ) %>% 
  select(ticker, ref.date, price.close) %>% 
  bind_cols(
    tibble(
      predicted = coint$fitted.values,
      residuals = coint$residuals
    )    
  ) %>% 
  mutate( lagRes = lag(residuals,1),
          deltaRes = residuals - lagRes ) -> coint.ds

# fit dos residuos
coint.ds %>% 
  lm(lagRes ~ deltaRes, .) -> coint.lm

# avaliacao do fit
coint.lm %>% glance()
coint.lm %>% anova() %>% tidy()
coint.lm %>% tidy()

# plot

# banda +/- 2 SD
sd.res <- sd(coint.ds$residuals)

# residuos e banda
coint.ds %>% 
  ggplot(aes(x=ref.date, y=residuals)) + 
  geom_line() +
  geom_hline(yintercept =  2*sd.res) +
  geom_hline(yintercept = -2*sd.res)

