library(tidyverse)
library(lubridate)
library(BatchGetSymbols)
library(broom)
library(tseries)
library(urca)
source("./R/coint_analysis/coint_lib.R")

# range da analise
start.date <- ymd(20050101)
end.date   <- ymd(20091231)

# pair / test.cases
test.cases <- readRDS("./data/pair_analysis.rds") %>% 
  mutate( ticker.a = paste0(ticker.a, ".SA"), # precisa manter a nomenclatura do yahoo
          ticker.b = paste0(ticker.b, ".SA"))

# tickers to get data
tickers <- c(test.cases$ticker.a, test.cases$ticker.b) %>% # todos os ativos dos pares
  unique() # nao repetido
  

# obtem precos
prices <- BatchGetSymbols(
    tickers = tickers,       # tickers da analise
    first.date = start.date, # data de inicio
    last.date =  end.date,   # data de fim
    thresh.bad.data = 0.01   # considerar serie com pelo menos 99% de cobertura
  )

# da uma olhada no que foi baixado
prices$df.control

# vamos tirar do test.cases tickers nao baixados
prices$df.control %>% 
  filter(download.status=="OK", threshold.decision=="KEEP") %>% 
  pull(ticker) %>% 
  as.character() -> valid.tickers

# mantem somente casos de testes que tem dados
test.cases %>%
  filter( ticker.a %in% valid.tickers,
          ticker.b %in% valid.tickers ) -> test.cases

# dando uma olhada no que está no price.adjusted
prices <- prices$df.tickers %>% 
  as.tibble() %>% # please! 
  arrange(ref.date)

# clean up
rm(tickers); gc();

# olha algum ticker
prices %>% 
  filter( ticker %in% c("ITUB4.SA")) %>% 
  select( ticker, ref.date, price.close, price.adjusted ) %>% 
  ggplot(aes(group=ticker, x=ref.date)) +
  geom_line(aes(y=price.close), color="red") +
  geom_line(aes(y=price.adjusted), color="blue") +
  theme_light()
  
## o preco esta ajustado para o futuro, nao para a janela :|
## vamos fazer o exercicio com ele ajustado

# ultimo ano de "treinamento"
# antes do ultimo quadrimestre
prices.training <- prices %>% 
  filter(ref.date <  ymd(20090901),
         ref.date >= ymd(20080901))

# ultimo quadrimestre 
prices.validation <- prices %>% 
  filter( ref.date >= ymd(20090901),
          ref.date  < ymd(20100101))


# trading pairs 
test.cases %>% 
  select( ticker.a, ticker.b ) -> pairs

# nest price by tickers
prices.training %>% 
  group_by(ticker) %>% 
  nest() -> nested.prices

# put toghether case and data
pairs %>%
  inner_join( nested.prices %>% set_names(c("ticker.a","prices.a")), by = "ticker.a" ) %>% 
  inner_join( nested.prices %>% set_names(c("ticker.b","prices.b")), by = "ticker.b" ) %>% 
  select(ticker.a, prices.a, ticker.b, prices.b) -> dtset


# calculos
dtset %>% 
  # modelo linear (a=f(b))
  mutate( model = map2(.x=prices.a, .y=prices.b, .f=fitLinModel) ) %>% 
  # estrutura resultados da regressao
  mutate(
    model.coefs  = map(model,tidy),    # coeficientes obtidos do modelo
    model.glance = map(model, glance), # qualidade do fit
    model.anova  = map(map(model,anova), tidy) # analise de variancia
  ) %>% 
  mutate(
    adf.test  = map(model, testADF),  # Teste ADF do pacote tseries
    urdf.test = map(model, testURDF)  # Teste de DF do pacote urca
  ) %>%
  mutate(
    spread.size    = map(model,spreadSize) %>% unlist(),       # tamanho do spread 2*sd dos residuos
    half.life      = map(model, calcHalfLife) %>% unlist(),    # half-life calculado "na mao"
    half.life.urdf = map(urdf.test, urdfHalfLife) %>% unlist() # half-life calculado a partir do urca::ur.df
  ) %>% 
  mutate(
    urdf = map(urdf.test, function(df) {df@teststat %>% as.tibble()})
  ) %>% 
  unnest(adf.test, .drop=F) %>% 
  unnest(urdf, .drop=F) %>% 
  select(ticker.a, ticker.b, spread.size, half.life, half.life.urdf, adf.statistic, adf.p.value, tau2, phi1)



