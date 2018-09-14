# setup
library(tidyverse)
library(lubridate)
library(broom)
library(urca)

# funcoes auxiliares do pipe
source("./R/pipeline/pipeline_helper.R") 
source("./R/pipeline/coint_library.R")

##### parametros de analise

# faixa de tempo para range de dados
end.date   <- ymd(20091231) # now()
start.date <- end.date - years(2)
periods    <- 100

# obtem os pares a serem analisados
pairs <- getCandidatePairs()

##### pre process

# obtem precos
price.data <- getPrices(pairs, start.date, end.date)

# atualiza pares candidatos válidos
# e tabela de precos
pairs <- price.data$valid.pairs
prices <- price.data$price.table %>% 
  as.tibble() %>% 
  group_by(ticker) %>% 
  arrange(ref.date) %>%
  nest()

# monta os cenarios de analise
pairs %>%
  inner_join( prices %>% set_names(c("ticker.a","prices.a")), by = "ticker.a" ) %>% 
  inner_join( prices %>% set_names(c("ticker.b","prices.b")), by = "ticker.b" ) %>% 
  select(ticker.a, prices.a, ticker.b, prices.b) -> dtset

##### calculos  

# calculos p/ 
dtset %>% 
  # modelo linear (a=f(b,t))
  mutate( 
    model = map2(.x=prices.a, .y=prices.b, .f=fitLinModel, p=periods),
    mdata = map(model, lmMetaData)
  ) %>% 
  # estrutura resultados da regressao
  mutate(
    model.coefs  = map(model,tidy),    # coeficientes obtidos do modelo
    model.glance = map(model, glance), # qualidade do fit
    model.anova  = map(map(model,anova), tidy) # analise de variancia
  ) %>% 
  # dickeyFullerTest, half-life, channel size
  mutate(
    adf.test    = map(model, dickeyFuller),
    adf.results = map(adf.test, tidyADF),
    flat.coefs  = map(model.coefs,flatCoefTidy),
    flat.anova  = map(model.anova, flatTidy),
    half.life   = map_dbl(model,calcMeiaVida),
    corr = map(model, correlationAnalysis)
    # ,beta.rotation = map2(.x=prices.a, .y=prices.b, .f=calcBetaRotation, p=periods)
  ) %>% 
  unnest(mdata, model.glance, flat.coefs, adf.results, corr, beta.rotation) -> x

x %>% 
  select( ticker.a, ticker.b, periods, adf, coint.level, coint.result,
          z.fisher.conf.low, z.fisher.estimate, z.fisher.conf.high, z.fisher.eval.99,
          half.life, spread.size, linear.estimate, angular.estimate, temporal.estimate,
          ref.date.current, residual.current, z.score.current, sd,
          ref.date.last, residual.last, z.score.last 
          # ,beta.rotation.mu, beta.rotation.sd
        ) %>% View()


