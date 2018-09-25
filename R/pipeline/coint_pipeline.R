# setup
library(tidyverse)
library(lubridate)
library(parallel)

# funcoes auxiliares do pipe
source("./R/pipeline/pipeline_helper.R") 
source("./R/pipeline/coint_library.R")

##### parametros de analise

# faixa de tempo para range de dados
data.end.date   <- now() - days(1) # now()
data.start.date <- now() - years(2)
periods    <- 100

# obtem os pares a serem analisados
pairs <- getCandidatePairs()

# price.table
price.data <- getPrices(pairs, data.start.date, data.end.date)

valid.pairs <- price.data$valid.pairs
price.table <- price.data$price.table %>% 
  as.tibble() %>% 
  arrange(ref.date)

target.date <- now() - months(5)
search.range <- 
  days(seq(1,60)) + target.date
  
# parallel preparation
cl <- makeCluster(4)
clusterEvalQ(cl,
             {
               library(tidyverse)
               library(lubridate)
               source("./R/pipeline/coint_library.R")
             })

# execute in parallel
parLapply(cl,search.range,
          searchOpCandidates,
          .pairs=valid.pairs,
          .price.table=price.table) -> coint.analysis

# stop clusters
stopCluster(cl)

# lapply(search.range,
#        searchOpCandidates,
#        .pairs=valid.pairs,
#        .price.table=price.table) -> coint.analysis

coint.analysis %>% 
  map("result") %>% 
  bind_rows() %>% 
  distinct() -> ops.candidatas

##### pre process

# # obtem precos
# price.data <- getPrices(pairs, start.date, end.date)
# 
# # atualiza pares candidatos válidos
# # e tabela de precos
# pairs <- price.data$valid.pairs
# prices <- price.data$price.table %>% 
#   as.tibble() %>% 
#   group_by(ticker) %>% 
#   arrange(ref.date) %>%
#   nest()
# 
# # monta os cenarios de analise
# pairs %>%
#   inner_join( prices %>% set_names(c("ticker.a","prices.a")), by = "ticker.a" ) %>% 
#   inner_join( prices %>% set_names(c("ticker.b","prices.b")), by = "ticker.b" ) %>% 
#   select(ticker.a, prices.a, ticker.b, prices.b) -> dtset
# 
# ##### calculos  
# 
# # testa cointegracao 100 periodos
# coint.now <- execCointAnSimplified(100,dtset)
# 
# # filtra operacoes candidatas
# coint.now %>%
#   filter(
#     coint.result == T,
#     corr.z.fisher.eval.99 == T,
#     abs(z.score.current) >= 2,
#     abs(z.score.last) < 2
#   ) -> ops.candidatas

if (nrow(ops.candidatas)>0) {

  # prepara testes das candidatas
  ops.candidatas %>% 
    select(ticker.a, ticker.b) %>% 
    left_join(dtset, by = c("ticker.a", "ticker.b")) -> teste.op.candidata
  
  # periodos de teste
  period.ranges <- seq(100,240,20) %>% c(250)
  
  # tabela de cointegracao
  lapply(period.ranges,
         execCointAnSimplified,
         .dtset=teste.op.candidata) %>% 
    bind_rows() -> coint.table
  
  # meata avaliacao da cointegracao
  coint.table %>% 
    group_by(ticker.a, ticker.b) %>% 
    # tira as medias
    mutate_if(function(x) (is.numeric(x)|is.logical(x)), mean, na.rm=T) %>% 
    distinct() %>% 
    ungroup() %>% 
    # so interessa alguns valroes
    select(
      ticker.a, ticker.b, 
      coint.level, coint.result, 
      corr.z.fisher.eval.99) %>% 
    # filtra valores minimos de cointegracao e teste de fisher
    filter( coint.result>=.75,
            corr.z.fisher.eval.99>=.75 ) -> coint.result
}

# contem as cointegracoes de 100 periodos que estao trigadas
ops.candidatas

# tabela de analise de multiperiodo das operacoes trigadas
coint.table

# consolidado de "go" da analise de multiperiodo
coint.result

# monta a operacao
coint.result %>% 
  select(ticker.a, ticker.b) %>% 
  left_join(dtset, by = c("ticker.a", "ticker.b")) %>% 
  lapply(100,
         execCointAnalysis,
         .dtset=.) %>% 
  bind_rows() %>% 
  unnest(coint.summary) %>% 
  mutate(
    start.date = ref.date.current + 1,
    end.date = ref.date.current + days(ceiling(half.life)),
    duration = ceiling(half.life)
  ) %>% 
  select(
    ticker.a,
    ticker.b,
    start.date,
    duration,
    end.date,
    angular.estimate,
    temporal.estimate,
    linear.estimate,
    model
    # adf, 
    # ref.date.current,
    # z.score.current,
    # residual.current,
    # sd,
    # half.life
  ) -> triggers

# obtem precos
new.pairs <- triggers %>% select(ticker.a, ticker.b)
new.price.data <- getPrices(.pairs = new.pairs, .start.date = min(triggers$start.date), .end.date = now()-months(1))

# atualiza pares candidatos válidos
# e tabela de precos
new.pairs <- new.price.data$valid.pairs
new.prices <- new.price.data$price.table %>% 
  as.tibble() %>% 
  group_by(ticker) %>% 
  arrange(ref.date) %>%
  nest()

# monta os cenarios de analise
new.pairs %>%
  inner_join( triggers ) %>% 
  inner_join( new.prices %>% set_names(c("ticker.a","prices.a")), by = "ticker.a" ) %>% 
  inner_join( new.prices %>% set_names(c("ticker.b","prices.b")), by = "ticker.b" ) -> backtest

a <- backtest[1,]$prices.a[[1]]
b <- backtest[1,]$prices.b[[1]]
start.date <- backtest[1,]$start.date
duration   <- backtest[1,]$duration
mymodel      <- backtest[1,]$model[[1]]
  
a %>%
  select(ref.date, price.a = price.adjusted) %>%
  filter(ref.date >= start.date) %>% 
  inner_join(b %>% select(ref.date, price.b = price.adjusted), by="ref.date") %>%
  filter(complete.cases(.)) %>% 
  arrange( ref.date ) %>% 
  slice(1:(2*duration)) %>% 
  mutate( price.a_hat = predict(mymodel, newdata=.),
          residuals   = price.a - price.a_hat ) -> pred

c(mymodel$residuals) %>% # , pred$residuals) %>% 
  plot(type="l")

class(model)

?predict

%>% 
  mutate( price.a_hat = predict(model,.) )
  

  
  mutate( price.a_hat = angular*price.b + temporal*ref.date + linear )


backtest$prices.b[[1]] %>% 
  filter(ref.date >= start.date) 


