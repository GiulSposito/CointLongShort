# setup
library(tidyverse)
library(lubridate)

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

# testa cointegracao 100 periodos
coint.now <- execCointAnSimplified(100,dtset)

# filtra operacoes candidatas
coint.now %>%
  filter(
    coint.result == T,
    corr.z.fisher.eval.99 == T,
    abs(z.score.current) >= 2,
    abs(z.score.last) < 2
  ) -> ops.candidatas

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
  
  coint.table %>% 
    group_by(ticker.a, ticker.b) %>% 
    mutate_if(function(x) (is.numeric(x)|is.logical(x)), mean, na.rm=T) %>% 
    distinct() %>% 
    ungroup() %>% 
    select(
      ticker.a, ticker.b, 
      coint.level, coint.result, 
      corr.z.fisher.eval.99) -> coint.result
}
