# function para fazer o fit linear te dois ativos
# price.adjusted.A = f(price.adjusted.B, ref.date, periods)
fitLinModel <- function(a,b,p){
  a %>%
    select(ref.date, price.a = price.adjusted) %>%
    inner_join(b %>% select(ref.date, price.b = price.adjusted), by="ref.date") %>%
    arrange( ref.date ) %>% 
    slice((n()-p):n()) %>% 
    lm(price.a ~ price.b + ref.date, .) %>% 
    return()
}

# apply a simple DF test
dickeyFuller <- function(m){
  m$residuals %>% 
    ur.df(lags = 1) %>% 
    return()
}

# get basic data from de model
lmMetaData <- function(m, .desv.entrada=2){

  z.score <- scale(m$residuals)
  
  # cria um dataset de residuos
  rsd <- tibble(
    ref.date = m$model$ref.date,
    residual = m$residuals,
    z.score  = as.vector(z.score)
  )

  # ultimo valor do dataset
  rsd %>%
    filter(ref.date==max(ref.date)) -> rsd.current
  
  # penultimo
  rsd %>%
    filter(ref.date < rsd.current$ref.date) %>% 
    filter(ref.date==max(ref.date)) -> rsd.last
  
  # tibble de resposta
  tibble(
    periods      = length(m$residuals),
    sd           = sd(m$residuals),
    spread.size  = .desv.entrada * sd(m$residuals),
    ref.date.current = rsd.current$ref.date,
    residual.current = rsd.current$residual,
    z.score.current  = rsd.current$z.score,
    ref.date.last = rsd.last$ref.date,
    residual.last = rsd.last$residual,
    z.score.last  = rsd.last$z.score,
    z.score.center = attributes(z.score)$`scaled:center`,
    z.score.scale  = attributes(z.score)$`scaled:scale`,
    z.scores = list(rsd)
  ) %>% return()
}

# extrai os coeficientes 
# flatTidyCoefs <- function(tidyCoef){
# 
#   tidyCoef %>% 
#     select(term,estimate) %>% 
#     spread(term,estimate) %>% 
#     set_names(c("coef.lin", "coef.ang", "coef.tmp")) -> coefs
#   
#   tidyCoef %>% 
#     select(term,std.error) %>% 
#     spread(term,std.error) %>% 
#     set_names(c("std.error.lin", "std.error.ang", "std.error.tmp")) -> std.errors
# 
#   tidyCoef %>% 
#     select(term,statistic) %>% 
#     spread(term,statistic) %>% 
#     set_names(c("statistic.lin", "statistic.ang", "statistic.tmp")) -> statistics
#   
#   tidyCoef %>% 
#     select(term,p.value) %>% 
#     spread(term,p.value) %>% 
#     set_names(c("p.value.lin", "p.value.ang", "p.value.tmp")) -> p.values
#   
#   bind_cols(
#     coefs,
#     std.errors,
#     statistics,
#     p.values
#   ) %>% return()
#   
# }

# devolve o cálculo de meia vida tirado do tseries::adf.test
calcMeiaVida <- function(m, .desv.entrada=2){
  tibble(y = m$residuals) %>% 
    mutate(
      y.lag   = dplyr::lag(y,1),
      delta.y = c(NA,diff(y))
    ) %>% 
    filter( complete.cases(.) ) %>% 
    lm(delta.y ~ y.lag, data=.) %>% 
    summary() -> regress
  
  beta = -log(1+regress$coefficients[2])
  half.life=.desv.entrada/beta
  
  return(half.life)
}


# calcula a largura do canal (2*spread)
spreadSize <- function(m, desv.entrada=2, desv.saida=0){
  width = desv.entrada - desv.saida
  return(width*sd(m$residuals))
}

tidyADF <- function(adf){
  
  # extrai critical values
  cval <- adf@cval
  # resultado do df
  tstat <- adf@teststat %>% as.vector()
  
  # converte em o critical values em um tibble
  tibble(
    pct = colnames(cval), # coluna com porcentagens
    val = as.vector(cval) # coluna com valores
  ) %>%
    mutate(
      # transforma a porcentagem e numero
      pct = 1 - (pct %>% gsub("pct","",.) %>% as.numeric())/100,
      # testa qual passou
      pass = val <= tstat
    ) %>%
    filter( pass==F ) %>% # filtra as falhas
    pull( pct ) %>%
    c(0,.) %>% 
    max() -> coint.level # pega aultima falha

  # monta o tibble de resposta (1 linha)
  tibble(size = length(adf@y)) %>% # tamanho testado
    bind_cols(adf@teststat %>% as.tibble() %>% set_names(c("adf"))) %>% # resultado do df
    bind_cols(adf@cval %>% as.tibble()) %>% # Critical Values do test
    bind_cols(
      tibble(
        coint.level = coint.level,  # valor da %
        coint.result = coint.level >= .95 # teste de aprovacao
      )
    ) %>% 
    return()
  
}


flatTidy <- function(.anova){
  result <- map(
    .anova$term,  
    function(.t,.a){
      .a %>% 
        filter(term==.t) %>% 
        set_names(paste0(.t,".",names(.))) %>% 
        select(-1) %>% 
        return()
    }, .a=.anova)
  
  bind_cols(result) %>% 
    return()
}

flatCoefTidy <- function(.coefs){
  .coefs$term <- c("linear","angular","temporal")
  flatTidy(.coefs) %>% 
    return()
}

correlationAnalysis <- function(m){
  
  # obtem dados usado para fit do modelo
  prices <- m$model %>% 
    as.tibble()
  
  # estatisticas de correlacao direta
  corr <- cor.test(prices$price.b, prices$price.a, conf.level = 0.99) %>% 
    tidy() %>% 
    select(-method,-alternative) %>% 
    set_names(paste0("corr.",names(.)))
  
  # calculando correlacao de acordo com a planilha do ferro
  log.prices <- prices %>% 
    mutate(
      price.a = log(price.a/lag(price.a)),
      price.b = log(price.b/lag(price.b))
    ) %>% 
    filter(complete.cases(.))
  
  # correlacao dos logs dos pontos
  r.fisher <- cor.test(log.prices$price.b, log.prices$price.a, conf.level = 0.99) %>% 
    tidy() %>% 
    select(-method,-alternative) %>% 
    set_names(paste0("r.fisher.",names(.)))
  
  # transformando do r.space para o z.space (fisher)
  r <- r.fisher$r.fisher.estimate
  z <- 0.5 * log((1+r)/(1-r))
  
  # calculo dos limites
  n <- nrow(prices)
  sigma <- sqrt(1/(n-3))
  upper <- z + sigma * 2.58 # 99% | 1.96 -> 95%
  lower <- z - sigma * 2.58 # 99% | 1.96 -> 95%

  # montando tibble do z.fisher
  z.fisher <- tibble(
    estimate  = z,
    sigma     = sigma,
    parameter = n, 
    conf.low  = lower, # 99% | 1.96 -> 95%
    conf.high = upper, # 99% | 1.96 -> 95%
    eval.99   = z>=lower & z<=upper #,
    # expected  =  0.5 * log((1+pr)/(1-pr)) 
  ) %>% 
    set_names(paste0("z.fisher.",names(.)))
  
  bind_cols(corr, r.fisher, z.fisher) %>% 
    return()
  
}

# 
# # apply DickeyFuller to 8 distinct periods
# checkDickeyFuller_old <- function(m){
#   
#   # checa se tem pontos os suficientes
#   sample.size <- length(m$residuals)
#   
#   # vai testar o DF para 8 sequencias padrao
#   c(seq(100, 220, 20),250) %>% 
#     .[.<=sample.size] %>% # filtra se tem tamanho suficiente
#     map(function(n,m){
#       
#       # para cada # de amostras na sequencia pega as ultimas N
#       m$residuals %>% 
#         tail(n) %>% 
#         ur.df(lags = 1) -> df # aplica o DF
#       
#       # extrai critical values
#       cval <- df@cval
#       # resultado do df
#       tstat <- df@teststat %>% as.vector()
#       
#       # converte em o critical values em um tibble
#       tibble(
#         pct = colnames(cval), # coluna com porcentagens
#         val = as.vector(cval) # coluna com valores
#       ) %>% 
#         mutate(
#           # transforma a porcentagem e numero
#           pct = 1 - (pct %>% gsub("pct","",.) %>% as.numeric())/100,
#           # testa qual passou
#           pass = val <= tstat
#         ) %>% 
#         filter( pass==F ) %>% # filtra as falhas
#         pull( pct ) %>% 
#         max() -> coint.level # pega aultima falha
#       
#       # monta o tibble de resposta (1 linha)
#       tibble(size = n) %>% # tamanho testado
#         bind_cols(df@teststat %>% as.tibble() %>% set_names(c("df"))) %>% # resultado do df 
#         bind_cols(df@cval %>% as.tibble()) %>% # Critical Values do test
#         bind_cols(
#           tibble(
#             coint.level = coint.level,  # valor da % 
#             coint.result = coint.level >= .95, # teste de aprovacao
#             urdf = list(df) # o proprio DF
#           )
#         ) %>% 
#         return()
#     }, m=m) %>% 
#     bind_rows() %>% 
#     return()
# }
# 
# # aplica o ADF por um determinado periodo
# applyADF <- function(n,m){
#   
#   # pega os ultimos 
#   m$residuals %>% 
#     tail(n) %>% 
#     ur.df(lags = 1) -> df
#   
#   cval <- df@cval
#   tstat <- df@teststat %>% as.vector()
#   
#   tibble(
#     pct = colnames(cval),
#     val = as.vector(cval)
#   ) %>% 
#     mutate(
#       pct = 1 - (pct %>% gsub("pct","",.) %>% as.numeric())/100,
#       pass = val <= tstat
#     ) %>% 
#     filter( pass==F ) %>% 
#     pull( pct ) %>% 
#     max() -> coint.level
#   
#   tibble(size = n) %>%
#     bind_cols(df@teststat %>% as.tibble() %>% set_names(c("df"))) %>% 
#     bind_cols(df@cval %>% as.tibble()) %>% 
#     bind_cols(
#       tibble(
#         coint.level = coint.level,
#         coint.result = coint.level >= .95,
#         urdf = list(df)
#       )
#     ) %>% 
#     return()
# }
# 
# 
# # resume os testes de Dickey Fuller
# adfSummary <- function(adf.table){
#   
#   # ordena pelo tamanho
#   adf.table %>% 
#     arrange(size) %>% 
#     select(-urdf) -> adf.results
#   
#   # separa estatisticas do menor periodo
#   # dos ultimos 3
#   # de todos
#   tibble(
#     adfL1.adf  = adf.results[1, ]$df,
#     adfL1.lvl  = adf.results[1, ]$coint.level,
#     adfL1.rslt = adf.results[1, ]$coint.result,
#     adfL3.adf  = adf.results[3,]$df,
#     adfL3.rslt = mean(adf.results[1:3,]$coint.result),
#     adfL8.adf  = adf.results[nrow(adf.results),]$df,
#     adfL8.rslt = mean(adf.results$coint.result),
#     adfL8.per  = nrow(adf.results)
#   ) %>% 
#     return()
# }