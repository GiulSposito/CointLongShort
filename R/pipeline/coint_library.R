library(tseries)
library(forecast)
library(broom)
library(urca)

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

# modela uma ARIMA dos residuos
fitARIMA <- function(m, adf.res){
  m$residuals %>% 
    ts(frequency=20) %>%  # mensal: 20 pregoes
    auto.arima(stationary=adf.res$coint.result) %>% 
    return()
}

# extrai a ordem dos modelos
extractArma <- function(ar.model){
  ar.model$arma %>%
    t() %>%  as.tibble() %>% 
    set_names(c("AR","MA","sAR","sMA", "periods", "Diff","sDiff")) %>% 
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

# faz a analise de "beta rotation"
calcBetaRotation <- function(a,b,p,s=40){
  
  # prepara os dados para fazer o beta rotation
  dtfm <- a %>%
    select(ref.date, price.a = price.adjusted) %>%
    inner_join(b %>% select(ref.date, price.b = price.adjusted), by="ref.date") %>%
    arrange( ref.date ) 
  
  # intervalo em que o beta rotation sera feito 
  data.range <- dtfm %>% 
    slice((n()-p):n()) %>% # os 'p' periodos que cobrem a analise
    pull(ref.date)
  
  # aplica regressao para cada data, selecionando "s" periodos anteriores
  rotation <- lapply(data.range,
                     function(.loopdate, .rotSize){
                       dtfm %>% 
                         filter(ref.date <= .loopdate) %>% #
                         slice((n()-.rotSize):n()) %>%  # range do beta rotation (s periodos)
                         lm(price.a ~ price.b + ref.date, .) %>% 
                         tidy() %>% 
                         mutate(term=c("linear","angular","temporal")) %>% 
                         flatCoefTidy() %>% 
                         mutate( ref.date=.loopdate ) %>% 
                         return()
                     },
                     .rotSize=s) %>% bind_rows() 
  
  # monta tibble de resposta
  tibble(
    beta.rotation.ref.date = data.range[length(data.range)],
    beta.rotation.n    = s,
    beta.rotation.mu   = mean(rotation$angular.estimate, na.rm = T),
    beta.rotation.sd   = sd(rotation$angular.estimate, na.rm = T),
    beta.rotation.data = list(rotation)
  ) %>% 
    return()
  
}

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
    set_names(names(.))
  
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

execCointAnalysis <- function(.periods, .dtset){
  # calculos p/ cointegracao
  .dtset %>% 
    # modelo linear (a=f(b,t))
    mutate( 
      model.periods = .periods,
      model = map2(.x=prices.a, .y=prices.b, .f=fitLinModel, p=.periods),
      mdata = map(model, lmMetaData)
    ) %>% 
    # estrutura resultados da regressao
    mutate(
      model.coefs  = map(model,tidy),    # coeficientes obtidos do modelo
      model.glance    = map(model, glance), # qualidade do fit
      model.anova  = map(map(model,anova), tidy) # analise de variancia
    ) %>% 
    # dickeyFullerTest, half-life, channel size
    mutate(
      adf.test     = map(model,       dickeyFuller),
      adf.results  = map(adf.test,    tidyADF),
      flat.coefs   = map(model.coefs, flatCoefTidy),
      flat.anova   = map(model.anova, flatTidy),
      half.life    = map_dbl(model,   calcMeiaVida),
      corr         = map(model,       correlationAnalysis)
      # ,beta.rotation = map2(.x=prices.a, .y=prices.b, .f=calcBetaRotation, p=periods)
    ) %>% 
    # arima model
    mutate(
      arima        = map2(model, adf.results, fitARIMA),
      arima.coefs  = map(arima,  tidy), 
      arima.glance = map(arima,  glance),
      arima.arma   = map(arima,  extractArma)
    ) -> coint.analysis
  
  # cria um sumario
  coint.analysis %>% 
    unnest(mdata, adf.results, model.glance, flat.coefs) %>% 
    unnest(corr, arima.glance, arima.arma, .sep=".") %>% 
    select( ticker.a, ticker.b, model.periods, periods, adf, coint.level, coint.result,
            corr.z.fisher.conf.low, corr.z.fisher.estimate, corr.z.fisher.conf.high, 
            corr.z.fisher.eval.99,
            half.life, spread.size, linear.estimate, angular.estimate, temporal.estimate,
            ref.date.current, residual.current, z.score.current, sd,
            ref.date.last, residual.last, z.score.last 
    ) %>% 
    group_by(ticker.a, ticker.b) %>% 
    nest() %>% 
    set_names(c("ticker.a", "ticker.b", "coint.summary")) -> coint.summary
  
  # anexa o sumario e retorna
  coint.analysis %>% 
    left_join(coint.summary, by = c("ticker.a", "ticker.b")) %>% 
    return()
}

execCointAnSimplified <- function(.periods, .dtset){
  # calculos p/ cointegracao
  .dtset %>% 
    # modelo linear (a=f(b,t))
    mutate(
      model.periods = .periods,
      model = map2(.x=prices.a, .y=prices.b, .f=fitLinModel, p=.periods),
      mdata = map(model, lmMetaData)
    ) %>% 
    # estrutura resultados da regressao
    mutate(
      model.coefs  = map(model,tidy)    # coeficientes obtidos do modelo
    ) %>% 
    # dickeyFullerTest, half-life, channel size
    mutate(
      adf.test     = map(model,       dickeyFuller),
      adf.results  = map(adf.test,    tidyADF),
      flat.coefs   = map(model.coefs, flatCoefTidy),
      half.life    = map_dbl(model,   calcMeiaVida),
      corr         = map(model,       correlationAnalysis)
      # ,beta.rotation = map2(.x=prices.a, .y=prices.b, .f=calcBetaRotation, p=periods)
    ) %>% 
    # cria um sumario
    unnest(mdata, adf.results, flat.coefs) %>% 
    unnest(corr, .sep=".") %>% 
    select( ticker.a, ticker.b, model.periods, periods, adf, coint.level, coint.result,
            corr.z.fisher.conf.low, corr.z.fisher.estimate, corr.z.fisher.conf.high, 
            corr.z.fisher.eval.99,
            half.life, spread.size, linear.estimate, angular.estimate, temporal.estimate,
            ref.date.current, residual.current, z.score.current, sd,
            ref.date.last, residual.last, z.score.last 
    ) %>% 
    return()
}
