# function para fazer o fit linear te dois ativos
# price.adjusted.A = f(price.adjusted.B)
fitLinModel <- function(a,b){
  a %>%
    select(ref.date, price.a = price.adjusted) %>%
    inner_join(b %>% select(ref.date, price.b = price.adjusted), by="ref.date") %>% 
    lm(price.a ~ price.b, .) %>% 
    return()
}

# aplica o teste de Dickey-Fuller
testADF <- function(m) suppressWarnings({
  m$residuals %>% 
    adf.test(.,"stationary",k=1) %>% 
    tidy() %>% 
    select(-alternative, -method, -parameter) %>% 
    set_names(paste0("adf.",names(.))) %>% 
    return()
})

# aplica Dickey-Fuller usando o URCA package
testURDF <- function(m){
  m$residuals %>% 
    ur.df(type="drift", lags=1) %>% 
    return()
}

# devolve o cálculo de meia vida tirado do urca::ur.df
urdfHalfLife <- function(urdf){
  urdf %>% summary() -> m
  half.life <- -log(2)/m@testreg$coefficients[2]
  return(half.life)
}

# devolve o cálculo de meia vida tirado do tseries::adf.test
calcHalfLife <- function(m){
  tibble(y = m$residuals) %>% 
    mutate(
      y.lag   = dplyr::lag(y,1),
      delta.y = c(NA,diff(y))
    ) %>% 
    filter( complete.cases(.) ) %>% 
    lm(delta.y ~ y.lag, data=.) %>% 
    summary() -> regress
  
  lambda  <- regress$coefficients[2]
  half.life <- -log(2)/lambda
  return(half.life)
}

# calcula a largura do canal (2*spread)
spreadSize <- function(m){
  return(2*sd(m$residuals))
}