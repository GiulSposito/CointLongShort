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



# apply DickeyFuller to 8 distinct periods
checkDickeyFuller <- function(m){
  
  # checa se tem pontos os suficientes
  sample.size <- length(m$residuals)
  
  # vai testar o DF para 8 sequencias padrao
  c(seq(100, 220, 20),250) %>% 
    .[.<=sample.size] %>% # filtra se tem tamanho suficiente
    map(function(n,m){
      
      # para cada # de amostras na sequencia pega as ultimas N
      m$residuals %>% 
        tail(n) %>% 
        ur.df(lags = 1) -> df # aplica o DF
      
      # extrai critical values
      cval <- df@cval
      # resultado do df
      tstat <- df@teststat %>% as.vector()
      
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
        max() -> coint.level # pega aultima falha
      
      # monta o tibble de resposta (1 linha)
      tibble(size = n) %>% # tamanho testado
        bind_cols(df@teststat %>% as.tibble() %>% set_names(c("df"))) %>% # resultado do df 
        bind_cols(df@cval %>% as.tibble()) %>% # Critical Values do test
        bind_cols(
          tibble(
            coint.level = coint.level,  # valor da % 
            coint.result = coint.level >= .95, # teste de aprovacao
            urdf = list(df) # o proprio DF
          )
        ) %>% 
        return()
    }, m=m) %>% 
    bind_rows() %>% 
    return()
}

# aplica o ADF por um determinado periodo
applyADF <- function(n,m){
  
  # pega os ultimos 
  m$residuals %>% 
    tail(n) %>% 
    ur.df(lags = 1) -> df
  
  cval <- df@cval
  tstat <- df@teststat %>% as.vector()
  
  tibble(
    pct = colnames(cval),
    val = as.vector(cval)
  ) %>% 
    mutate(
      pct = 1 - (pct %>% gsub("pct","",.) %>% as.numeric())/100,
      pass = val <= tstat
    ) %>% 
    filter( pass==F ) %>% 
    pull( pct ) %>% 
    max() -> coint.level
  
  tibble(size = n) %>%
    bind_cols(df@teststat %>% as.tibble() %>% set_names(c("df"))) %>% 
    bind_cols(df@cval %>% as.tibble()) %>% 
    bind_cols(
      tibble(
        coint.level = coint.level,
        coint.result = coint.level >= .95,
        urdf = list(df)
      )
    ) %>% 
    return()
}

# devolve o cálculo de meia vida tirado do tseries::adf.test
calcMeiaVida <- function(m,desv.entrada=2){
  tibble(y = m$residuals) %>% 
    mutate(
      y.lag   = dplyr::lag(y,1),
      delta.y = c(NA,diff(y))
    ) %>% 
    filter( complete.cases(.) ) %>% 
    lm(delta.y ~ y.lag, data=.) %>% 
    summary() -> regress
  
  beta = -log(1+regress$coefficients[2])
  half.life=desv.entrada/beta
  
  return(half.life)
}


# calcula a largura do canal (2*spread)
spreadSize <- function(m, desv.entrada=2, desv.saida=0){
  width = desv.entrada - desv.saida
  return(width*sd(m$residuals))
}

# resume os testes de Dickey Fuller
adfSummary <- function(adf.table){
  
  # ordena pelo tamanho
  adf.table %>% 
    arrange(size) %>% 
    select(-urdf) -> adf.results
  
  # separa estatisticas do menor periodo
  # dos ultimos 3
  # de todos
  tibble(
    adfL1.adf  = adf.results[1, ]$df,
    adfL1.lvl  = adf.results[1, ]$coint.level,
    adfL1.rslt = adf.results[1, ]$coint.result,
    adfL3.adf  = adf.results[3,]$df,
    adfL3.rslt = mean(adf.results[1:3,]$coint.result),
    adfL8.adf  = adf.results[nrow(adf.results),]$df,
    adfL8.rslt = mean(adf.results$coint.result),
    adfL8.per  = nrow(adf.results)
  ) %>% 
    return()
}
