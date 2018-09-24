library(tidyselect)
library(BatchGetSymbols)

# funcao para listar os pares a serem testados
# nesta implementacao pega arbritariamente os pares
# estudados no case da UFRGS
getCandidatePairs <- function(){
  # pair / test.cases
  readRDS("./data/pair_analysis.rds") %>% 
    mutate( ticker.a = paste0(ticker.a, ".SA"), # precisa manter a nomenclatura do yahoo
            ticker.b = paste0(ticker.b, ".SA")) %>% 
    select( ticker.a, ticker.b ) %>% 
    return()
}


# funcao que retorna a tabela de precos
# mas tambem retorna pares validos para analise
# excluindo pares que nao tem dados
getPrices <- function(.pairs, .start.date, .end.date){

    # tickers to get data
  .tickers <- c(.pairs$ticker.a, .pairs$ticker.b) %>% # todos os ativos dos pares
    unique() # nao repetido
  
  # obtem precos
  .prices <- BatchGetSymbols(
    tickers = .tickers,       # tickers da analise
    first.date = .start.date, # data de inicio
    last.date =  .end.date,   # data de fim
    thresh.bad.data = 0.01   # considerar serie com pelo menos 99% de cobertura
  )
  
  # vamos tirar do test.cases tickers nao baixados
  .prices$df.control %>% 
    filter(download.status=="OK", threshold.decision=="KEEP") %>% 
    pull(ticker) %>% 
    as.character() -> valid.tickers
  
  # mantem somente casos de testes que tem dados
  .pairs %>%
    filter( ticker.a %in% valid.tickers,
            ticker.b %in% valid.tickers ) -> .valid.pairs
  
  # dando uma olhada no que está no price.adjusted
  .prices <- .prices$df.tickers %>% 
    as.tibble() %>% # please! 
    arrange(ref.date)
  
  # retorna tabela de preco
  list(
    valid.pairs = .valid.pairs,
    price.table = .prices
  ) %>% return()
}
