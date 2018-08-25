---
title: "Operando Long-Short usando Cointegração em R"
author: Giuliano Sposito
output:
  html_document:
    keep_md: yes
    code_folding: hide
  pdf_document: default
---

Operações Long-Short por Cointegração é uma estratégia de investimento em ativos que consistes em ...este notebook refaz o passo-a-passo nas análises estatísticas necessárias para esse tipo de operação

<!--more-->

## Introdução

Operações Long-Short por Cointegração... 

## Análise de Cointegração usando R



Este notebook tem o objetivo de reproduzir o passo-a-passo para detectar e avaliar pares de ativos [cointegrados](https://en.wikipedia.org/wiki/Cointegration) mostrado no [blog do Dr. Nickel](https://drnickel.wordpress.com/2015/04/03/long-short-atraves-de-cointegracao-parte-3/). 
Muitos dos textos explicativos aqui foram retirados (ou fortemente baseados) na série de posts sobre Cointegração explicando o processo de análise, portando vale a pena ler a série toda, bem mais detalhada e com ótimas referências:

1. [Long-Short através de Cointegração – Parte 1](https://drnickel.wordpress.com/2015/03/15/long-short-atraves-de-cointegracao-parte-1/)
1. [Long-Short através de Cointegração – Parte 2](https://drnickel.wordpress.com/2015/03/15/long-short-atraves-de-cointegracao-parte-2/)
1. [Long-Short através de Cointegração – Parte 3](https://drnickel.wordpress.com/2015/04/03/long-short-atraves-de-cointegracao-parte-3/)
1. [Long-Short através de Cointegração – Parte 4](https://drnickel.wordpress.com/2016/11/05/long-short-atraves-de-cointegracao-parte-4/)

### Dataset:  QUAL3 e RENT3

No poste ele faz faz uma análise dos ativos [**QUAL3** - QUALICORP ON](https://www.infomoney.com.br/qualicorp-qual3) e [**RENT3** - Localiza ON](https://www.infomoney.com.br/localiza-rent3) usando uma [planilha excel](https://drnickel.files.wordpress.com/2015/04/exemplos-cointegracao-qual3_rent32.xlsx), neste notebook refaço o procedimento usando R. Para garantir que os resultados obtidos em ambas sejam os mesmos usaremos exatamente os mesmo dados que constam na planilha.


![Cotações de QUAL3 e RENT3 no Excel](./img/excel_data.png)


Após a importação, obtemos:


```r
# data handlers
library(tidyverse)
library(lubridate)

# table formatting
library(kableExtra)

# loading data
library(xlsx)
xls <- read.xlsx("./exemplos-cointegracao-qual3_rent32.xlsx",2) 

# O que nos importamos?
xls %>%
  select(-`NA..4`) %>%  # esta coluna estraga a formatacao da tabela
  head(10) %>% 
  kable(caption="Excel Importado") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Excel Importado</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> QUAL3 </th>
   <th style="text-align:left;"> NA. </th>
   <th style="text-align:left;"> NA..1 </th>
   <th style="text-align:left;"> RENT3 </th>
   <th style="text-align:left;"> NA..2 </th>
   <th style="text-align:left;"> NA..3 </th>
   <th style="text-align:left;"> NA..5 </th>
   <th style="text-align:left;"> NA..6 </th>
   <th style="text-align:left;"> NA..7 </th>
   <th style="text-align:left;"> NA..8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Preco </td>
   <td style="text-align:left;"> Lag </td>
   <td style="text-align:left;"> Delta </td>
   <td style="text-align:left;"> Preco </td>
   <td style="text-align:left;"> Lag </td>
   <td style="text-align:left;"> Delta </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-10 </td>
   <td style="text-align:left;"> 19.65 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 31.953 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-11 </td>
   <td style="text-align:left;"> 19.7 </td>
   <td style="text-align:left;"> 19.65 </td>
   <td style="text-align:left;"> 0.0500000000000007 </td>
   <td style="text-align:left;"> 32.232 </td>
   <td style="text-align:left;"> 31.953 </td>
   <td style="text-align:left;"> 0.279 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-15 </td>
   <td style="text-align:left;"> 19.87 </td>
   <td style="text-align:left;"> 19.7 </td>
   <td style="text-align:left;"> 0.170000000000002 </td>
   <td style="text-align:left;"> 32.124 </td>
   <td style="text-align:left;"> 32.232 </td>
   <td style="text-align:left;"> -0.107999999999997 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-16 </td>
   <td style="text-align:left;"> 19.35 </td>
   <td style="text-align:left;"> 19.87 </td>
   <td style="text-align:left;"> -0.52 </td>
   <td style="text-align:left;"> 31.584 </td>
   <td style="text-align:left;"> 32.124 </td>
   <td style="text-align:left;"> -0.540000000000003 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-17 </td>
   <td style="text-align:left;"> 18.75 </td>
   <td style="text-align:left;"> 19.35 </td>
   <td style="text-align:left;"> -0.600000000000001 </td>
   <td style="text-align:left;"> 30.594 </td>
   <td style="text-align:left;"> 31.584 </td>
   <td style="text-align:left;"> -0.989999999999998 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-18 </td>
   <td style="text-align:left;"> 18.87 </td>
   <td style="text-align:left;"> 18.75 </td>
   <td style="text-align:left;"> 0.120000000000001 </td>
   <td style="text-align:left;"> 31.134 </td>
   <td style="text-align:left;"> 30.594 </td>
   <td style="text-align:left;"> 0.539999999999999 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-19 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 18.87 </td>
   <td style="text-align:left;"> 0.129999999999999 </td>
   <td style="text-align:left;"> 31.215 </td>
   <td style="text-align:left;"> 31.134 </td>
   <td style="text-align:left;"> 0.0809999999999995 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-22 </td>
   <td style="text-align:left;"> 19.2 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 0.199999999999999 </td>
   <td style="text-align:left;"> 31.512 </td>
   <td style="text-align:left;"> 31.215 </td>
   <td style="text-align:left;"> 0.297000000000001 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-23 </td>
   <td style="text-align:left;"> 19.15 </td>
   <td style="text-align:left;"> 19.2 </td>
   <td style="text-align:left;"> -0.0500000000000007 </td>
   <td style="text-align:left;"> 31.512 </td>
   <td style="text-align:left;"> 31.512 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Como o Excel não está adequadamente formatado (há células colapsadas e colunas vazias entre os dados) e acabamos com um dado importado muito sujo, há necessidade de limpar, *tipar* e arrumar 
[(data tidying)](http://r4ds.had.co.nz/tidy.html), deixando-os simples de manipular.


```r
# limpando e arrumando (tidying)
# primeiro isola o QUAL3
xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=QUAL3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="QUAL3",
          price.close = as.numeric(as.character(price.close)) ) -> qual


# despois o RENT3
xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=RENT3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="RENT3",
          price.close = as.numeric(as.character(price.close)) ) -> rent

# Junta ambos
df.tickers = bind_rows(qual,rent)

# ficou melhor
df.tickers %>% 
  arrange(ref.date) %>% 
  head(10) %>% 
  kable(caption="Dados 'Tidy' ") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Dados 'Tidy' </caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ref.date </th>
   <th style="text-align:right;"> price.close </th>
   <th style="text-align:left;"> ticker </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2012-10-10 </td>
   <td style="text-align:right;"> 19.650 </td>
   <td style="text-align:left;"> QUAL3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-10 </td>
   <td style="text-align:right;"> 31.953 </td>
   <td style="text-align:left;"> RENT3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-11 </td>
   <td style="text-align:right;"> 19.700 </td>
   <td style="text-align:left;"> QUAL3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-11 </td>
   <td style="text-align:right;"> 32.232 </td>
   <td style="text-align:left;"> RENT3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-15 </td>
   <td style="text-align:right;"> 19.870 </td>
   <td style="text-align:left;"> QUAL3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-15 </td>
   <td style="text-align:right;"> 32.124 </td>
   <td style="text-align:left;"> RENT3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-16 </td>
   <td style="text-align:right;"> 19.350 </td>
   <td style="text-align:left;"> QUAL3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-16 </td>
   <td style="text-align:right;"> 31.584 </td>
   <td style="text-align:left;"> RENT3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-17 </td>
   <td style="text-align:right;"> 18.750 </td>
   <td style="text-align:left;"> QUAL3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012-10-17 </td>
   <td style="text-align:right;"> 30.594 </td>
   <td style="text-align:left;"> RENT3 </td>
  </tr>
</tbody>
</table>




Com o dataset mais organizado, vamos visualizar os preços dos ativos importados.


```r
# plota o preco dos ativos
library(ggplot2)
ggplot(df.tickers,aes(x=ref.date, y=price.close, color=ticker)) +
  geom_line(size=1) + theme_light()
```

![](cointegracao_em_R_files/figure-html/tickersPlot-1.png)<!-- -->

## Séries Não Estacionárias

O primeiro passo é testar se cada uma das séries é não-estacionária, aplicando o teste [Dickey-Fuller](https://en.wikipedia.org/wiki/Dickey%E2%80%93Fuller_test) em cada série de preços. A hipótese nula do teste DF é que a série é não-estacionária, portanto neste primeiro teste queremos que a hipótese nula não seja rejeitada.

Para aplicar o teste DF, precisamos primeiro calcular os valores defasados (`lags`) de cada série e o `delta` (diferença entre o preço do dia e o preço do dia anterior) e após isto fazer regressão do `delta` em função do `lag`:

$$\Delta P_{t} = \alpha + \beta P_{t-1} + \epsilon$$

Após fazer a regressão, o valor chave a ser usado para determinar se a série é não estacionária é o [`t-statistic`](https://dss.princeton.edu/online_help/analysis/interpreting_regression.htm) do coeficente $\beta$ obtido, que é o valor do coeficiente dividido pelo desvio padrão. Pode ser pensado como uma medida da precisão com que o coeficiente de regressão é medido. Se um coeficiente é grande comparado ao seu erro padrão, provavelmente é diferente de 0. Esse valor então é confrontado contra uma tabela de referência, que determinará se a série é estacionária ou não. 

![Dickey-Fuller Table](./img/dickey-fuller-table.png)

O valor de referência é sensível ao número de pontos usados na regressão. Assim sendo, vamos fazer o fit da regressão linear e obter os valores de referência desta regressão


```r
# esta funcao calcula o delta e o "lag" e entao fit e devolve um modelo
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

# tidyfica alguns parametros do modelo
library(broom)

# testes de series estacionarias
# para cada ticker fita o modelo linear
df.tickers %>%
  group_by(ticker) %>%
  nest() %>% 
  mutate ( lagModel = map(data, fitLagModel), # modelo liner do "lag x delta"
           lm.coefs  = map(lagModel,tidy),    # coeficientes obtidos do modelo
           lm.glance = map(lagModel, glance), # qualidade do fit
           lm.anova  = map(lagModel, anova),  # analise de variancia do modelo
           lm.anova  = map(lm.anova, tidy)) -> stat.test
```

Então para cada um dos ticker, temos um dataset com as cotações, o modelo fitado, dados dos coeficientes, análise de variação e qualidade do fit. Vamos olhar o valor para cada uma das séries.



```r
# visualizando a qualidade do fit
stat.test %>% 
  select(ticker, lm.glance) %>% 
  unnest(lm.glance) %>% 
  kable(caption = "Qualidade da regressão") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Qualidade da regressão</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ticker </th>
   <th style="text-align:right;"> r.squared </th>
   <th style="text-align:right;"> adj.r.squared </th>
   <th style="text-align:right;"> sigma </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> logLik </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> BIC </th>
   <th style="text-align:right;"> deviance </th>
   <th style="text-align:right;"> df.residual </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:right;"> 0.0033114 </td>
   <td style="text-align:right;"> 0.0013220 </td>
   <td style="text-align:right;"> 2.877734 </td>
   <td style="text-align:right;"> 1.664528 </td>
   <td style="text-align:right;"> 0.1975882 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -1244.397 </td>
   <td style="text-align:right;"> 2494.793 </td>
   <td style="text-align:right;"> 2507.455 </td>
   <td style="text-align:right;"> 4148.957 </td>
   <td style="text-align:right;"> 501 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:right;"> 0.0069421 </td>
   <td style="text-align:right;"> 0.0049599 </td>
   <td style="text-align:right;"> 2.466154 </td>
   <td style="text-align:right;"> 3.502292 </td>
   <td style="text-align:right;"> 0.0618665 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -1166.762 </td>
   <td style="text-align:right;"> 2339.524 </td>
   <td style="text-align:right;"> 2352.186 </td>
   <td style="text-align:right;"> 3047.041 </td>
   <td style="text-align:right;"> 501 </td>
  </tr>
</tbody>
</table>


```r
# visualizando a analise de variancia do modelo
stat.test %>% 
  select(ticker, lm.anova) %>% 
  unnest(lm.anova) %>% 
  kable(caption= "Análise de variação") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Análise de variação</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ticker </th>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> sumsq </th>
   <th style="text-align:right;"> meansq </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> price.delta </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 13.78455 </td>
   <td style="text-align:right;"> 13.784545 </td>
   <td style="text-align:right;"> 1.664528 </td>
   <td style="text-align:right;"> 0.1975882 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> Residuals </td>
   <td style="text-align:right;"> 501 </td>
   <td style="text-align:right;"> 4148.95749 </td>
   <td style="text-align:right;"> 8.281352 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:left;"> price.delta </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 21.30065 </td>
   <td style="text-align:right;"> 21.300652 </td>
   <td style="text-align:right;"> 3.502292 </td>
   <td style="text-align:right;"> 0.0618665 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:left;"> Residuals </td>
   <td style="text-align:right;"> 501 </td>
   <td style="text-align:right;"> 3047.04060 </td>
   <td style="text-align:right;"> 6.081917 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>


```r
# coeficientes obtidos
stat.test %>% 
  select(ticker, lm.coefs) %>% 
  unnest(lm.coefs) %>% 
  kable(caption="Coeficientes da regressão") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Coeficientes da regressão</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ticker </th>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 21.368836 </td>
   <td style="text-align:right;"> 0.1283564 </td>
   <td style="text-align:right;"> 166.480526 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> price.delta </td>
   <td style="text-align:right;"> -0.447896 </td>
   <td style="text-align:right;"> 0.3471615 </td>
   <td style="text-align:right;"> -1.290166 </td>
   <td style="text-align:right;"> 0.1975882 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 33.074075 </td>
   <td style="text-align:right;"> 0.1099886 </td>
   <td style="text-align:right;"> 300.704459 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:left;"> price.delta </td>
   <td style="text-align:right;"> -0.423032 </td>
   <td style="text-align:right;"> 0.2260461 </td>
   <td style="text-align:right;"> -1.871441 </td>
   <td style="text-align:right;"> 0.0618665 </td>
  </tr>
</tbody>
</table>

Então comparamos os `t-statistics` obtido para cada um dos coeficientes.


```r
# isola o coeficient dos betas dos modelos
stat.test %>% 
  select(ticker, lm.coefs) %>% 
  unnest(lm.coefs) %>% 
  select(ticker, term, estimate, statistic, p.value) %>%
  filter( term!="(Intercept)" ) %>% 
  select(-term) %>% 
  inner_join( count(df.tickers , ticker), by="ticker") -> coefs

# mostra eles
coefs %>% 
  kable(caption="Coeficientes da regressão") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Coeficientes da regressão</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ticker </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:right;"> -0.447896 </td>
   <td style="text-align:right;"> -1.290166 </td>
   <td style="text-align:right;"> 0.1975882 </td>
   <td style="text-align:right;"> 504 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:right;"> -0.423032 </td>
   <td style="text-align:right;"> -1.871441 </td>
   <td style="text-align:right;"> 0.0618665 </td>
   <td style="text-align:right;"> 504 </td>
  </tr>
</tbody>
</table>

Para QUAL3 encontramos o valor de **-1.29** e para RENT3 encontramos **-1.871** para ~500 amostras. Ambos os valores estão abaixo (em módulo) dos valores de referência na tabela Dickey-Fuller (**-2.87**), mostrando que ambas as séries são "não-estacionárias".

## Teste de Cointegração (metodologia de Engle-Granger)

A metodologia de Engle-Granger consiste em dois passos: primeiro fazemos uma regressão linear de uma série temporal contra a outra. Em seguida, aplicamos um teste de estacionariedade (como por exemplo o teste de Dickey-Fuller) nos resíduos desta regressão. Se os resíduos forem estacionários, isto significa que encontramos a combinação linear tal que as duas séries são cointegradas.

Então vamos avaliar a combinação linear entre QUAL3 e RENT3


```r
df.tickers %>% 
  spread(key=ticker, value=price.close) %>% 
  ggplot(aes(x=RENT3, y=QUAL3)) +
    geom_point(size=2, alpha=.5) + theme_light()
```

![](cointegracao_em_R_files/figure-html/plotCombinacao-1.png)<!-- -->



```r
# faz a composicao para detectar a cointegração
# fitando um modelo de um ativo contra outro
df.tickers %>% 
  spread(key=ticker, value=price.close) %>% # pivota para ref.date, qual3 e rent3
  lm(QUAL3 ~ RENT3, .) -> coint # fita o modelo

# avalia valores obtidos no modelo
coint %>%
  glance() %>% 
  kable(caption = "Qualidade da regressão") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Qualidade da regressão</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> r.squared </th>
   <th style="text-align:right;"> adj.r.squared </th>
   <th style="text-align:right;"> sigma </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> logLik </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> BIC </th>
   <th style="text-align:right;"> deviance </th>
   <th style="text-align:right;"> df.residual </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> value </td>
   <td style="text-align:right;"> 0.7743701 </td>
   <td style="text-align:right;"> 0.7739206 </td>
   <td style="text-align:right;"> 1.369506 </td>
   <td style="text-align:right;"> 1722.882 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -872.6258 </td>
   <td style="text-align:right;"> 1751.252 </td>
   <td style="text-align:right;"> 1763.919 </td>
   <td style="text-align:right;"> 941.5243 </td>
   <td style="text-align:right;"> 502 </td>
  </tr>
</tbody>
</table>


```r
# avalia dados de variancia
coint %>%
  anova() %>%
  tidy() %>%  
  kable(caption = "Análise de Variacao") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Análise de Variacao</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> sumsq </th>
   <th style="text-align:right;"> meansq </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3231.3452 </td>
   <td style="text-align:right;"> 3231.345212 </td>
   <td style="text-align:right;"> 1722.882 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Residuals </td>
   <td style="text-align:right;"> 502 </td>
   <td style="text-align:right;"> 941.5243 </td>
   <td style="text-align:right;"> 1.875546 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>


```r
# mostra os coeficientes do modelo
coint %>% 
  tidy() %>%  
  kable(caption = "Coeficientes da Regressao") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Coeficientes da Regressao</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> -12.466815 </td>
   <td style="text-align:right;"> 0.8174937 </td>
   <td style="text-align:right;"> -15.25004 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RENT3 </td>
   <td style="text-align:right;"> 1.022958 </td>
   <td style="text-align:right;"> 0.0246451 </td>
   <td style="text-align:right;"> 41.50761 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

E aplicar teste DF nos resíduos da regressão.


```r
# monta um dataset para avaliar os residuos
# nao eh necessario mas fica mais facil de visualizar
df.tickers %>%
  filter( ticker=="QUAL3" ) %>% 
  select(ticker, ref.date, price.close) %>% 
  bind_cols(
    tibble(
      predicted = coint$fitted.values,
      residuals = coint$residuals
    )    
  ) %>% 
  mutate( lagRes = lag(residuals,1),
          deltaRes = residuals - lagRes ) -> coint.ds

# mosta os dados
coint.ds %>% 
  head(10) %>% 
  kable(caption = "Preparacao dos Resíduos para fazer teste Dickey-Fuller") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Preparacao dos Resíduos para fazer teste Dickey-Fuller</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ticker </th>
   <th style="text-align:left;"> ref.date </th>
   <th style="text-align:right;"> price.close </th>
   <th style="text-align:right;"> predicted </th>
   <th style="text-align:right;"> residuals </th>
   <th style="text-align:right;"> lagRes </th>
   <th style="text-align:right;"> deltaRes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-10 </td>
   <td style="text-align:right;"> 19.65 </td>
   <td style="text-align:right;"> 20.21976 </td>
   <td style="text-align:right;"> -0.5697610 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-11 </td>
   <td style="text-align:right;"> 19.70 </td>
   <td style="text-align:right;"> 20.50517 </td>
   <td style="text-align:right;"> -0.8051662 </td>
   <td style="text-align:right;"> -0.5697610 </td>
   <td style="text-align:right;"> -0.2354053 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-15 </td>
   <td style="text-align:right;"> 19.87 </td>
   <td style="text-align:right;"> 20.39469 </td>
   <td style="text-align:right;"> -0.5246868 </td>
   <td style="text-align:right;"> -0.8051662 </td>
   <td style="text-align:right;"> 0.2804795 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-16 </td>
   <td style="text-align:right;"> 19.35 </td>
   <td style="text-align:right;"> 19.84229 </td>
   <td style="text-align:right;"> -0.4922895 </td>
   <td style="text-align:right;"> -0.5246868 </td>
   <td style="text-align:right;"> 0.0323973 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-17 </td>
   <td style="text-align:right;"> 18.75 </td>
   <td style="text-align:right;"> 18.82956 </td>
   <td style="text-align:right;"> -0.0795611 </td>
   <td style="text-align:right;"> -0.4922895 </td>
   <td style="text-align:right;"> 0.4127284 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-18 </td>
   <td style="text-align:right;"> 18.87 </td>
   <td style="text-align:right;"> 19.38196 </td>
   <td style="text-align:right;"> -0.5119584 </td>
   <td style="text-align:right;"> -0.0795611 </td>
   <td style="text-align:right;"> -0.4323973 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-19 </td>
   <td style="text-align:right;"> 19.00 </td>
   <td style="text-align:right;"> 19.46482 </td>
   <td style="text-align:right;"> -0.4648180 </td>
   <td style="text-align:right;"> -0.5119584 </td>
   <td style="text-align:right;"> 0.0471404 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-22 </td>
   <td style="text-align:right;"> 19.20 </td>
   <td style="text-align:right;"> 19.76864 </td>
   <td style="text-align:right;"> -0.5686365 </td>
   <td style="text-align:right;"> -0.4648180 </td>
   <td style="text-align:right;"> -0.1038185 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-23 </td>
   <td style="text-align:right;"> 19.15 </td>
   <td style="text-align:right;"> 19.76864 </td>
   <td style="text-align:right;"> -0.6186365 </td>
   <td style="text-align:right;"> -0.5686365 </td>
   <td style="text-align:right;"> -0.0500000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QUAL3 </td>
   <td style="text-align:left;"> 2012-10-24 </td>
   <td style="text-align:right;"> 19.99 </td>
   <td style="text-align:right;"> 19.11497 </td>
   <td style="text-align:right;"> 0.8750336 </td>
   <td style="text-align:right;"> -0.6186365 </td>
   <td style="text-align:right;"> 1.4936701 </td>
  </tr>
</tbody>
</table>


```r
# fit dos residuos
coint.ds %>% 
  lm(lagRes ~ deltaRes, .) -> coint.lm
```



```r
# avalia valores obtidos no modelo
coint.lm %>%
  glance() %>% 
  kable(caption = "Qualidade da regressão") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Qualidade da regressão</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> r.squared </th>
   <th style="text-align:right;"> adj.r.squared </th>
   <th style="text-align:right;"> sigma </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> logLik </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> BIC </th>
   <th style="text-align:right;"> deviance </th>
   <th style="text-align:right;"> df.residual </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> value </td>
   <td style="text-align:right;"> 0.0405699 </td>
   <td style="text-align:right;"> 0.0386549 </td>
   <td style="text-align:right;"> 1.341484 </td>
   <td style="text-align:right;"> 21.18501 </td>
   <td style="text-align:right;"> 5.3e-06 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -860.4938 </td>
   <td style="text-align:right;"> 1726.988 </td>
   <td style="text-align:right;"> 1739.649 </td>
   <td style="text-align:right;"> 901.5899 </td>
   <td style="text-align:right;"> 501 </td>
  </tr>
</tbody>
</table>


```r
# variancia do modelo dos residuos
coint.lm %>%
  anova() %>%
  tidy() %>%  
  kable(caption = "Análise de Variacao") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Análise de Variacao</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> sumsq </th>
   <th style="text-align:right;"> meansq </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> deltaRes </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 38.12413 </td>
   <td style="text-align:right;"> 38.124130 </td>
   <td style="text-align:right;"> 21.18501 </td>
   <td style="text-align:right;"> 5.3e-06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Residuals </td>
   <td style="text-align:right;"> 501 </td>
   <td style="text-align:right;"> 901.58992 </td>
   <td style="text-align:right;"> 1.799581 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>


```r
# coeficientes do modelo dos residuos
coint.lm %>% 
  tidy() %>%  
  kable(caption = "Coeficientes da Regressao") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Coeficientes da Regressao</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.0019098 </td>
   <td style="text-align:right;"> 0.0598141 </td>
   <td style="text-align:right;"> 0.0319291 </td>
   <td style="text-align:right;"> 0.9745413 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> deltaRes </td>
   <td style="text-align:right;"> -0.4952238 </td>
   <td style="text-align:right;"> 0.1075938 </td>
   <td style="text-align:right;"> -4.6027174 </td>
   <td style="text-align:right;"> 0.0000053 </td>
  </tr>
</tbody>
</table>

```r
# usado para mostrar no texto
tstat <- coint.lm %>% tidy %>% filter(term=="deltaRes") %>% pull(statistic)
```

Desta vez, o `t-stat` obtido foi de **-4.603**, muito além (em módulo) dos valores para a tabela de significância para 500 ou mais amostras (**-2.87**). Neste caso confirmando que a hipótese de que a série é estacionária, e portanto os pares estão cointegrados.

## Trading Long-Short

### Spread

O gráfico abaixo apresenta a evolução do spread ao longo do tempo, com duas bandas representando -2 e 2 vezes o desvio padrão dos resíduos. Vemos que, apesar de os resíduos apresentarem certa persistência, comportam-se de maneira aparentemente desejável: flutuam razoavelmente ao redor da média, visitando-a com certa frequência. 


```r
# plot

# banda +/- 2 SD
sd.res <- sd(coint.ds$residuals)

# residuos e banda
coint.ds %>% 
  ggplot(aes(x=ref.date, y=residuals)) + 
  geom_line(color="blue",size=1) +
  geom_hline(yintercept =  2*sd.res, color="red", linetype=2) +
  geom_hline(yintercept = -2*sd.res, color="red", linetype=2) +
  theme_light()
```

![](cointegracao_em_R_files/figure-html/spreadPlot-1.png)<!-- -->

Uma regra possível de operação consistiria em vender o par quando o resíduo estiver acima da banda, e comprar o par quando estiver abaixo da banda. O trade pode ser encerrado quando o par voltar a média ou a um percentual qualquer da média.

### Detectando Operações

Seguindo a estratégia acima, usamos as transições de banda e pela média para detectar os pontos de entrada e saída de operações long e short.


```r
# estrategia de negociacao, abrir ao -/+ 2 * sd e fechar no 0
startPar <- 2*sd.res
closePar <- 0

# detecta os pontos de entrada e saida nas operacoes de short
coint.ds %>% 
  mutate(operation=case_when(
    lagRes < startPar & residuals >= startPar   ~ "short_start", # cruzou o gatilho de start
    lagRes > closePar & residuals <= closePar    ~ "short_stop"  # cruzou o gatilho de stop
  )) %>%
  filter( !is.na(operation) ) %>% 
  # elminia cruzamentos duplicado (quando a operacao ja esta aberta)
  mutate( operation.lag = lag(operation,1) ) %>% 
  select( ref.date, residuals, lagRes, operation, operation.lag ) %>% 
  filter( operation!=operation.lag ) %>% 
  select( -operation.lag) -> ops.short

# detecta pontos de entrada e saida nas operacoes de long
coint.ds %>% 
  mutate(operation=case_when(
    lagRes > -startPar & residuals <= -startPar ~ "long_start",
    lagRes < -closePar & residuals >= -closePar ~ "long_stop"
  )) %>%
  filter( !is.na(operation) ) %>% 
  mutate( operation.lag = lag(operation,1) ) %>% 
  select( ref.date, residuals, lagRes, operation, operation.lag ) %>% 
  filter( operation!=operation.lag ) %>% 
  select( -operation.lag) -> ops.long

# junta as operacoes
operations <- bind_rows(ops.short, ops.long)

# residuos e banda
coint.ds %>% 
  ggplot(aes(x=ref.date, y=residuals)) + 
  geom_line(color="blue",size=1, alpha=0.3) +
  geom_hline(yintercept =  startPar, color="red", linetype=2, alpha=0.5) +
  geom_hline(yintercept =  closePar, color="blue", linetype=2, alpha=0.5) +
  geom_hline(yintercept = -closePar, color="blue", linetype=2, alpha=0.5) +
  geom_hline(yintercept = -startPar, color="red", linetype=2, alpha=0.5) +
  geom_vline(data=operations, 
             mapping=aes(xintercept = ref.date, color=operation),
             size=1, linetype=1) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
```

![](cointegracao_em_R_files/figure-html/startOperations-1.png)<!-- -->

A partir dos pontos de entrada e saída do par detectados, podemos então compor a série de operações.


```r
# junta os pares operados  pares
operations %>% 
  arrange(ref.date) %>% 
  mutate( opLag = lead(operation, 1),
          open=ref.date,
          close = lead(ref.date,1) ) %>% 
  filter( (operation=="short_start" & opLag=="short_stop") |
          (operation=="long_start" & opLag=="long_stop") ) %>% 
  select(-ref.date, -residuals, -lagRes, -opLag) %>% 
  mutate( operation=case_when(
    operation=="short_start" ~ "short",
    operation=="long_start"  ~ "long"
  ) ) %>% 
  mutate( life.span = round(lubridate::interval(open, close)/days(1)) )-> operations

operations %>%  
  kable(caption = "Operacoes Cointegradas") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Operacoes Cointegradas</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> operation </th>
   <th style="text-align:left;"> open </th>
   <th style="text-align:left;"> close </th>
   <th style="text-align:right;"> life.span </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2012-11-12 </td>
   <td style="text-align:left;"> 2012-11-29 </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> long </td>
   <td style="text-align:left;"> 2013-02-13 </td>
   <td style="text-align:left;"> 2013-02-20 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> long </td>
   <td style="text-align:left;"> 2013-07-11 </td>
   <td style="text-align:left;"> 2013-08-20 </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2014-02-04 </td>
   <td style="text-align:left;"> 2014-02-18 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2014-03-07 </td>
   <td style="text-align:left;"> 2014-04-22 </td>
   <td style="text-align:right;"> 46 </td>
  </tr>
</tbody>
</table>

Com a tabela de operações, podemos simular os ganhos de operação, lembrando que quando o par é vendido abre-se simultaneamente uma posição vendida na primeira ação e comprada na segunda ação e a operação oposta é realizada quando o par é comprado.

Neste sentido vamos recuperar os valores da cotação de QUAL3 e RENT3 nas datas de abertura e fechamento de operações.


```r
# precos de abertura
operations %>%
  select( open ) %>% 
  inner_join(df.tickers, by=c("open"="ref.date")) %>% 
  spread(key = ticker, value=price.close) %>% 
  set_names(c("open","QUAL3.SA.OPEN","RENT3.SA.OPEN")) -> prices.open

# precos de fechamento
operations %>%
  select( close ) %>% 
  inner_join(df.tickers, by=c("close"="ref.date")) %>% 
  spread(key = ticker, value=price.close) %>% 
  set_names(c("close","QUAL3.SA.CLOSE","RENT3.SA.CLOSE")) -> prices.close

# monta operacao + precos
operations %>%
  inner_join(prices.open, by="open") %>% 
  inner_join(prices.close, by="close") -> op.results

op.results %>% 
  kable(caption = "Cotações do par integrado") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Cotações do par integrado</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> operation </th>
   <th style="text-align:left;"> open </th>
   <th style="text-align:left;"> close </th>
   <th style="text-align:right;"> life.span </th>
   <th style="text-align:right;"> QUAL3.SA.OPEN </th>
   <th style="text-align:right;"> RENT3.SA.OPEN </th>
   <th style="text-align:right;"> QUAL3.SA.CLOSE </th>
   <th style="text-align:right;"> RENT3.SA.CLOSE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2012-11-12 </td>
   <td style="text-align:left;"> 2012-11-29 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 22.40 </td>
   <td style="text-align:right;"> 31.017 </td>
   <td style="text-align:right;"> 19.99 </td>
   <td style="text-align:right;"> 32.511 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> long </td>
   <td style="text-align:left;"> 2013-02-13 </td>
   <td style="text-align:left;"> 2013-02-20 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 19.50 </td>
   <td style="text-align:right;"> 34.582 </td>
   <td style="text-align:right;"> 21.50 </td>
   <td style="text-align:right;"> 32.914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> long </td>
   <td style="text-align:left;"> 2013-07-11 </td>
   <td style="text-align:left;"> 2013-08-20 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 15.03 </td>
   <td style="text-align:right;"> 30.311 </td>
   <td style="text-align:right;"> 17.37 </td>
   <td style="text-align:right;"> 29.026 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2014-02-04 </td>
   <td style="text-align:left;"> 2014-02-18 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 21.00 </td>
   <td style="text-align:right;"> 29.657 </td>
   <td style="text-align:right;"> 19.60 </td>
   <td style="text-align:right;"> 31.387 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2014-03-07 </td>
   <td style="text-align:left;"> 2014-04-22 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 20.85 </td>
   <td style="text-align:right;"> 29.806 </td>
   <td style="text-align:right;"> 22.47 </td>
   <td style="text-align:right;"> 34.571 </td>
  </tr>
</tbody>
</table>

É possível visualizar as operações de cada um dos ativos na cotação dos mesmos.


```r
# plota os ativos e as operacoes
ggplot() +
  geom_line(data=df.tickers, 
            aes(x=ref.date, y=price.close, color=ticker),size=1) +
  geom_rect(data=op.results,
            aes(xmin=open, ymin=QUAL3.SA.OPEN,
                xmax=close, ymax=QUAL3.SA.CLOSE,
                fill=ifelse(QUAL3.SA.OPEN<QUAL3.SA.CLOSE,"green","purple")),
                alpha=0.35, show.legend = F) +
    geom_rect(data=op.results,
            aes(xmin=open, ymin=RENT3.SA.OPEN,
                xmax=close, ymax=RENT3.SA.CLOSE,
                fill=ifelse(RENT3.SA.OPEN<RENT3.SA.CLOSE,"green","purple")),
                alpha=0.35, show.legend = F) +
  theme_light() 
```

![](cointegracao_em_R_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Com as cotações na mão vamos simular uma operação cointegrada com cash neutro, ou seja, o volume financeiro comprado é o mesmo valor vendido, nesta simulação usaremos R$1000,00 em cada uma dos dois ativos, e vamos avaliar o rendimento das mesmas.


```r
# define o capital investido
k <- 2000

# calculo dos rendimentos
op.results %>% 
  # calcula o volume de cada ativo comprado/vendido correspondente ao valor do capital
  mutate( vol.qual3 = round(0.5*k/QUAL3.SA.OPEN),
          vol.rent3 = round(0.5*k/RENT3.SA.OPEN) ) %>% 
  # calcula o saldo $$ entre vendido e comprado dependendo da direção da operação
  mutate( entrada = case_when(
    operation == "short" ~ round(vol.qual3*QUAL3.SA.OPEN-vol.rent3*RENT3.SA.OPEN,2),
    operation == "long"  ~ round(vol.rent3*RENT3.SA.OPEN-vol.qual3*QUAL3.SA.OPEN,2)
  )) %>% 
  # calcula o saldo $$ para fechar a operação
  mutate( saida = case_when(
    operation == "short" ~ round(vol.rent3*RENT3.SA.CLOSE-vol.qual3*QUAL3.SA.CLOSE,2),
    operation == "long"  ~ round(vol.qual3*QUAL3.SA.CLOSE-vol.rent3*RENT3.SA.CLOSE,2)
  )) %>% 
  # calcula o retorno em função do capital total investido
  mutate(
    retorno = (saida-entrada)/k
  ) -> results

results %>% 
  select(operation, open, close, entrada, saida, retorno) %>% 
  kable(caption = "Retorno obtido") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Retorno obtido</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> operation </th>
   <th style="text-align:left;"> open </th>
   <th style="text-align:left;"> close </th>
   <th style="text-align:right;"> entrada </th>
   <th style="text-align:right;"> saida </th>
   <th style="text-align:right;"> retorno </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2012-11-12 </td>
   <td style="text-align:left;"> 2012-11-29 </td>
   <td style="text-align:right;"> 15.46 </td>
   <td style="text-align:right;"> 140.80 </td>
   <td style="text-align:right;"> 0.062670 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> long </td>
   <td style="text-align:left;"> 2013-02-13 </td>
   <td style="text-align:left;"> 2013-02-20 </td>
   <td style="text-align:right;"> 8.38 </td>
   <td style="text-align:right;"> 141.99 </td>
   <td style="text-align:right;"> 0.066805 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> long </td>
   <td style="text-align:left;"> 2013-07-11 </td>
   <td style="text-align:left;"> 2013-08-20 </td>
   <td style="text-align:right;"> -6.75 </td>
   <td style="text-align:right;"> 205.93 </td>
   <td style="text-align:right;"> 0.106340 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2014-02-04 </td>
   <td style="text-align:left;"> 2014-02-18 </td>
   <td style="text-align:right;"> -0.34 </td>
   <td style="text-align:right;"> 126.36 </td>
   <td style="text-align:right;"> 0.063350 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> short </td>
   <td style="text-align:left;"> 2014-03-07 </td>
   <td style="text-align:left;"> 2014-04-22 </td>
   <td style="text-align:right;"> -12.60 </td>
   <td style="text-align:right;"> 96.85 </td>
   <td style="text-align:right;"> 0.054725 </td>
  </tr>
</tbody>
</table>

Vale ressaltar que as `entradas` mostrados na tabela corresponde a diferença ao balanço líquido de capital, já que nem sempre é possível "casar" exatamente o mesmo valor dado o valor unitário das ações. Já as `saídas` corresponde saldo obtido para zerar as operações de long+short, o que em tese, corresponde o ganho obtido.

Já o `rendimento` é calculado com base no valor obtido sobre o capital total investido, na nossa simulação R$2000,00, ou seja, R$1000,00 para cada operação e o resultado total:


```r
results %>% 
  summarise( saldoEntrada = sum(entrada,na.rm = T),
             saldoSaida   = sum(saida,na.rm = T)) %>% 
  t() %>% 
  kable(caption="Resultado") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Resultado</caption>
<tbody>
  <tr>
   <td style="text-align:left;"> saldoEntrada </td>
   <td style="text-align:right;"> 4.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> saldoSaida </td>
   <td style="text-align:right;"> 711.93 </td>
  </tr>
</tbody>
</table>

Como simulamos as operações usando a estratégia [_cash-neutral_](https://www.investopedia.com/terms/c/cash-neutral.asp), em teoria poderíamos manter o capital total investido em Tesouro Direto, LCAs ou LCI e usá-lo como garantia, permitindo assim que as operações pudessem sere feitas com muito pouco dinheiro investido. No cenário simulado foi possível obter cerca de R$700,00 dos R$2000,00 reais em uma janela de dois anos (35% de rendimento bruto), o que parece bem promissor.

Cabe lembrar que não acrescentamos os custos de operação, do aluguel de ações ou de tributos, o que poderiam alterar significativamente o resultado final. Tema esse que ficará para um próximo post.

## Conclusão

Reprodizimos neste [R-Notebook](https://rmarkdown.rstudio.com/r_notebooks) o passo-a-passo para encontrar ativos financeiros cointegrados e identificar os pontos de entrada de operações Long e Short e também conseguimos avaliar os resultados dessa estratégida de investimento. 

## Referências

1. Moura, Guilherme V. e Caldeira, João F. - **Seleção de uma Carteira de Pares de Ações Usando Cointegração: Uma Estrategia de Arbitragem Estatística** - https://www.scribd.com/document/237462625/4785-19806-1-PB
1. Caldeira, João F. - **Arbitragem Estatística, Estratégia Long-Short Pairs Trading, Abordagem com Cointegração Aplicada ao Mercado de Ações Brasileiro** - http://www.anpec.org.br/revista/vol14/vol14n1p521_546.pdf
1. [Blog do Dr. Nickel](https://drnickel.wordpress.com/) - **Long-Short através de Cointegração** – [Parte 1](https://drnickel.wordpress.com/2015/03/15/long-short-atraves-de-cointegracao-parte-1/), [Parte 2](https://drnickel.wordpress.com/2015/03/15/long-short-atraves-de-cointegracao-parte-2/), [Parte 3](https://drnickel.wordpress.com/2015/04/03/long-short-atraves-de-cointegracao-parte-3/) e [Parte 4](https://drnickel.wordpress.com/2016/11/05/long-short-atraves-de-cointegracao-parte-4/)
