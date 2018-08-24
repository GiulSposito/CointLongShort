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

# what we get?
xls %>%
  select(-`NA..4`) %>%  # this column crashs the table format
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
[data tidying](http://r4ds.had.co.nz/tidy.html), deixando-os simples de manipular.


```r
# cleaning up (tidying)
# first get QUAL3
xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=QUAL3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="QUAL3",
          price.close = as.numeric(as.character(price.close)) ) -> qual


# and then RENT3
xls %>% 
  as.tibble() %>% 
  select( ref.date = Data, price.close=RENT3 ) %>% 
  filter( complete.cases(.) ) %>% 
  mutate( ticker="RENT3",
          price.close = as.numeric(as.character(price.close)) ) -> rent

# bind them
df.tickers = bind_rows(qual,rent)

# much better
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
## plot as line
library(ggplot2)
ggplot(df.tickers,aes(x=ref.date, y=price.close, color=ticker)) +
  geom_line(size=1) + theme_light()
```

![](cointegracao_em_R_files/figure-html/tickersPlot-1.png)<!-- -->

## Séries Não Estacionárias

O primeiro passo é testar se cada uma das séries é não-estacionária, aplicando o teste [Dickey-Fuller](https://en.wikipedia.org/wiki/Dickey%E2%80%93Fuller_test) em cada série de preços. A hipótese nula do teste DF é que a série é não-estacionária, portanto neste primeiro teste queremos que a hipótese nula não seja rejeitada.

Para aplicar o teste DF, precisamos primeiro calcular os valores defasados (_lags_) de cada série e o _delta_ (diferença entre o preço do dia e o preço do dia anterior) e após isto fazer regressão do _delta_ em função do _lag_:

$$\Delta P_{t} = \alpha + \beta P_{t-1} + \epsilon$$

Calculo do Lag e Delta


```r
# thake the QUAL3 price.close and calc the delta and the lag
df.tickers %>% 
  filter(ticker=="QUAL3") %>% 
  select( price.close ) %>% 
  mutate( price.lag = lag(price.close,1),
          price.delta = price.close - price.lag ) -> lagData

# shows data
lagData %>% 
  head(10) %>% 
  kable(caption="Dados com Lag e Delta de Preço") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Dados com Lag e Delta de Preço</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> price.close </th>
   <th style="text-align:right;"> price.lag </th>
   <th style="text-align:right;"> price.delta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 19.65 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19.70 </td>
   <td style="text-align:right;"> 19.65 </td>
   <td style="text-align:right;"> 0.05 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19.87 </td>
   <td style="text-align:right;"> 19.70 </td>
   <td style="text-align:right;"> 0.17 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19.35 </td>
   <td style="text-align:right;"> 19.87 </td>
   <td style="text-align:right;"> -0.52 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 18.75 </td>
   <td style="text-align:right;"> 19.35 </td>
   <td style="text-align:right;"> -0.60 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 18.87 </td>
   <td style="text-align:right;"> 18.75 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19.00 </td>
   <td style="text-align:right;"> 18.87 </td>
   <td style="text-align:right;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19.20 </td>
   <td style="text-align:right;"> 19.00 </td>
   <td style="text-align:right;"> 0.20 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19.15 </td>
   <td style="text-align:right;"> 19.20 </td>
   <td style="text-align:right;"> -0.05 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19.99 </td>
   <td style="text-align:right;"> 19.15 </td>
   <td style="text-align:right;"> 0.84 </td>
  </tr>
</tbody>
</table>

Regressão:


```r
# fit linear model as lag = f(delta)
fit <- lm( price.lag ~ price.delta, lagData )
print(fit)
```

```
## 
## Call:
## lm(formula = price.lag ~ price.delta, data = lagData)
## 
## Coefficients:
## (Intercept)  price.delta  
##     21.3688      -0.4479
```

No exemplo assim, obteríamos $\alpha$ = 21.3688364 e $\beta$ = -0.447896.


### Referências

1. Moura, Guilherme V. e Caldeira, João F. - **Seleção de uma Carteira de Pares de Ações Usando Cointegração: Uma Estrategia de Arbitragem Estatística** - https://www.scribd.com/document/237462625/4785-19806-1-PB
1. Caldeira, João F. - **Arbitragem Estatística, Estratégia Long-Short Pairs Trading, Abordagem com Cointegração Aplicada ao Mercado de Ações Brasileiro** - http://www.anpec.org.br/revista/vol14/vol14n1p521_546.pdf
1. [Blog do Dr. Nickel](https://drnickel.wordpress.com/) - **Long-Short através de Cointegração** – [Parte 1](https://drnickel.wordpress.com/2015/03/15/long-short-atraves-de-cointegracao-parte-1/), [Parte 2](https://drnickel.wordpress.com/2015/03/15/long-short-atraves-de-cointegracao-parte-2/), [Parte 3](https://drnickel.wordpress.com/2015/04/03/long-short-atraves-de-cointegracao-parte-3/) e [Parte 4](https://drnickel.wordpress.com/2016/11/05/long-short-atraves-de-cointegracao-parte-4/)
