# reproduzindo valores da planilha do Sergio Ferro
# referente ao webinar: https://www.youtube.com/watch?v=5_J95MzeeaE

library(BatchGetSymbols)
library(tidyverse)
library(lubridate)

tickers <- c("BBAS3", "CSAN3") %>% paste0(".SA")
start <- dmy(04052013)
end   <- dmy(02042014)

dataset <- BatchGetSymbols(
  tickers = tickers,
  first.date = start,
  last.date = end,
  thresh.bad.data = 0.01
)

dataset$df.control

prices <- dataset$df.tickers %>% 
  as.tibble() %>% 
  filter(complete.cases(.)) %>% 
  mutate( ticker = gsub("\\.SA","",ticker))

prices %>% 
  select(ticker, ref.date, price.close) %>% 
  spread(key = ticker, value=price.close ) %>% 
  arrange(ref.date) -> ativos

ativos %>% 
  ggplot(aes(x=BBAS3, y=CSAN3)) +
  geom_point() +
  theme_light()

regr <- lm(BBAS3~CSAN3+ref.date, ativos)

regr %>% 
  summary()


library(urca)
df <- ur.df(regr$residuals, lags=1)

df@cval
df@teststat

var <- sd(regr$residuals)


tibble(
  ref.date = ativos$ref.date,
  coint    = regr$residuals,
  ramp     = abs(coint/var)
) %>% 
  ggplot(aes(x=ref.date, y=coint)) +
  geom_line(aes(color=ramp), size=1) +
  geom_hline(yintercept = 2*var, color="red", size=1, linetype=2) +
  geom_hline(yintercept = -2*var, color="green", size=1, linetype=2) +
  scale_color_continuous(low="red",high="green") +
  theme_light()

?deltat

spread <- ts(regr$residuals, end=length(regr$residuals))

plot(spread)

spread2 <- spread


plot(spread2)

ou.fit <- mle(ou.lik(spread2),
              start=list(theta1=1,theta2=1,theta3=1),
              method="L-BFGS-B",lower=c(0,1e-5,1e-3), upper=c(2,2,2))
ou.coe <- coef(ou.fit)
ou.coe  

sigma <- ou.coe[2]
mu    <- ou.coe[1]/ou.coe[2]
sd(regr$residuals)
ou.coe[3]
1/sigma

sde.sim(t0 = 1,
        T = length(regr$residuals),
        X0 = regr$residuals[1],
        N = length(regr$residuals),
        drift = expression(0.001152081-0.298470941*x),
        sigma = expression(0.802541915),
        sigma.x = expression(0)) -> x

plot(x)
plot(spread)

f1 <- Vectorize(function(x) 5-3*x)
f2 <- Vectorize(function(x) 3*((5/3)-x))
f1(1:10)
f2(1:10)
