
x %>% 
  filter( ticker.a == "CYRE3.SA" & ticker.b=="GFSA3.SA" ) %>% 
  pull(model) %>% 
  .[[1]] %>% 
  residuals() -> rsd
  
rsd %>%   
  plot(type="l")

rsd.ts <- ts(rsd, frequency = 5)

rsd.ts %>% 
  stl(s.window="periodic") -> rsd.decomp

rsd.decomp %>% 
  plot() 

rsd.ts %>% 
  adf.test(alternative="stationary")

rsd.deseas <- seasadj(rsd.decomp)

plot(rsd.ts)
plot(rsd.deseas)

acf(rsd.deseas)
pacf(rsd.deseas)

auto.arima(rsd.deseas)

fit <- arima(rsd.deseas, order=c(1,0,0))

tsdisplay(residuals(fit),lag.max = 45, main="Arima (1,0,0)")
