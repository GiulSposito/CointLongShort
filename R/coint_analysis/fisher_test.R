library(psych)

n <- 30 
r <- seq(0,.9,.1) 
rc <- matrix(r.con(r,n),ncol=2) 
test <- r.test(n,r)


plot(rc)


cor(rc)

library(tidyverse)
library(lubridate)

dt <- read_csv("./data/sferro_cenario.csv") %>% 
  filter(complete.cases(.)) %>% 
  mutate( DATA = dmy(DATA) ) %>% 
  arrange(desc(DATA))

dt %>% 
  arrange(desc(DATA)) %>% 
  mutate(
    KROT3.LAG = lag(KROT3),
    BEEF3.LAG = lag(BEEF3)
  ) %>%
  mutate(
    KROT3.PRP = log( KROT3.LAG / KROT3 ),
    BEEF3.PRP = log( BEEF3.LAG / BEEF3 )
  ) %>%
  filter(complete.cases(.)) %>%
  slice(n()-100:n()) %>%
  select(KROT3.PRP, BEEF3.PRP) %>%
  as.matrix() %>% 
  r.test(n=nrow(.),r)
  
  

  slice(n()-100:n()) %>% 
  select(KROT3.PRP, BEEF3.PRP) %>% 
  cor()




