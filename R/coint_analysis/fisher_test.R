library(tidyverse)
library(lubridate)
library(broom)

dt <- read_csv("./data/sferro_cenario.csv") %>% 
  filter(complete.cases(.)) %>% 
  mutate( DATA = dmy(DATA) ) %>% 
  arrange(DATA) %>% 
  tail(100)

dt %>% 
  mutate(
    KROT3 = log(KROT3/lag(KROT3)),
    BEEF3 = log(BEEF3/lag(BEEF3))
  ) %>% 
  filter(complete.cases(.)) -> dtl

ggplot(dtl, aes(x=KROT3, y=BEEF3)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", se=F, fullrange=T)


corr <- cor.test(x=dtl$KROT3, y=dtl$BEEF3, method = "pearson", conf.level = .999)

tidy(corr)

r <- corr$estimate

z <- 0.5 * log((1+r)/(1-r))

n <- nrow(dt)
pr <- sqrt(1/(n-3))
expected.z <- 0.5 * log((1+pr)/(1-pr))

upper <- z + pr * 2.58 # 1.96 
lower <- z - pr * 2.58 # 1.96
  
conf.upper <- (exp(2 * upper) - 1) / (exp(2 * upper) + 1)
conf.lower <- (exp(2 * lower) - 1) / (exp(2 * lower) + 1)
c(conf.lower, conf.upper)

corr$conf.int



# Summary of Computations
# 
# 1. Compute the sample r.
# 
# 2. Use the r to z' table to convert the value of r to z'.
# 
# 3. Use a z table to find the value of z for the level of confidence desired.
# 
# 4. Compute: 
# 
#   4a. Lower limit = z' - (z)(σz')
#                         
#   4b. Upper limit = z' + (z)(σz')
# 
# 5. Use procedure the r to z' table to convert the lower and upper limits from z' to r 
#                                                 
# Assumptions
#   1. Each subject (observation) is sampled independently from each other subject.
#   2. Subjects are sampled randomly.