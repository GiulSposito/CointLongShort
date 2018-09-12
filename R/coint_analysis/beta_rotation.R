library(tidyverse)
library(lubridate)
library(broom)

readr::read_csv("./data/sferro_cenario.csv",T) %>% 
  filter(complete.cases(.)) %>% 
  mutate(DATA=dmy(DATA)) %>% 
  set_names(c("ref.date","ticker.a","ticker.b")) %>% 
  arrange(ref.date) -> prices

dtfm <- prices
periods <- 40


dates <- dtfm$ref.date
steps <- 1:(length(dates)-periods)

lapply( steps, 
        function(idx, periods, dates, dtfm){
          sel.date  <- dates[idx]
          last.date <- dates[idx+periods]
          
          dtfm %>% 
            filter(ref.date >= sel.date) %>% 
            head(periods) %>% 
            lm(ticker.a ~ ticker.b + ref.date, .) %>% 
            tidy() %>% 
            filter( term=="ticker.b" ) %>% 
            select(estimate) %>% 
            bind_cols(tibble(ref.date=last.date),.) %>% 
            return()
          
        },
        periods =50, 
        dates   = dates,
        dtfm    = prices) %>% 
  bind_rows() -> beta.rotation

beta.rotation %>% 
  filter(ref.date >= dmy(20062018)) %>% 
  pull(estimate) %>% 
  sd() -> beta.sd
  
beta.rotation %>% 
  filter(ref.date >= dmy(20062018)) %>% 
  pull(estimate) %>% 
  mean() -> beta.mu

beta.rotation %>% 
  filter(ref.date >= dmy(20062018)) %>% 
  ggplot(.) + 
  geom_line(aes(x=ref.date, y=estimate), size=1, color="blue") +
  geom_hline(yintercept = beta.mu) +
  geom_hline(yintercept = beta.mu+2*beta.sd) +
  geom_hline(yintercept = beta.mu-2*beta.sd) +
  theme_light()
