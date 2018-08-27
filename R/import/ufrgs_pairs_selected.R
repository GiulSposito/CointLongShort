# scrapping no artigo: ./docs/vol14n1p521_546.pdf

# setup
library(tidyverse)
library(tabulizer)
library(tabulizerjars)

# extract "tables"
pdf.file <- "./docs/vol14n1p521_546.pdf"
pdf.dat <- extract_tables(pdf.file)

# look the result
str(pdf.dat)

# we are interested in the first one
table.txt <- pdf.dat[[1]]
str(table.txt)

# They are a char matrix of 21 x 1 in including header
test.cases <- pdf.dat[[1]][2:21,1] %>%       # without the header
  str_split(pattern = " ", simplify = T) %>% # split the coluns
  as.tibble() %>%                            # convert in tibble
  mutate_at(vars(V3:V7), as.numeric) %>%     # values as numeric
  set_names(c("ticker.a","ticker.b","eg.adf","jh.lambda", # set names
              "ir.in_sample","half.life","rent.liq"))

# save dataset
saveRDS(test.cases, "./data/pair_analysis.rds")

