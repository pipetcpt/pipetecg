library(tidyverse)

examples <- list()
examples$case1 <- read_csv('data-raw/1001-A-PRE1D_0H_R110-0H_20191008083018.CSV') %>% 
  print()

examples$case2 <- read_csv('data-raw/1105-C-POST2D_8H_R260-8H_20191011165456.CSV') %>% 
  print()

save(examples, file = 'examples.Rdata')
