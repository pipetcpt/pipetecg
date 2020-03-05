# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4389751/

examples_1 <- map(sprintf('data-raw/12248_2014_9661_MOESM%s_ESM.txt', 2:6), 
                ~ read.csv(.x, header = TRUE,sep = '\t', as.is=TRUE) %>% 
                  select(-1) %>% 
                  select(SUBJ = 1, GRP = 2, PRD = 3, TRT = 4, AUClast = 5)) %>% 
  set_names(sprintf('case%s', 2:6))

examples_2 <- map(sprintf('data-raw/12248_2014_9661_MOESM%s_ESM.txt', 7:8), 
                ~ read.csv(.x, header = TRUE,sep = '\t', as.is=TRUE) %>% 
                  select(-1) %>% 
                  select(SUBJ = 1, GRP = 4, PRD = 3, TRT = 2, AUClast = 5)) %>% 
  set_names(sprintf('case%s', 7:8))

examples_1$case1 <- read.csv('12248_2014_9661_MOESM1_ESM.txt',
                           header = TRUE,
                           sep = '\t', as.is=TRUE) %>% 
  select(SUBJ = 1, GRP = 2, PRD = 3, TRT = 4, AUClast = 5) %>% 
  print()

examples <- c(examples_1, examples_2)

save(examples, file = 'examples.Rdata')
