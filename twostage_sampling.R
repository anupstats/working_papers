library(tidyverse)
library(haven)
test <- read_sav("C:/Users/SGPGIMS/Downloads/new01_3.sav")
#View(test)
test <- tibble(test)


t<-test %>% 
  # group the population by cluster
  group_nest(SDISTRI) %>% 
  # draw the desired number of clusters
  sample_n(size = 10) %>% 
  # and revert to a form that we can work with.
  unnest(data)




mssample <- function(test) {
  samplesz <- 1000
  strata_cnt <- length(unique(test$SDISTRI))
  hh_per_cluster <- 10
  clusters_per_stratum <- samplesz/hh_per_cluster/strata_cnt
  
  msdata <- 
    test %>% 
    group_nest(SDISTRI) %>% 
    rowwise() %>% 
    mutate(psusz = nrow(data)) %>% 
    group_nest(SDISTRI) %>% 
    rowwise() %>% 
    mutate(stratasz = sum(data$psusz))
  
  stage1 <- 
    msdata %>% 
    rowwise() %>% 
    mutate(sample = list(tibble(slice_sample(data, n = clusters_per_stratum, weight_by = data[[1]]$psusz)))) %>% 
    select(-data) %>% 
    unnest(sample)
  
  stage2 <- 
    stage1 %>% 
    rowwise() %>% 
    mutate(sample = list(sample_n(data, size = hh_per_cluster))) %>% 
    select(-data) %>% 
    unnest(sample)
  
  s <- 
    stage2 %>% 
    mutate(
      p1 = psusz/stratasz, p2 = hh_per_cluster/psusz,
      p = p1 * p2,
      wt = 1/p, wt = wt/sum(wt) * length(wt)) %>% 
    select(-stratasz, -psusz, -p1, -p2, -p)
  
  s
}

(s <- mssample(test))



