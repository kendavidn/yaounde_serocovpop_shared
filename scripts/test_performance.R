library(tidyverse)
## test sensitivity with confidence intervals

# sensitivity validation from https://www.sciencedirect.com/science/article/pii/S1386653220303875#bib0060
positives <- 82 
true_positives <- 75
false_negatives <- positives - true_positives

# specificity validation from own work (Projet EPICO pdf)
negatives <- 246
false_positives <- 16
true_negatives <- negatives - false_positives

sens <- true_positives/(true_positives + false_negatives)
spec <- true_negatives/(true_negatives + false_positives)

dat <- as.table(matrix(c(true_positives,
                         false_positives,
                         false_negatives,
                         true_negatives), nrow = 2, 
                       byrow = TRUE))
colnames(dat) <- c("Dis+","Dis-")
rownames(dat) <- c("Test+","Test-")

igg_test_performance <- epiR::epi.tests(dat)

igg_test_performance_table <- 
  data.frame(test_sensivitity = igg_test_performance$elements$sensitivity,
             test_specificity = igg_test_performance$elements$specificity) %>% 
  pivot_longer(cols = 1:6,  names_prefix = "test_") %>% 
  huxtable::huxtable()


# ~~~~ IgM  ----

# placeholders! Temoporarily can't find these values. BUt we don't report them anyway
positives <- 82 
true_positives <- 0
false_negatives <- positives - true_positives

# igm specificity validation from own work (Projet EPICO pdf)
negatives <- 246
false_positives <- 17
true_negatives <- negatives - false_positives

sens <- true_positives/(true_positives + false_negatives)
spec <- true_negatives/(true_negatives + false_positives)

dat <- as.table(matrix(c(true_positives,
                         false_positives,
                         false_negatives,
                         true_negatives), nrow = 2, 
                       byrow = TRUE))
colnames(dat) <- c("Dis+","Dis-")
rownames(dat) <- c("Test+","Test-")

igm_test_performance <- epiR::epi.tests(dat)

igm_test_performance_table <- 
  data.frame(test_sensivitity = igm_test_performance$elements$sensitivity,
             test_specificity = igm_test_performance$elements$specificity) %>% 
  pivot_longer(cols = 1:6,  names_prefix = "test_") %>% 
  filter(row_number() >= 4) %>% 
  huxtable::huxtable()
