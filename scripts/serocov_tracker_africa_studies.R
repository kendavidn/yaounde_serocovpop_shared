library(tidyverse)
library(here)
library(countrycode)
library(janitor)

serotrack <- 
  read_csv(here("data", "serotracker_serosurveys.csv")) %>% 
  clean_names()

serotrack %>% 
  mutate(countrycode = countryname(country, "iso3c")) %>% 
  mutate(continent = countrycode(countrycode, "iso3c", "continent")) %>% 
  filter(continent == "Africa") %>% 
  arrange(denominator_value) %>% 
  quick_html()
  

igg_positive_with_symptoms <- 
  yao %>% 
  filter(cat_igg_result == "Positive") %>% 
  filter(mcat_symp != "No symptoms")

igg_positive_with_symptoms %>% 
  filter(!is.na(mcat_consult)) %>% 
  plot_upset(mcat_consult,
             denom = igg_positive_with_symptoms
             )

  plot_upset()

