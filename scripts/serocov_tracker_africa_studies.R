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
  