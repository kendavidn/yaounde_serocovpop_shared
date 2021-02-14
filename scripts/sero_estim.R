
yaounde_dhs_age_sex <- 
  read_excel(here("data/yaounde_age_2018_2020.xlsx"), sheet = 2) %>% 
  select(age_lower, age_upper, cat_age, male, female, total) %>% 
  # create wider age categories, then resummarise
  mutate(cat_age = cut(age_lower, c(5,15,30,45,65,105), right = FALSE )) %>% 
  group_by(cat_age) %>% 
  summarise(n_male_dhs = sum(male),
            n_female_dhs = sum(female)) %>% 
  # NA category is under 5s
  filter(!is.na(cat_age)) %>% 
  mutate(cat_age = factor(cat_age, labels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  pivot_longer(cols = c(2:3), values_to = "strat_size_dhs", names_to = "cat_sex") %>% 
  mutate(cat_sex = recode(cat_sex, 
                       "n_male_dhs" = "Male", 
                       "n_female_dhs" = "Female"))
  

# sensitivity validation from https://www.sciencedirect.com/science/article/pii/S1386653220303875#bib0060
positives <- 82 
true_positives <- 75
false_negatives <- positives - true_positives


# "Sensitivity was further evaluated with the UK panel of single timepoint collections from 82 hospitalized patients between 14–56 days post symptom onset. "


# specificity validation from own work (Projet EPICO pdf)
negatives <- 246
false_positives <- 16
true_negatives <- negatives - false_positives

sens <- true_positives/(true_positives + false_negatives)
spec <- true_negatives/(true_negatives + false_positives)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Age section  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

igg_pos_table_age <- 
  yao %>% 
  filter(!is.na(cat_igg_result)) %>% 
  group_by(cat_age, cat_sex, cat_igg_result) %>% 
  summarise(n = n()) %>% 
  group_by(cat_age, cat_sex) %>% 
  mutate(strat_size = sum(n)) %>% 
  ungroup() %>% 
  filter(cat_igg_result == "Positive") %>% 
  mutate(prev = n/strat_size) %>% 
  select(-c(cat_igg_result)) %>% 
  rename(n_pos = n) %>% 
  left_join(yaounde_dhs_age_sex) %>% 
  group_by(cat_age) %>% 
  summarise(n_pos_summ = sum(n_pos),
            strat_size_summ = sum(strat_size), 
            raw_prev = sum(prev * strat_size)/sum(strat_size), 
            strat_size_dhs_summ = sum(strat_size_dhs),
            weighted_prev = sum(prev * strat_size_dhs)/sum(strat_size_dhs), 
            n_pos_dhs_summ = weighted_prev * strat_size_dhs_summ # est. number that would be positive if test was applied on whole pop. For CIs
  ) %>%
  relocate(n_pos_dhs_summ, .after = raw_prev) %>% 
  # confidence intervals
  # see https://pubmed.ncbi.nlm.nih.gov/24416798/ for lang_rei formula
  group_by(cat_age) %>% 
  mutate(raw_wilsonCI = BinomCI(n_pos_summ, strat_size_summ, method = "wilson"), 
         weighted_wilsonCI  = BinomCI((weighted_prev * strat_size_summ), strat_size_summ, method = "wilson"), 
         raw_wilsonCI_lower = raw_wilsonCI[[2]], 
         raw_wilsonCI_upper = raw_wilsonCI[[3]], 
         weighted_wilsonCI_lower = weighted_wilsonCI[[2]], 
         weighted_wilsonCI_upper = weighted_wilsonCI[[3]],
         rogan_gladen = (weighted_prev + spec - 1)/(spec + sens - 1), 
         lang_rei_CI = lang_rei_CI(nprev = strat_size_summ, kprev = n_pos_summ, 
                                   nsens = positives, ksens = true_positives, 
                                   nspec = negatives, kspec = true_negatives), 
         lang_rei_CI_lower = lang_rei_CI[[1]],
         lang_rei_CI_upper = lang_rei_CI[[2]]
         ) %>% 
  select(cat_age,strat_size_summ, n_pos_summ, raw_prev, raw_wilsonCI_lower, raw_wilsonCI_upper, 
         weighted_prev, weighted_wilsonCI_lower, weighted_wilsonCI_upper,
         rogan_gladen, lang_rei_CI_lower, lang_rei_CI_upper)  %>% 
  ungroup() %>% 
  rename(variable = cat_age)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Sex section  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igg_pos_table_sex <- 
  yao %>% 
  filter(!is.na(cat_igg_result)) %>% 
  group_by(cat_age, cat_sex, cat_igg_result) %>% 
  summarise(n = n()) %>% 
  group_by(cat_age, cat_sex) %>% 
  mutate(strat_size = sum(n)) %>% 
  ungroup() %>% 
  filter(cat_igg_result == "Positive") %>% 
  mutate(prev = n/strat_size) %>% 
  select(-c(cat_igg_result)) %>% 
  rename(n_pos = n) %>% 
  left_join(yaounde_dhs_age_sex) %>% 
  group_by(cat_sex) %>% 
  summarise(n_pos_summ = sum(n_pos),
            strat_size_summ = sum(strat_size), 
            raw_prev = sum(prev * strat_size)/sum(strat_size), 
            strat_size_dhs_summ = sum(strat_size_dhs),
            weighted_prev = sum(prev * strat_size_dhs)/sum(strat_size_dhs), 
            n_pos_dhs_summ = weighted_prev * strat_size_dhs_summ # est. number that would be positive if test was applied on whole pop. For CIs
  ) %>%
  relocate(n_pos_dhs_summ, .after = raw_prev) %>% 
  # confidence intervals
  # see https://pubmed.ncbi.nlm.nih.gov/24416798/ for lang_rei formula
  group_by(cat_sex) %>% 
  mutate(raw_wilsonCI = BinomCI(n_pos_summ, strat_size_summ, method = "wilson"), 
         weighted_wilsonCI  = BinomCI((weighted_prev * strat_size_summ), strat_size_summ, method = "wilson"), 
         raw_wilsonCI_lower = raw_wilsonCI[[2]], 
         raw_wilsonCI_upper = raw_wilsonCI[[3]], 
         weighted_wilsonCI_lower = weighted_wilsonCI[[2]], 
         weighted_wilsonCI_upper = weighted_wilsonCI[[3]],
         rogan_gladen = (weighted_prev + spec - 1)/(spec + sens - 1), 
         lang_rei_CI = lang_rei_CI(nprev = strat_size_summ, kprev = n_pos_summ, 
                                   nsens = positives, ksens = true_positives, 
                                   nspec = negatives, kspec = true_negatives), 
         lang_rei_CI_lower = lang_rei_CI[[1]],
         lang_rei_CI_upper = lang_rei_CI[[2]]
  ) %>% 
  select(cat_sex,strat_size_summ, n_pos_summ, raw_prev, raw_wilsonCI_lower, raw_wilsonCI_upper, 
         weighted_prev, weighted_wilsonCI_lower, weighted_wilsonCI_upper,
         rogan_gladen, lang_rei_CI_lower, lang_rei_CI_upper)  %>% 
  ungroup() %>% 
  rename(variable = cat_sex)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  verall  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

igg_pos_table_overall <- 
yao %>% 
  filter(!is.na(cat_igg_result)) %>% 
  group_by(cat_age, cat_sex, cat_igg_result) %>% 
  summarise(n = n()) %>% 
  group_by(cat_age, cat_sex) %>% 
  mutate(strat_size = sum(n)) %>% 
  ungroup() %>% 
  filter(cat_igg_result == "Positive") %>% 
  mutate(prev = n/strat_size) %>% 
  select(-c(cat_igg_result)) %>% 
  rename(n_pos = n) %>% 
  left_join(yaounde_dhs_age_sex) %>% 
  summarise(n_pos_summ = sum(n_pos),
            strat_size_summ = sum(strat_size), 
            raw_prev = sum(prev * strat_size)/sum(strat_size), 
            strat_size_dhs_summ = sum(strat_size_dhs),
            weighted_prev = sum(prev * strat_size_dhs)/sum(strat_size_dhs), 
            n_pos_dhs_summ = weighted_prev * strat_size_dhs_summ # est. number that would be positive if test was applied on whole pop. For CIs
  ) %>%
  relocate(n_pos_dhs_summ, .after = raw_prev) %>% 
  # confidence intervals
  # see https://pubmed.ncbi.nlm.nih.gov/24416798/ for lang_rei formula
  mutate(raw_wilsonCI = BinomCI(n_pos_summ, strat_size_summ, method = "wilson"), 
         weighted_wilsonCI  = BinomCI((weighted_prev * strat_size_summ), strat_size_summ, method = "wilson"), 
         raw_wilsonCI_lower = raw_wilsonCI[[2]], 
         raw_wilsonCI_upper = raw_wilsonCI[[3]], 
         weighted_wilsonCI_lower = weighted_wilsonCI[[2]], 
         weighted_wilsonCI_upper = weighted_wilsonCI[[3]],
         rogan_gladen = (weighted_prev + spec - 1)/(spec + sens - 1), 
         lang_rei_CI = lang_rei_CI(nprev = strat_size_summ, kprev = n_pos_summ, 
                                   nsens = positives, ksens = true_positives, 
                                   nspec = negatives, kspec = true_negatives), 
         lang_rei_CI_lower = lang_rei_CI[[1]],
         lang_rei_CI_upper = lang_rei_CI[[2]]
  ) %>% 
  select(strat_size_summ, n_pos_summ, raw_prev, raw_wilsonCI_lower, raw_wilsonCI_upper, 
         weighted_prev, weighted_wilsonCI_lower, weighted_wilsonCI_upper,
         rogan_gladen, lang_rei_CI_lower, lang_rei_CI_upper)  %>% 
  add_column(variable = "Total", .before = 0)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Combine and print table ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

igg_pos_table_print <- 
  igg_pos_table_overall %>% 
  bind_rows(igg_pos_table_sex) %>% 
  bind_rows(igg_pos_table_age) %>%
  # arrange properly
  mutate(variable  = factor(variable, levels = c("Total", "Female", "Male", 
                                                 "5 - 14", "15 - 29", 
                                                 "30 - 44", "45 - 64", 
                                                 "65 +"))) %>% 
  arrange(variable) %>% 
  mutate(variable = as.character(variable)) %>% 
  # etc.
  mutate(across(.cols = c(4:12), .fns = ~ 100 * .x)) %>% # to percentages
  mutate(across(.cols = c(4:12), .fns = ~ format(.x, digits = 3))) %>% 
  transmute(
    Group = variable, 
    n = strat_size_summ, 
    Seropos. = n_pos_summ,
    Crude = paste0(raw_prev, "%", " (", raw_wilsonCI_lower, " - ", raw_wilsonCI_upper, ")"),
    Weighted = paste0(weighted_prev, "%", " (", weighted_wilsonCI_lower, " - ", weighted_wilsonCI_upper, ")"),
    `Weighted,\ntest-adjusted` = paste0(rogan_gladen, "%", " (", lang_rei_CI_lower, " - ", lang_rei_CI_upper, ")")
  ) %>% 
  huxtable() %>% 
  set_contents(1, 1:3, c("", "", "") ) %>% 
  insert_row("", "n", "Seropos.", "Seroprevalence (95% confidence interval)", "", "", after = 0) %>% 
  #merge_cells(1, 4:6) %>% 
  set_bold(1, col = everywhere) %>% 
  theme_basic() %>% 
  set_latex_float("h!") %>% 
  set_all_padding(0.5) 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   IGM  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We have no sensitivity validation study to use for PanBio IgM. Sensitivity is very time-dependent here anyways
# Here I just put what it says on the product manual, assuming 100 individuals
positives <- 100
true_positives <- 96
false_negatives <- positives - true_positives


# specificity validation from own work (Projet EPICO pdf)
negatives <- 246
false_positives <- 17
true_negatives <- negatives - false_positives

sens <- true_positives/(true_positives + false_negatives)
spec <- true_negatives/(true_negatives + false_positives)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Age section  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

igm_pos_table_age <- 
  yao %>% 
  filter(!is.na(cat_igm_result)) %>% 
  group_by(cat_age, cat_sex, cat_igm_result) %>% 
  summarise(n = n()) %>% 
  group_by(cat_age, cat_sex) %>% 
  mutate(strat_size = sum(n)) %>% 
  ungroup() %>% 
  filter(cat_igm_result == "Positive") %>% 
  mutate(prev = n/strat_size) %>% 
  select(-c(cat_igm_result)) %>% 
  rename(n_pos = n) %>% 
  left_join(yaounde_dhs_age_sex) %>% 
  group_by(cat_age) %>% 
  summarise(n_pos_summ = sum(n_pos),
            strat_size_summ = sum(strat_size), 
            raw_prev = sum(prev * strat_size)/sum(strat_size), 
            strat_size_dhs_summ = sum(strat_size_dhs),
            weighted_prev = sum(prev * strat_size_dhs)/sum(strat_size_dhs), 
            n_pos_dhs_summ = weighted_prev * strat_size_dhs_summ # est. number that would be positive if test was applied on whole pop. For CIs
  ) %>%
  relocate(n_pos_dhs_summ, .after = raw_prev) %>% 
  # confidence intervals
  # see https://pubmed.ncbi.nlm.nih.gov/24416798/ for lang_rei formula
  group_by(cat_age) %>% 
  mutate(raw_wilsonCI = BinomCI(n_pos_summ, strat_size_summ, method = "wilson"), 
         weighted_wilsonCI  = BinomCI((weighted_prev * strat_size_summ), strat_size_summ, method = "wilson"), 
         raw_wilsonCI_lower = raw_wilsonCI[[2]], 
         raw_wilsonCI_upper = raw_wilsonCI[[3]], 
         weighted_wilsonCI_lower = weighted_wilsonCI[[2]], 
         weighted_wilsonCI_upper = weighted_wilsonCI[[3]],
         rogan_gladen = (weighted_prev + spec - 1)/(spec + sens - 1), 
         lang_rei_CI = lang_rei_CI(nprev = strat_size_summ, kprev = n_pos_summ, 
                                   nsens = positives, ksens = true_positives, 
                                   nspec = negatives, kspec = true_negatives), 
         lang_rei_CI_lower = lang_rei_CI[[1]],
         lang_rei_CI_upper = lang_rei_CI[[2]], 
         rogan_gladen = ifelse(rogan_gladen < 0, 0, rogan_gladen)
  ) %>% 
  select(cat_age,strat_size_summ, n_pos_summ, raw_prev, raw_wilsonCI_lower, raw_wilsonCI_upper, 
         weighted_prev, weighted_wilsonCI_lower, weighted_wilsonCI_upper,
         rogan_gladen, lang_rei_CI_lower, lang_rei_CI_upper)  %>% 
  ungroup() %>% 
  rename(variable = cat_age)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Sex section  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igm_pos_table_sex <- 
  yao %>% 
  filter(!is.na(cat_igm_result)) %>% 
  group_by(cat_age, cat_sex, cat_igm_result) %>% 
  summarise(n = n()) %>% 
  group_by(cat_age, cat_sex) %>% 
  mutate(strat_size = sum(n)) %>% 
  ungroup() %>% 
  filter(cat_igm_result == "Positive") %>% 
  mutate(prev = n/strat_size) %>% 
  select(-c(cat_igm_result)) %>% 
  rename(n_pos = n) %>% 
  left_join(yaounde_dhs_age_sex) %>% 
  group_by(cat_sex) %>% 
  summarise(n_pos_summ = sum(n_pos),
            strat_size_summ = sum(strat_size), 
            raw_prev = sum(prev * strat_size)/sum(strat_size), 
            strat_size_dhs_summ = sum(strat_size_dhs),
            weighted_prev = sum(prev * strat_size_dhs)/sum(strat_size_dhs), 
            n_pos_dhs_summ = weighted_prev * strat_size_dhs_summ # est. number that would be positive if test was applied on whole pop. For CIs
  ) %>%
  relocate(n_pos_dhs_summ, .after = raw_prev) %>% 
  # confidence intervals
  # see https://pubmed.ncbi.nlm.nih.gov/24416798/ for lang_rei formula
  group_by(cat_sex) %>% 
  mutate(raw_wilsonCI = BinomCI(n_pos_summ, strat_size_summ, method = "wilson"), 
         weighted_wilsonCI  = BinomCI((weighted_prev * strat_size_summ), strat_size_summ, method = "wilson"), 
         raw_wilsonCI_lower = raw_wilsonCI[[2]], 
         raw_wilsonCI_upper = raw_wilsonCI[[3]], 
         weighted_wilsonCI_lower = weighted_wilsonCI[[2]], 
         weighted_wilsonCI_upper = weighted_wilsonCI[[3]],
         rogan_gladen = (weighted_prev + spec - 1)/(spec + sens - 1), 
         lang_rei_CI = lang_rei_CI(nprev = strat_size_summ, kprev = n_pos_summ, 
                                   nsens = positives, ksens = true_positives, 
                                   nspec = negatives, kspec = true_negatives), 
         lang_rei_CI_lower = lang_rei_CI[[1]],
         lang_rei_CI_upper = lang_rei_CI[[2]], 
         rogan_gladen = ifelse(rogan_gladen < 0, 0, rogan_gladen)
  ) %>% 
  select(cat_sex,strat_size_summ, n_pos_summ, raw_prev, raw_wilsonCI_lower, raw_wilsonCI_upper, 
         weighted_prev, weighted_wilsonCI_lower, weighted_wilsonCI_upper,
         rogan_gladen, lang_rei_CI_lower, lang_rei_CI_upper)  %>% 
  ungroup() %>% 
  rename(variable = cat_sex)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Sex section  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

igm_pos_table_overall <- 
  yao %>% 
  filter(!is.na(cat_igm_result)) %>% 
  group_by(cat_age, cat_sex, cat_igm_result) %>% 
  summarise(n = n()) %>% 
  group_by(cat_age, cat_sex) %>% 
  mutate(strat_size = sum(n)) %>% 
  ungroup() %>% 
  filter(cat_igm_result == "Positive") %>% 
  mutate(prev = n/strat_size) %>% 
  select(-c(cat_igm_result)) %>% 
  rename(n_pos = n) %>% 
  left_join(yaounde_dhs_age_sex) %>% 
  summarise(n_pos_summ = sum(n_pos),
            strat_size_summ = sum(strat_size), 
            raw_prev = sum(prev * strat_size)/sum(strat_size), 
            strat_size_dhs_summ = sum(strat_size_dhs),
            weighted_prev = sum(prev * strat_size_dhs)/sum(strat_size_dhs), 
            n_pos_dhs_summ = weighted_prev * strat_size_dhs_summ # est. number that would be positive if test was applied on whole pop. For CIs
  ) %>%
  relocate(n_pos_dhs_summ, .after = raw_prev) %>% 
  # confidence intervals
  # see https://pubmed.ncbi.nlm.nih.gov/24416798/ for lang_rei formula
  mutate(raw_wilsonCI = BinomCI(n_pos_summ, strat_size_summ, method = "wilson"), 
         weighted_wilsonCI  = BinomCI((weighted_prev * strat_size_summ), strat_size_summ, method = "wilson"), 
         raw_wilsonCI_lower = raw_wilsonCI[[2]], 
         raw_wilsonCI_upper = raw_wilsonCI[[3]], 
         weighted_wilsonCI_lower = weighted_wilsonCI[[2]], 
         weighted_wilsonCI_upper = weighted_wilsonCI[[3]],
         rogan_gladen = (weighted_prev + spec - 1)/(spec + sens - 1), 
         lang_rei_CI = lang_rei_CI(nprev = strat_size_summ, kprev = n_pos_summ, 
                                   nsens = positives, ksens = true_positives, 
                                   nspec = negatives, kspec = true_negatives), 
         lang_rei_CI_lower = lang_rei_CI[[1]],
         lang_rei_CI_upper = lang_rei_CI[[2]], 
         rogan_gladen = ifelse(rogan_gladen < 0, 0, rogan_gladen)
  ) %>% 
  select(strat_size_summ, n_pos_summ, raw_prev, raw_wilsonCI_lower, raw_wilsonCI_upper, 
         weighted_prev, weighted_wilsonCI_lower, weighted_wilsonCI_upper,
         rogan_gladen, lang_rei_CI_lower, lang_rei_CI_upper)  %>% 
  add_column(variable = "Total", .before = 0)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Combine and print table ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

igm_pos_table_print <- 
  igm_pos_table_overall %>% 
  bind_rows(igm_pos_table_sex) %>% 
  bind_rows(igm_pos_table_age) %>% 
  mutate(across(.cols = c(4:12), .fns = ~ 100 * .x)) %>% # to percentages
  mutate(across(.cols = c(4:12), .fns = ~ format(.x, digits = 3))) %>% 
  transmute(
    Group = variable, 
    n = strat_size_summ, 
    Seropos. = n_pos_summ,
    Crude = paste0(raw_prev, "%", " (", raw_wilsonCI_lower, " - ", raw_wilsonCI_upper, ")"),
    Weighted = paste0(weighted_prev, "%", " (", weighted_wilsonCI_lower, " - ", weighted_wilsonCI_upper, ")"),
    `Weighted,\ntest-adjusted` = paste0(rogan_gladen, "%", " (", lang_rei_CI_lower, " - ", lang_rei_CI_upper, ")")
  ) %>% 
  #mutate(`Weighted,\ntest-adjusted` = " ") %>% 
  huxtable() %>% 
  set_contents(1, 1:3, c("", "", "") ) %>% 
  insert_row("", "n", "Seropos.", "Seroprevalence (95% confidence interval)", "", "", after = 0) %>% 
  merge_cells(1, 4:6) %>% 
  set_bold(1, col = everywhere) %>% 
  theme_basic() %>% 
  set_latex_float("h!") %>% 
  set_all_padding(0.5) 



