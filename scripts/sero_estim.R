
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
  

# PanBio IgG validation from https://www.sciencedirect.com/science/article/pii/S1386653220303875#bib0060
positives <- 82 
negatives <- 150
true_positives <- 75
false_positives <- 1
false_negatives <- positives - true_positives
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
#~  Sex section  ----
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

igg_pos_table_overall %>% 
  bind_rows(igg_pos_table_sex) %>% 
  bind_rows(igg_pos_table_age) %>% 
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
  merge_cells(1, 4:6) %>% 
  set_bold(1, col = everywhere) %>% 
  theme_basic() %>% 
  quick_docx()




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
            weighted_prev = sum(prev * strat_size_dhs)/sum(strat_size_dhs)
            )
  
yao %>% 
   group_by(cat_sex, cat_age, cat_igg_result) %>% 
  summarise(n = n() )


  
  
         pos = sum(cat_igg_result == "Positive"), 
         prev = pos/weights)

# 
#  igg_pos_table %>% 
#   rename(Age = cat_age, 
#          No. = n_samples,
#          `No. Seropositive` = n_positive,
#          `Population-weighted seroprevalence`)
#   huxtable()
# 
# 
# 
# 
# 
# 
# #pos_age_sex <- 
# yao %>% 
#   filter(!is.na(cat_igm_result)) %>% # remove 37 people who refused a test
#   group_by(cat_sex, cat_age, cat_igm_result) %>% 
#   count() %>% 
#   mutate(cat_sex = str_to_lower(cat_sex)) %>% 
#   group_by(cat_age, cat_sex) %>% 
#   mutate(prop_pos = n/sum(n)) %>% 
#   group_by(cat_sex, cat_age) %>% 
#   mutate(n = sum(n)) %>% 
#   filter(cat_igm_result == "Positive") %>% 
#   pivot_wider(id_cols = cat_age, names_from = c(cat_sex), values_from = c(n, prop_pos)) %>% 
#   left_join(yaounde_dhs_age_sex) %>% 
#   mutate(prop_pos_raw = ((prop_pos_female * n_female) + (prop_pos_male * n_male))/(n_male + n_female), 
#          prop_pos_weighted = ((prop_pos_female * n_female_dhs) + (prop_pos_male * n_male_dhs))/(n_female_dhs + n_male_dhs)) %>% 
#   # see https://pubmed.ncbi.nlm.nih.gov/24416798/ for formula
#   mutate(rogan_gladen = (prop_pos_weighted + spec - 1)/(spec + sens - 1)) %>% 
#   # see equation 10 in https://pubmed.ncbi.nlm.nih.gov/24416798/ for formula
#   mutate(agresti_coull_var = (1/(sens + spec - 1)) * sqrt((prop_pos_raw*(1-prop_pos_raw)/(n_female + n_male)))) %>%
#   mutate(lower_int = rogan_gladen - 1.96 * agresti_coull_var, 
#          upper_int = rogan_gladen + 1.96 * agresti_coull_var) %>% 
#   mutate(prop_pos_raw = prop_pos_raw * 100, 
#          prop_pos_weighted = prop_pos_weighted * 100, 
#          rogan_gladen = rogan_gladen * 100, 
#          lower_int = lower_int * 100, 
#          upper_int = upper_int * 100,
#          lower_int = format(lower_int, digits = 4), 
#          upper_int = format(upper_int, digits = 4),
#          conf_int = paste0(lower_int, " - ", upper_int), 
#          n_samples = n_female + n_male, 
#          n_positive = prop_pos_female * n_female + prop_pos_male * n_male, 
#          wilsonCI  = BinomCI(n_positive, n_samples, method = "wilson")) %>% 
#   group_by(cat_age) %>% 
#   mutate(lang_rei_CI = lang_rei_CI(nprev = n_samples, kprev = n_positive, 
#                                    nsens = positives, ksens = true_positives, 
#                                    nspec = negatives, kspec = true_negatives)
#   ) %>% 
#   select(cat_age,n_samples, n_positive, prop_pos_raw, 
#          wilsonCI, lang_rei_CI)
# 
# 
# yao %>% 
#   group_by(cat_sex, cat_age, cat_pos) %>% 
#   summarise(n = n() )
# 
# 
# 
# 
# pivot_wider(id_cols = c(cat_age), names_from = cat_pos, values_from = n) %>% 
#   ungroup() %>% 
#   mutate(Total = Positive + Negative, 
#          prop_pos = Positive/Total)
#     
#   mutate(prop_pos = n/sum(n)) %>% 
#   filter(cat_pos == "Positive")
