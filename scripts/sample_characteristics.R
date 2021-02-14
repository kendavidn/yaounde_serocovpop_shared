
# hhld_head <- 
#   yao %>% 
#   # summarise
#   group_by(cat_igg_result) %>% 
#   summarise(n = sum(is_head == "Yes")) %>% 
#   ungroup() %>% 
#   # add total 
#   add_row(cat_igg_result = "Total",
#           n = sum(.$n)) %>% 
#   # transform
#   #mutate(Characteristic = "Household heads, n (%)") %>% 
#   mutate(Characteristic = "Num. household heads") %>% 
#   pivot_wider(id_cols = Characteristic, values_from = n, names_from = cat_igg_result) %>% 
#   mutate(across(.fns = as.character))
#   
yao_NA_replaced <- 
  yao %>% 
  mutate(cat_igg_result = ifelse(is.na(cat_igg_result), "Refused Test", cat_igg_result))


mean_age <- 
  yao_NA_replaced %>% 
  # summarise
  group_by(cat_igg_result) %>% 
  summarise(mean_age = format(mean(val_age), digits = 3 ) , 
            sd = format(sd(val_age), digits = 3), 
            mean_sd = paste0(mean_age, " (", sd, ")")
  ) %>% 
  ungroup() %>% 
  # add total
  add_row(cat_igg_result = "Total",
          mean_age = format(mean(yao_NA_replaced$val_age), digits = 3 ), 
          sd = format(sd(yao_NA_replaced$val_age), digits = 3)) %>% 
  mutate(mean_sd = ifelse(cat_igg_result == "Total", 
                          paste0(mean_age, " (", sd, ")"),
                          mean_sd)) %>% 
  # transform
  mutate(Characteristic = "Mean Age (SD)") %>% 
  pivot_wider(id_cols = Characteristic, values_from = mean_sd, names_from = cat_igg_result)


BMI_row <- 
  yao_NA_replaced %>% 
  # summarise
  group_by(cat_igg_result) %>% 
  summarise(mean_BMI = format(mean(val_BMI), digits = 3 ) , 
            sd = format(sd(val_BMI), digits = 3), 
            mean_sd = paste0(mean_BMI, " (", sd, ")")
  ) %>% 
  ungroup() %>% 
  # add total
  add_row(cat_igg_result = "Total",
          mean_BMI = format(mean(yao_NA_replaced$val_BMI), digits = 3 ), 
          sd = format(sd(yao_NA_replaced$val_BMI), digits = 3)) %>% 
  mutate(mean_sd = ifelse(cat_igg_result == "Total", 
                          paste0(mean_BMI, " (", sd, ")"),
                          mean_sd)) %>% 
  # transform
  mutate(Characteristic = "Mean BMI (SD)") %>% 
  pivot_wider(id_cols = Characteristic, values_from = mean_sd, names_from = cat_igg_result)


tabulate_categories <- function(df, col) {
  df %>%
    # count
    group_by(cat_igg_result, {{ col }}) %>%
    #count(id_ind) %>%
    count() %>%
    rename(nn = n) %>% 
    ungroup() %>%
    # add percentages
    group_by(cat_igg_result) %>%
    mutate(
      pct = 100 * nn / sum(nn),
      pct = round(pct, 1)
    ) %>%
    ungroup() %>%
    # add total
    add_row(cat_igg_result = "Total") %>%
    complete(cat_igg_result, {{ col }}) %>%
    group_by({{ col }}) %>%
    mutate(nn = ifelse(cat_igg_result == "Total",
                       sum(nn, na.rm = T), nn)) %>%
    group_by(cat_igg_result) %>%
    mutate(pct = ifelse(cat_igg_result == "Total",
                        round(100 * nn / sum(nn, na.rm = T), 1),
                        pct)) %>%
    filter(!is.na({{ col }})) %>%
    {
      suppressWarnings(mutate(., across(.fns = ~ replace_na(.x, 0))))
    } %>%
    # paste count and percentages
    mutate(nn = format(nn, digits = 3)) %>%
    mutate(nn_pct = paste0(nn, " (", pct, "%)")) %>%
    # transform
    select(Characteristic = {{ col }},
           Sex = cat_igg_result,
           nn_pct) %>%
    pivot_wider(id_cols = Characteristic, values_from = nn_pct, names_from = Sex) %>%
    mutate(across(.fns = as.character))
}

sex_count <- 
  yao_NA_replaced %>% 
  # count
  tabulate_categories(cat_sex)


age_groups <- 
  yao_NA_replaced %>% 
  # count
  tabulate_categories(cat_age)


education_levels <- 
  yao_NA_replaced %>% 
  mutate(cat_educ = recode(cat_educ, "No response" = "Other")) %>% 
  mutate(cat_educ = fct_infreq(cat_educ), 
         cat_educ = fct_relevel(cat_educ, "Other", after = Inf)) %>% 
  tabulate_categories(cat_educ)


professions <- 
  yao_NA_replaced %>% 
  # separate
  select(cat_igg_result, id_ind, mcat_occup) %>% 
  separate_rows(mcat_occup, sep = "--") %>% 
  mutate(mcat_occup = replace_na(mcat_occup, "Other")) %>% 
  mutate(mcat_occup = ifelse(str_detect(mcat_occup, "Other"), "Other", mcat_occup)) %>% 
  mutate(mcat_occup = str_replace_all(mcat_occup, "No response", "Other")) %>% 
  mutate(mcat_occup = str_replace_all(mcat_occup, "Farmer", "Other")) %>% 
  #mutate(mcat_occup = recode(mcat_occup, "Farmer" = "Other")) %>% 
  mutate(mcat_occup = as.character(mcat_occup)) %>% 
  mutate(mcat_occup = fct_infreq(mcat_occup), 
         mcat_occup = fct_relevel(mcat_occup, "Other", after = Inf)) %>% 
  tabulate_categories(mcat_occup)
  
  
chronic_illnesses <- 
  yao_NA_replaced %>% 
  # separate
  select(cat_igg_result, id_ind, mcat_chronic) %>% 
  separate_rows(mcat_chronic, sep = "--") %>% 
  mutate(mcat_chronic = recode(mcat_chronic, "Other chronic illness" = "Other")) %>% 
  mutate(mcat_chronic = recode(mcat_chronic, "Cardiovascular illness" = "Other")) %>% 
  mutate(mcat_chronic = recode(mcat_chronic, "Chronic neurologic disease" = "Other")) %>% 
  mutate(mcat_chronic = recode(mcat_chronic, "HIV" = "Other")) %>% 
  mutate(mcat_chronic = recode(mcat_chronic, "Cancer" = "Other")) %>% 
  mutate(mcat_chronic = recode(mcat_chronic, "No response" = "Other")) %>% 
  mutate(mcat_chronic = recode(mcat_chronic, "Tuberculosis" = "Other")) %>% 
  mutate(mcat_chronic = fct_infreq(mcat_chronic), 
         mcat_chronic = fct_relevel(mcat_chronic, "Other", after = Inf)) %>% 
  # count
  tabulate_categories(mcat_chronic) 

# 
# sample_characteristics_table <- 
#   #  bind_rows(hhld_head) %>% 
#   mean_age %>% 
#   bind_rows(BMI_row) %>% 
#   add_row(Characteristic  = "Sex, n (% of sero-category in each sex group)") %>% 
#   bind_rows(sex_count) %>%
#   add_row(Characteristic  = "Age groups, n (% of sero-category in each age group)") %>% 
#   bind_rows(age_groups) %>% 
#   add_row(Characteristic  = "Education Level, n (% of sero-category who completed this level)") %>% 
#   bind_rows(education_levels) %>% 
#   add_row(Characteristic  = "Profession, n (% of sero-category identifying as this profession)") %>% 
#   bind_rows(professions) %>% 
#   add_row(Characteristic  = "Chronic conditions, n (% of sero-category with each condition)") %>% 
#   bind_rows(chronic_illnesses) %T>% 
#   {which(is.na(.$Negative)) ->> rows_to_merge } %>% 
#   # possibly temp
#   select(Characteristic, Total) %>% 
#   huxtable() %>% 
#   theme_basic() %>% 
#   merge_across(row = rows_to_merge + 1) %>% 
#   set_italic(row = c(rows_to_merge + 1), col = 1) %>% 
#   set_latex_float("h!") %>% 
#   set_all_padding(0.5) 


sample_characteristics_table <- 
  #  bind_rows(hhld_head) %>% 
  mean_age %>% 
  bind_rows(BMI_row) %>% 
  add_row(Characteristic  = "Sex, n (%)") %>% 
  bind_rows(sex_count) %>%
  add_row(Characteristic  = "Age groups, n (%)") %>% 
  bind_rows(age_groups) %>% 
  add_row(Characteristic  = "Education Level, n (%)") %>% 
  bind_rows(education_levels) %>% 
  add_row(Characteristic  = "Profession, n (%)") %>% 
  bind_rows(professions) %>% 
  add_row(Characteristic  = "Chronic conditions, n (%)") %>% 
  bind_rows(chronic_illnesses) %T>% 
  {which(is.na(.$Negative)) ->> rows_to_merge } %>% 
  # possibly temp
  select(Characteristic, Total) %>% 
  rename(Count = Total) %>% 
  separate(Count, into = c("N", "%"), sep = "\\(") %>% 
  huxtable() %>% 
  theme_basic() %>% 
  merge_across(row = rows_to_merge + 1) %>% 
  set_italic(row = c(rows_to_merge + 1), col = 1) %>% 
  set_latex_float("h!") %>% 
  set_all_padding(0.5) 
