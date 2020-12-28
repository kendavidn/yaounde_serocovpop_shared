
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Regression subset  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# regression is only run on folks who took the test.
yao_regn <- 
  yao %>% 
  # drop folks who didn't do any test
  filter(!is.na(cat_pos_num)) %>% 
  # drop folks with missing values for regressors
  filter(!is.na(is_breadwin)) %>% 
  filter(!is.na(cat_BMI)) %>% 
  # replace NA
  mutate(mcat_chronic = replace_na(mcat_chronic, "None")) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# baseline risk for converting from odds ratio to relative risk
base_risk <- 
  tabyl(yao_regn$cat_pos_num) %>% 
  pull(percent) %>% 
  .[2]

count_per_group <- 
  function(df, col) {
  df %>%
    group_by({{ col }}) %>%
    summarise(n = n(),
              pos = sum(cat_pos_num)) %>%
    mutate(pct = pos / n,
           pct = 100 * pct,
           pct = round(pct, 1),
           pos_pct = paste0(pos, " (", pct, ")")) %>% 
    rename("labels" = {{ col }})
}


regn_per_group <- 
  function(df, col, the_formula = NULL, col_string = NULL) {
  
    if(is.null(col_string)) {
      col_string <- deparse(substitute(col))
    }
  
  if(is.null(the_formula)) {
    the_formula <- paste("cat_pos_num ~", col_string, "+ (1 | id_hhld)") %>% as.formula()
  }
  # run regn and clean output
  df %>% 
    glmer(formula = the_formula,
          data = .,
          family = binomial(),
          control = glmerControl(optimizer = "bobyqa")) %>% 
      broom.mixed::tidy() %>% 
      filter(row_number()!= 1 & row_number()!= nrow(.) ) %>%  # drop first and last rows
      mutate(estimate = exp(estimate),
             upper_CI = estimate + 1.96 * std.error,
             lower_CI = estimate - 1.96 * std.error, 
             avg_rel_risk = estimate/( 1 - base_risk + ( base_risk * estimate) ),
             signif = (upper_CI > 1 & lower_CI > 1) | (upper_CI < 1 & lower_CI < 1)
      ) %>% 
      mutate(estimate = format(estimate, digits = 2 ), 
             upper_CI = format(upper_CI, digits = 2 ),
             lower_CI = round(lower_CI, digits = 2),
             lower_CI = format(lower_CI, digits = 2 ), 
             avg_rel_risk = format(avg_rel_risk, digits = 2 )
      ) %>% 
      mutate(labels = str_replace_all(term, col_string, "")) %>% 
      mutate(estimate_and_CI = paste0(estimate, " (", lower_CI, " - ", upper_CI, ")")) %>% 
      select(labels, estimate_and_CI, lower_CI, estimate, upper_CI, avg_rel_risk, signif, p.value)
}

clean_count_and_regn <- 
  function(df, label, group){
  df %>% 
    bind_rows(c(labels = paste0("**", label ,"**") )) %>% 
    arrange(labels != paste0("**", label ,"**") ) %>% 
    mutate(estimate = as.numeric(estimate),
           lower_CI = as.numeric(lower_CI), 
           upper_CI = as.numeric(upper_CI)) %>% 
    mutate(rowid = rev(1:nrow(.)), 
           group = group) %>% 
    mutate(min.p.value = min(p.value, na.rm = T)) 
    
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Gender ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sex_count <- yao_regn %>% count_per_group(cat_sex)
sex_regn <- yao_regn %>% regn_per_group(cat_sex)

sex_count_and_regn <- 
  sex_count %>% 
  left_join(sex_regn) %>% 
  clean_count_and_regn(label = "Sex", group ="cat_sex")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Ages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

age_count <- yao_regn %>% count_per_group(cat_age)

age_regn <- 
  yao_regn %>% 
  # set reference level for regressions
  mutate(cat_age = factor(cat_age, levels = c( "30 - 44", "5 - 14", "15 - 29", "45 - 64", "65 +") )) %>% 
  regn_per_group(cat_age)
  
age_count_and_regn <- 
  age_count %>% 
  left_join(age_regn) %>% 
  clean_count_and_regn(label = "Age", group = "cat_age") %>% 
  # reorder for table
  mutate(labels = factor(labels, levels = c("**Age**" ,"5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  arrange(labels)
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Education  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

educ_count <- yao_regn %>% count_per_group(cat_educ)
educ_regn <- yao_regn %>% regn_per_group(cat_educ)
educ_count_and_regn <- 
  educ_count %>% 
  left_join(educ_regn) %>% 
  clean_count_and_regn(label = "Highest education level", group = "cat_educ")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  BMI classes  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BMI_count <- yao_regn %>% count_per_group(cat_BMI)
BMI_regn <- yao_regn %>% regn_per_group(cat_BMI)

BMI_count_and_regn <- 
  BMI_count %>% 
  left_join(BMI_regn) %>% 
  clean_count_and_regn(label = "BMI group", group = "cat_BMI") %>% 
  # reorder for table
  mutate(labels = factor(labels, levels = c("**BMI group**", "\\< 18.5 (Underweight)", "18.5 - 24.9", "25 - 30 (Overweight)", " \\> 30 (Obese)"))) %>% 
  arrange(labels)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Contact with traveller  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

contact_traveller_count <- yao_regn %>% count_per_group(has_contact_traveller)
contact_traveller_regn <- yao_regn %>% regn_per_group(has_contact_traveller)

contact_traveller_count_and_regn <- 
  contact_traveller_count %>% 
  left_join(contact_traveller_regn) %>% 
  clean_count_and_regn(label = "Recent contact with an international traveller", 
                       group = "has_contact_traveller")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Contact with traveller  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

contact_COVID_count <- yao_regn %>% count_per_group(has_contact_COVID)
contact_COVID_regn <- yao_regn %>% regn_per_group(has_contact_COVID)

contact_COVID_count_and_regn <- 
  contact_COVID_count %>% 
  left_join(contact_COVID_regn) %>% 
  clean_count_and_regn(label = "Contact with suspected/confirmed COVID patient", 
                       group = "has_contact_COVID")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Contact with COVID-19 patient  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

chronic_count <- yao_regn %>% count_per_group(has_chronic)
chronic_regn <- yao_regn %>% regn_per_group(has_chronic)

chronic_count_and_regn <- 
  chronic_count %>% 
  left_join(chronic_regn) %>% 
  clean_count_and_regn(label = "Do you have any chronic conditions?", group = "has_chronic")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Occupation  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

occup_count_and_regn <- function(df, col, occup_name) {
  col_string <- deparse(substitute(col))

  occup_count <-
    df %>%
    mutate("{{ col }}" := if_else({{ col }} == 1, "Yes", "No")) %>%
    count_per_group({{ col }}) %>% 
    .[2, ]

  occup_regn <-
    df %>%
    mutate("{{ col }}" := if_else({{ col }} == 1, "Yes", "No")) %>%
    regn_per_group({{ col }},
      the_formula = as.formula(paste("cat_pos_num ~", col_string, "+ (1 | id_hhld)")),
      col_string = col_string) %>% 
    select(-labels)

  occup_count_and_regn <-
    occup_count %>%
    bind_cols(occup_regn) %>% 
    mutate(labels = occup_name) %>% 
    mutate(estimate = as.numeric(estimate),
           lower_CI = as.numeric(lower_CI), 
           upper_CI = as.numeric(upper_CI)) %>% 
    rename("labels" = 1 )

  return(occup_count_and_regn)
}

student_section <- yao_regn %>% occup_count_and_regn(is_occup_student, "Student") 
trade_section <- yao_regn %>% occup_count_and_regn(is_occup_trade, "Small trader") 
business_section <- yao_regn %>% occup_count_and_regn(is_occup_business, "Businessperson") 
homemaker_section <- yao_regn %>% occup_count_and_regn(is_occup_homemaker, "Home-maker") 
unemp_section <- yao_regn %>% occup_count_and_regn(is_occup_unemp, "Unemployed") 
salarywork_section <- yao_regn %>% occup_count_and_regn(is_occup_salarywork, "Salary worker") 
retiree_section <- yao_regn %>% occup_count_and_regn(is_occup_retiree, "Retired") 
other_section <- yao_regn %>% occup_count_and_regn(is_occup_other, "Other") 
agric_section <- yao_regn %>% occup_count_and_regn(is_occup_agric, "Farmer") 

occup_section <- 
  as_tibble(data.frame(labels = "**Occupation**")) %>% 
  bind_rows(list(student_section, 
                 trade_section, 
                 business_section, 
                 homemaker_section, 
                 unemp_section, 
                 salarywork_section 
                 # retiree_section, # fewer than 10 positives
                 # other_section, 
                 # agric_section
                 )) %>% 
  mutate(group = "mcat_occup") %>% 
  mutate(min.p.value = min(p.value, na.rm = T)) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Is breadwinner  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

breadwin_count <- 
  yao_regn %>% 
  count_per_group(is_breadwin)

breadwin_regn <- 
  yao_regn %>%
  regn_per_group(is_breadwin)

breadwin_count_and_regn <- 
  breadwin_count %>% 
  left_join(breadwin_regn) %>% 
  clean_count_and_regn(label = "Are you the principal breadwinner?", group = "is_breadwin")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Respect of preventive measures  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

distancing_count <- 
  yao_regn %>% 
  filter(is_respecting_distancing %in% c("Definitely yes", "Definitely not", "Partly")) %>% 
  count_per_group(is_respecting_distancing)

distancing_regn <- 
  yao_regn %>%
  filter(is_respecting_distancing %in% c("Definitely yes", "Definitely not", "Partly")) %>% 
  regn_per_group(is_respecting_distancing)


distancing_count_and_regn <- 
  distancing_count %>% 
  left_join(distancing_regn) %>% 
  clean_count_and_regn(label = "Have you followed social distancing rules?", group = "is_breadwin")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Household area ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hhld_area_count <- 
  yao_regn %>% 
  count_per_group(loc_hhld_area) %>% 
  mutate(labels = fct_reorder(labels, pct)) %>% 
  arrange(labels)

hhld_area_regn <- 
  yao_regn %>%
  group_by(loc_hhld_area) %>% 
  mutate(pos_frac = sum(cat_pos_num)/sum(counter)) %>% 
  ungroup() %>% 
  mutate(loc_hhld_area = fct_reorder(loc_hhld_area, pos_frac)) %>% 
  regn_per_group(loc_hhld_area)


hhld_area_count_and_regn <- 
  hhld_area_count %>% 
  left_join(hhld_area_regn) %>% 
  clean_count_and_regn(label = "Health zone", group = "loc_hhld_area")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Number in household ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat_n_hhld_indiv_count <- 
  yao_regn %>% 
  count_per_group(cat_n_hhld_indiv) %>% 
  arrange(labels)

cat_n_hhld_indiv_regn <- 
  yao_regn %>%
  regn_per_group(cat_n_hhld_indiv)


cat_n_hhld_indiv_count_and_regn <- 
  cat_n_hhld_indiv_count %>% 
  left_join(cat_n_hhld_indiv_regn) %>% 
  clean_count_and_regn(label = "Number of household members", group = "cat_n_hhld_indiv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Hhld with Children? ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

has_hhld_children_count <- 
  yao_regn %>% 
  count_per_group(has_hhld_children) %>% 
  arrange(labels)

has_hhld_children_regn <- 
  yao_regn %>%
  regn_per_group(has_hhld_children)

has_hhld_children_count_and_regn <- 
  has_hhld_children_count %>% 
  left_join(has_hhld_children_regn) %>% 
  clean_count_and_regn(label = "Are there children in the household?", group = "has_hhld_children")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Combine and plot  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_sections_univariate <- 
  sex_count_and_regn %>% 
  bind_rows(age_count_and_regn) %>% 
  bind_rows(educ_count_and_regn)  %>% 
  bind_rows(BMI_count_and_regn)  %>% 
  bind_rows(contact_traveller_count_and_regn) %>% 
  bind_rows(contact_COVID_count_and_regn) %>% 
  bind_rows(chronic_count_and_regn) %>% 
  bind_rows(occup_section) %>% 
  bind_rows(breadwin_count_and_regn) %>% 
  bind_rows(distancing_count_and_regn) %>% 
  bind_rows(hhld_area_count_and_regn) %>% 
  bind_rows(cat_n_hhld_indiv_count_and_regn) %>% 
  bind_rows(has_hhld_children_count_and_regn) %>% 
  mutate(labels = fct_inorder(labels)) %>% 
  # Add "reference" to reference rows
  mutate(estimate_and_CI = if_else(labels %in% c("30 - 44", # age
                                                 "18.5 - 24.9", # BMI
                                                 "No contact with traveller",
                                                 "No COVID contact",
                                                 "No formal instruction", 
                                                 "Female", 
                                                 "Not breadwinner",
                                                 "Definitely yes", # followed distancing rules
                                                 "No comorbidity",
                                                 "Cité Verte", 
                                                 "1 - 2", 
                                                 "No children"),
                                   "*Reference*",
                                   estimate_and_CI
                                   )) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Filter out p-value not lower than 0.1 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  group_by(group) %>% 
  filter(min.p.value < 0.1 | group == "cat_age") %>% 
  ungroup() %>% 
  # row ids
  mutate(rowid = rev(1:nrow(.)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ Multivariate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

signif_vars <- paste(unique(all_sections_univariate$group), "+", collapse = " ")

all_regn <-
  yao_regn %>%
  # set reference level for regressions
  glmer(formula = paste("cat_pos_num ~", signif_vars, "(1 | id_hhld)"),
        data = .,
        family = binomial(),
        control = glmerControl(optimizer = "bobyqa")) %>%
  broom.mixed::tidy() %>%
  filter(row_number()!= 1 & row_number()!= nrow(.) ) %>%
  mutate(labels = str_replace(term, paste(unique(all_sections_univariate$group), collapse = "|"), "")) %>%
  select(labels, estimate, std.error, p.value) %>% 
  mutate(estimate = exp(estimate),
         upper_CI = estimate + 1.96 * std.error,
         lower_CI = estimate - 1.96 * std.error, 
         avg_rel_risk = estimate/( 1 - base_risk + ( base_risk * estimate) ),
         signif = (upper_CI > 1 & lower_CI > 1) | (upper_CI < 1 & lower_CI < 1)) %>% 
  mutate(estimate = format(estimate, digits = 2 ), 
         upper_CI = format(upper_CI, digits = 2 ),
         lower_CI = round(lower_CI, digits = 2),
         lower_CI = format(lower_CI, digits = 2 ), 
         avg_rel_risk = format(avg_rel_risk, digits = 2 )) %>% 
  mutate(estimate_and_CI = paste0(estimate, " (", lower_CI, " - ", upper_CI, ")")) %>% 
  select(labels, 
         multi_estimate_and_CI = estimate_and_CI,
         multi_avg_rel_risk = avg_rel_risk, 
         multi_signif = signif, 
         multi_p.value = p.value)


all_sections <- 
  all_sections_univariate %>% 
  left_join(all_regn) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# change text size
update_geom_defaults("text", list(size = 3))
update_geom_defaults("richtext", list(size = 3))
theme_set(theme_void())



table_sec1 <-
  all_sections %>%
  mutate(pos_over_n = ifelse(!is.na(n), paste0(pos, " / ", n ), NA )) %>% 
  ggplot() +
  geom_richtext(data = subset(all_sections,  is.na(n)), aes(y = labels, x = 0, label = labels), hjust = 0, fill = NA, label.color = NA) +
  geom_richtext(data = subset(all_sections, !is.na(n)), aes(y = labels, x = 0.2, label = labels), hjust = 0, fill = NA, label.color = NA) +
  geom_text(aes(y = labels, x = 3.5, label = n)) +
  geom_text(aes(y = labels, x = 4.5, label = pos)) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Variable", hjust = 0, fontface = "bold", size = 3.3) +
  annotate("text", x = 3.5, y = max(all_sections$rowid) + 1, label = "n", hjust = 0.5, fontface = "bold", size = 3.3) +
  annotate("text", x = 4.5, y = max(all_sections$rowid) + 1, label = "Positive", hjust = 0.5, fontface = "bold", size = 3.3) +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = c(0.5, 2))) +
  scale_x_continuous(expand = expansion(add = c(0.5, 0.5)))



positive_barplot <- 
  all_sections %>% 
  mutate(pct_fill = ifelse(!is.na(pct), 100, NA)) %>% 
  ggplot() + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_col(aes(y = labels, x = pct_fill), fill = alpha(my_green, 0.2)) + 
  geom_col(aes(y = labels, x = pct), fill = my_green) + 
  geom_text(aes(y = labels, x = pct, label = pct), color = my_darkgreen, size = 3, hjust = -0.2 ) +
  annotate("text", x = 50, y = max(all_sections$rowid) + 1, label = "% Positive",  fontface = "bold", size = 3.3, hjust = 0.5) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "") + #placeholder
  scale_y_discrete(limits = rev(all_sections$labels), 
                   expand = expansion(add = c(0.5, 2))) + 
  scale_x_continuous(expand = expansion(add = c(30, 40))) + 
  theme_void()

table_sec2 <-
  all_sections %>%
  ggplot() +
  geom_richtext(aes(y = labels, x = 0, label = estimate_and_CI), hjust = 0.5, fill = NA, label.color = NA) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "OR (95% CI)", hjust = 0.5, fontface = "bold", size = 3.3) +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = c(0.5, 2))) +
  scale_x_continuous(expand = expansion(add = c(0.06, 0.1)))


OR_plot_sec <- 
  all_sections %>% 
  ggplot() +
  scale_x_continuous(trans = pseudolog10_trans, 
                     expand = expansion(add = c(0, 0.1)), 
                     breaks = c(1, 2, 4, 8)) + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_pointrange(aes(y = labels, x = estimate, xmin = lower_CI, xmax = upper_CI), 
                  shape = 22, fill = "black", color = "black", size = 0.3) + 
  geom_point(data = subset(all_sections, signif == TRUE), 
             aes(y = labels, x = upper_CI), position = position_nudge(x = 0.1), shape = 8, size = 1.8) + 
  annotate("text", x = 1, y = max(all_sections$rowid) + 1, label = "OR plot",  fontface = "bold", size = 3.3, hjust = 0.3) +
  theme_void() + 
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "") + #placeholder
  annotate("segment", x = 1, xend = 1,  y = 0.5, yend =  max(all_sections$rowid) + 0.5 , linetype = "dashed") +
  scale_y_discrete(limits = rev(all_sections$labels), 
                   expand = expansion(add = c(0.5, 2))) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.line.x = element_line(), 
        axis.ticks.x = element_line(size = 0.1))


table_sec3 <-
  all_sections %>%
  ggplot() +
  geom_text(aes(y = labels, x = 0, label = avg_rel_risk), hjust = 0.5) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "ARR", hjust = 0.5,  fontface = "bold", size = 3.3) +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = c(0.5, 2))) +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.3)))




patchwork::wrap_plots(table_sec1,
                      positive_barplot, 
                      table_sec2,
                      OR_plot_sec, 
                      table_sec3, 
                      nrow = 1, widths = c(4.7,2,2,1.5,1))






