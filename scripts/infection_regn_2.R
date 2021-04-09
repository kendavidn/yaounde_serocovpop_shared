
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
  mutate(mcat_chronic = replace_na(mcat_chronic, "None")) %>% 
  # relevel covariable
  mutate(cat_n_hhld_indiv = relevel(cat_n_hhld_indiv, "3 - 5"))



sample_for_infection_regn <- nrow(yao_regn)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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


### WE MIGHT COME BACK AND FIX THIS LATER. THE TABLE POSSIBLY SHOULD BE BASED ON THE CORRECTED SEROPREV

# baseline risk for converting from odds ratio to relative risk
# base_risk <- 
#   tabyl(yao_regn$cat_pos_num) %>% 
#   pull(percent) %>% 
#   .[2]

count_per_group <- 
  function(df, col) {
    df %>%
      group_by({{ col }}) %>%
      summarise(n = n(),
                pos = sum(cat_pos_num)) %>%
      mutate(pct = pos / n,
             #pct = (pct + spec - 1)/(spec + sens - 1), # rogan_gladen correction
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
      the_formula <- paste("cat_pos_num ~", col_string, "+ (1 | id_hhld)") 
    }
    
    # run regn and clean output
    model <- df %>% 
      glmer(formula = the_formula,
            data = .,
            family = binomial(),
            control = glmerControl(optimizer = "bobyqa"))
    
    wald_chi_square_p_val <- 
      car::Anova(model) %>% 
      data.frame() %>% 
      .[1,3]
    
    model %>% 
      broom.mixed::tidy() %>%
      filter(row_number()!= 1 & row_number()!= nrow(.) ) %>%  # drop first and last rows
      mutate(upper_CI = estimate + 1.96 * std.error,
             lower_CI = estimate - 1.96 * std.error,
             estimate = exp(estimate),
             upper_CI = exp(upper_CI),
             lower_CI = exp(lower_CI),
             signif = (upper_CI > 1 & lower_CI > 1) | (upper_CI < 1 & lower_CI < 1)) %>%
      mutate(estimate = format(estimate, digits = 2 ),
             estimate = as.numeric(estimate),
             upper_CI = format(upper_CI, digits = 2 ),
             upper_CI = as.numeric(upper_CI),
             lower_CI = round(lower_CI, digits = 2),
             lower_CI = format(lower_CI, digits = 2 ),
             lower_CI = as.numeric(lower_CI)) %>%
      mutate(labels = str_replace_all(term, col_string, "")) %>%
      mutate(estimate_and_CI = paste0(estimate, " (", lower_CI, " - ", upper_CI, ")")) %>%
      mutate(wald_chi_square_p_val = wald_chi_square_p_val) %>% 
      select(labels, estimate_and_CI, lower_CI, estimate, upper_CI, signif, p.value, 
             wald_chi_square_p_val)
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
      mutate(min.p.value = min(p.value, na.rm = T)) %>% 
      mutate(wald_chi_square_p_val = first(na.omit(wald_chi_square_p_val)))
    
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
age_regn <- yao_regn %>% regn_per_group(cat_age)

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

educ_count <- 
  yao_regn %>% 
  mutate(cat_educ = recode(cat_educ, "No response" = "Other")) %>% 
  count_per_group(cat_educ)

educ_regn <- 
  yao_regn %>% 
  mutate(cat_educ = recode(cat_educ, "No response" = "Other")) %>% 
  regn_per_group(cat_educ)

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
#~  Contact with traveler  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

contact_traveler_count <- yao_regn %>% count_per_group(has_contact_traveler)
contact_traveler_regn <- yao_regn %>% regn_per_group(has_contact_traveler)

contact_traveler_count_and_regn <- 
  contact_traveler_count %>% 
  left_join(contact_traveler_regn) %>% 
  clean_count_and_regn(label = "Contact with international traveler", 
                       group = "has_contact_traveler")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Contact with COVID case  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

contact_COVID_count <- yao_regn %>% count_per_group(has_contact_COVID)
contact_COVID_regn <- yao_regn %>% regn_per_group(has_contact_COVID)

contact_COVID_count_and_regn <- 
  contact_COVID_count %>% 
  left_join(contact_COVID_regn) %>% 
  clean_count_and_regn(label = "Contact with COVID case", 
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
                   the_formula = paste("cat_pos_num ~", col_string, "+ (1 | id_hhld)"),
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
  clean_count_and_regn(label = "Have you followed social distancing rules?", group = "is_respecting_distancing")


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
  mutate(labels = factor(labels, levels = c("1 - 2", "3 - 5", "\\> 5"))) %>% 
  arrange(labels) %>% 
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

plot_only_p_below_0.2 <- TRUE


all_sections_univariate <- 
  sex_count_and_regn %>% 
  bind_rows(age_count_and_regn) %>% 
  bind_rows(educ_count_and_regn)  %>% 
  bind_rows(BMI_count_and_regn)  %>% 
  bind_rows(contact_traveler_count_and_regn) %>% 
  bind_rows(contact_COVID_count_and_regn) %>% 
  bind_rows(chronic_count_and_regn) %>% 
  #bind_rows(occup_section) %>% 
  bind_rows(breadwin_count_and_regn) %>% 
  bind_rows(distancing_count_and_regn) %>% 
  bind_rows(hhld_area_count_and_regn) %>% 
  bind_rows(cat_n_hhld_indiv_count_and_regn) %>% 
  bind_rows(has_hhld_children_count_and_regn) %>% 
  mutate(labels = fct_inorder(labels)) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Filter out p-value not lower than relaxed cutoff
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {if (plot_only_p_below_0.2 == TRUE){
    filter(.,  wald_chi_square_p_val < 0.2 | group == "cat_age")}
    else {.}
  } %>% 
  # row ids
  mutate(rowid = rev(1:nrow(.)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ Multivariate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



vars_to_paste <- unique(all_sections_univariate$group)
vars_to_paste <- vars_to_paste[!vars_to_paste %in% "mcat_occup"]
vars_to_paste <- paste(vars_to_paste, "+", collapse = " ")

the_formula <- paste("cat_pos_num ~", vars_to_paste, "(1 | id_hhld)")



all_regn <-
  yao_regn %>%
  # set reference level for regressions
  glmer(formula = the_formula,
        data = .,
        family = binomial(),
        control = glmerControl(optimizer = "bobyqa")) %>%
  broom.mixed::tidy() %>%
  filter(row_number()!= 1 & row_number()!= nrow(.) ) %>%   # remove intercept and random parameter
  mutate(labels = str_replace(term, paste(unique(all_sections_univariate$group), collapse = "|"), "")) %>%
  select(labels, estimate, std.error, p.value) %>% 
  mutate(upper_CI = estimate + 1.96 * std.error,
         lower_CI = estimate - 1.96 * std.error, 
         estimate = exp(estimate),
         upper_CI = exp(upper_CI),
         lower_CI = exp(lower_CI),
         signif = (upper_CI > 1 & lower_CI > 1) | (upper_CI < 1 & lower_CI < 1)) %>% 
  mutate(estimate = format(estimate, digits = 2 ),
         estimate = as.numeric(estimate),
         upper_CI = format(upper_CI, digits = 2 ),
         upper_CI = as.numeric(upper_CI),
         lower_CI = round(lower_CI, digits = 2),
         lower_CI = format(lower_CI, digits = 2 ),
         lower_CI = as.numeric(lower_CI)) %>% 
  mutate(estimate_and_CI = paste0(estimate, " (", lower_CI, " - ", upper_CI, ")")) %>% 
  select(labels, 
         multi_estimate_and_CI = estimate_and_CI,
         multi_estimate = estimate,
         multi_lower_CI = lower_CI,
         multi_upper_CI = upper_CI,
         multi_signif = signif, 
         multi_p.value = p.value)

all_sections <- 
  all_sections_univariate %>% 
  left_join(all_regn) %>% 
  # Add "reference" to reference rows
  mutate(across(.cols = c(estimate_and_CI, multi_estimate_and_CI), 
                .fns = ~ if_else(labels %in% c("30 - 44", # age
                                               "18.5 - 24.9", # BMI
                                               "No contact with traveler",
                                               "No COVID contact",
                                               "No formal instruction", 
                                               "Female", 
                                               "Not breadwinner",
                                               "Definitely yes", # followed distancing rules
                                               "No comorbidity",
                                               "Cité Verte", 
                                               "3 - 5", # number in household
                                               "No children"),
                                 "*Reference*",
                                 .x )
  ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# change text size
# update_geom_defaults("text", list(size = 3))
# update_geom_defaults("richtext", list(size = 3))
# theme_set(theme_void())

# y axis expansion (common to all plots)

y_axis_expansion <- c(1,3)
# 
# neg_color <- alpha("firebrick3", 0.2) 
# pos_color <- alpha("firebrick3", 0.7)
# text_color <- "firebrick4"


text_color <- "dodgerblue4"
bar_color <- "dodgerblue3"
background_color <- alpha("dodgerblue3", 0.4)

text_color <- "violetred4"
bar_color <- alpha("firebrick3", 0.65)
background_color <- "gray90"



table_sec1 <-
  all_sections %>%
  mutate(pos_over_n = ifelse(!is.na(n), paste0(pos, " / ", n ), NA )) %>% 
  ggplot() +
  # overall group labels
  geom_richtext(data = subset(all_sections,  is.na(n)), 
                aes(y = labels, x = 0, label = labels), 
                family = "Avenir Next", size = 2.7, hjust = 0, 
                fill = NA, label.color = NA) +
  # group subsections
  geom_richtext(data = subset(all_sections, !is.na(n)), 
                aes(y = labels, x = 0.2, label = labels), 
                family = "Avenir Next", size = 3, hjust = 0, 
                fill = NA, label.color = NA) +
  # n count
  geom_text(aes(y = labels, x = 3.3, label = n), size = 3) +
  # pos count
  geom_text(aes(y = labels, x = 4, label = pos), size = 3) +
  # table column names
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, 
           label = "", size = 2.7, hjust = 0, fontface = "bold") +
  annotate("text", x = 3.3, y = max(all_sections$rowid) + 1, 
           label = "n", size = 2.7, hjust = 0.5, fontface = "bold") +
  annotate("text", x = 4, y = max(all_sections$rowid) + 1, 
           label = "Pos.", size = 2.7, hjust = 0.5, fontface = "bold") +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = y_axis_expansion)) +
  scale_x_continuous(expand = expansion(add = c(0.2, 0.2))) + 
  coord_cartesian(clip = "off")


positive_pct_plot <- 
  all_sections %>% 
  mutate(pct_fill = ifelse(!is.na(pct), 100, NA)) %>% 
  ggplot() + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  # # background fill
  geom_col(aes(y = labels, x = pct_fill), fill = "gray88" ) + 
  # column fill
  geom_col(aes(y = labels, x = pct),fill = alpha("firebrick3", 0.65) ) + 
  geom_text(aes(y = labels, x = pct, label = pct ), fontface = "plain", color = "violetred4", size = 3, hjust = -0.1 ) +
  # background fill
  # geom_col(aes(y = labels, x = pct_fill), fill = neg_color) + 
  # # column fill
  # geom_col(aes(y = labels, x = pct), fill = pos_color) + 
  # geom_text(aes(y = labels, x = pct, label = pct), color = text_color, size = 3, hjust = -0.2 ) +
  annotate("text", x = 50, y = max(all_sections$rowid) + 1, label = "% Pos.", size = 2.7,  fontface = "bold", hjust = 0.5) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "") + #placeholder
  scale_y_discrete(limits = rev(all_sections$labels), 
                   expand = expansion(add = y_axis_expansion)) + 
  scale_x_continuous(expand = expansion(add = c(30, 40))) + 
  theme_void() + 
  coord_cartesian(clip = "off")


univariate_odds_tab <-
  all_sections %>%
  ggplot() +
  geom_richtext(aes(y = labels, x = 0, label = estimate_and_CI), family = "Avenir Next", size = 3, hjust = 0.5, fill = NA, label.color = NA) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Univariate\nOR (95% CI)", 
           hjust = 0.5, vjust = 0.2, fontface = "bold", size = 2.7) +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = y_axis_expansion)) +
  scale_x_continuous(expand = expansion(add = c(0.06, 0.1))) + 
  coord_cartesian(clip = "off")


univariate_odds_plot <- 
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
  annotate("text", x = 1, y = max(all_sections$rowid) + 1, label = "Univariate\nOR plot",  fontface = "bold", 
           hjust = 0.3, vjust = 0.2, size = 2.7) +
  theme_void() + 
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "") + #placeholder
  annotate("segment", x = 1, xend = 1,  y = 0.5, yend =  max(all_sections$rowid), linetype = "dashed") +
  scale_y_discrete(limits = rev(all_sections$labels), 
                   expand = expansion(add = y_axis_expansion)) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.line.x = element_line(), 
        axis.ticks.x = element_line(size = 0.1)) + 
  coord_cartesian(clip = "off")



multivariate_odds_tab <-
  all_sections %>%
  ggplot() +
  geom_richtext(aes(y = labels, x = 0, label = multi_estimate_and_CI), family = "Avenir Next", size = 3, hjust = 0.5, fill = NA, label.color = NA) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Multivariate\nOR (95% CI)", 
           hjust = 0.5, vjust = 0.2, fontface = "bold" ,size = 2.7) +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = y_axis_expansion)) +
  scale_x_continuous(expand = expansion(add = c(0.06, 0.1))) + 
  coord_cartesian(clip = "off")



multivariate_odds_plot <- 
  all_sections %>% 
  ggplot() +
  scale_x_continuous(trans = pseudolog10_trans, 
                     expand = expansion(add = c(0, 0.1)), 
                     breaks = c(1, 2, 4, 8)) + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_pointrange(aes(y = labels, x = multi_estimate, xmin = multi_lower_CI, xmax = multi_upper_CI), 
                  shape = 22, fill = "black", color = "black", size = 0.3) + 
  geom_point(data = subset(all_sections, multi_signif == TRUE), 
             aes(y = labels, x = multi_upper_CI), position = position_nudge(x = 0.1), shape = 8, size = 1.8) + 
  annotate("text", x = 1, y = max(all_sections$rowid) + 1, label = "Multivariate\nOR plot",  fontface = "bold", 
           hjust = 0.3, vjust = 0.2, size = 2.7) +
  theme_void() + 
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "") + #placeholder
  annotate("segment", x = 1, xend = 1,  y = 0.5, yend =  max(all_sections$rowid) , linetype = "dashed") +
  scale_y_discrete(limits = rev(all_sections$labels), 
                   expand = expansion(add = y_axis_expansion)) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.line.x = element_line(), 
        axis.ticks.x = element_line(size = 0.1)) + 
  coord_cartesian(clip = "off")


infection_regn_plot <- 
  patchwork::wrap_plots(table_sec1,
                        positive_pct_plot, 
                        univariate_odds_tab,
                        univariate_odds_plot, 
                        multivariate_odds_tab,
                        multivariate_odds_plot,
                        nrow = 1, widths = c(4.8,
                                             2,
                                             2,
                                             1.5,
                                             2, 
                                             1.5
                        ))


