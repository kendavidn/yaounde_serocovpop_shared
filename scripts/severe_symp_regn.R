
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Regression subset  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# regression is only run on folks who are seropositive and had symptoms

yao_regn <- 
  yao %>% 
  filter(cat_igg_result == "Positive") %>% 
  # drop folks with missing values for regressors
  filter(!is.na(cat_BMI)) %>% 
  filter(!is.na(is_smoker)) %>% 
  # replace NA
  mutate(mcat_chronic = replace_na(mcat_chronic, "None")) %>% 
  mutate(is_pregnant = replace_na(is_pregnant, "No response")) %>% 
  mutate(has_severe_symptoms_num = ifelse(cat_symp_severity == "Severe", 1, 0) )

positives_count <- yao %>% 
  filter(cat_igg_result == "Positive") %>% 
  nrow()

sample_for_symp_regn <- nrow(yao_regn)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# basedline risk of severe symptoms given seropositivity
#baseline risk for converting from odds ratio to relative risk
base_risk <-
  tabyl(yao_regn$cat_symp_severity) %>%
  pull(percent) %>%
  .[2]

count_per_group <- 
  function(df, col) {
    df %>%
      group_by({{ col }}) %>%
      summarise(n = n(),
                symptomatic = sum(has_severe_symptoms_num, na.rm = T)) %>%
      mutate(pct = symptomatic / n,
             pct = 100 * pct,
             pct = round(pct, 1),
             symptomatic_pct = paste0(symptomatic, " (", pct, ")")) %>% 
      rename("labels" = {{ col }})
  }


regn_per_group <- 
  function(df, col, the_formula = NULL, col_string = NULL) {
    
    if(is.null(col_string)) {
      col_string <- deparse(substitute(col))
    }
    
    if(is.null(the_formula)) {
      the_formula <- paste("has_severe_symptoms_num ~", col_string) 
    }
    # run regn and clean output
    df %>% 
      glm(formula = the_formula,
          data = .,
          family = "binomial") %>% 
      broom.mixed::tidy() %>% 
      filter(row_number()!= 1) %>%  # drop intercept row
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
      select(labels, estimate_and_CI, lower_CI, estimate, upper_CI, signif, p.value)
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

age_regn <- yao_regn %>% regn_per_group(cat_age)

age_count_and_regn <- 
  age_count %>% 
  left_join(age_regn) %>% 
  clean_count_and_regn(label = "Age", group = "cat_age") %>% 
  # reorder for table
  mutate(labels = factor(labels, levels = c("**Age**" ,"5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  arrange(labels)



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
#~  Contact with COVID-19 patient  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

chronic_count <- yao_regn %>% count_per_group(has_chronic)
chronic_regn <- yao_regn %>% regn_per_group(has_chronic)

chronic_count_and_regn <- 
  chronic_count %>% 
  left_join(chronic_regn) %>% 
  clean_count_and_regn(label = "Do you have any chronic conditions?", group = "has_chronic")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Chronic conditions  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

chronic_count_and_regn <- function(df, col, chronic_name) {
  col_string <- deparse(substitute(col))
  
  chronic_count <-
    df %>%
    mutate("{{ col }}" := if_else({{ col }} == 1, "Yes", "No")) %>%
    count_per_group({{ col }}) %>% 
    .[2, ]
  
  chronic_regn <-
    df %>%
    mutate("{{ col }}" := if_else({{ col }} == 1, "Yes", "No")) %>%
    regn_per_group({{ col }},
                   the_formula = paste("has_COVID_symptoms_num ~", col_string),
                   col_string = col_string)
  
  chronic_count_and_regn <-
    chronic_count %>%
    left_join(chronic_regn) %>% 
    mutate("{{ col }}" := chronic_name) %>% 
    mutate(estimate = as.numeric(estimate),
           lower_CI = as.numeric(lower_CI), 
           upper_CI = as.numeric(upper_CI)) %>% 
    rename("labels" = 1 ) %>% 
    mutate(labels = chronic_name)
  
  return(chronic_count_and_regn)
}

hypertension_section <- yao_regn %>% chronic_count_and_regn(is_chronic_hypert2, "Hypertension") 
resp_section <- yao_regn %>% chronic_count_and_regn(is_chronic_resp, "Respiratory illness") 
diab_section <- yao_regn %>% chronic_count_and_regn(is_chronic_diab2, "Diabetes") 


chronic_section <- 
  bind_rows(list(hypertension_section, 
                 resp_section, 
                 diab_section
  ))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Is smoker  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

smoker_count <- 
  yao_regn %>% 
  count_per_group(is_smoker)

smoker_regn <- 
  yao_regn %>%
  regn_per_group(is_smoker)

smoker_count_and_regn <- 
  smoker_count %>% 
  left_join(smoker_regn) %>% 
  clean_count_and_regn(label = "Do you smoke?", group = "is_smoker")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Is pregnant  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pregnant_count <- 
  yao_regn %>% 
  count_per_group(is_pregnant)

pregnant_regn <- 
  yao_regn %>%
  regn_per_group(is_pregnant)

pregnant_count_and_regn <- 
  pregnant_count %>% 
  left_join(pregnant_regn) %>% 
  clean_count_and_regn(label = "Are you pregnant?", group = "is_pregnant")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Combine and plot  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_only_p_below_0.1 <- TRUE


all_sections_univariate <- 
  sex_count_and_regn %>% 
  bind_rows(age_count_and_regn) %>% 
  bind_rows(BMI_count_and_regn)  %>% 
  bind_rows(smoker_count_and_regn) %>% 
  bind_rows(pregnant_count_and_regn) %>% 
  bind_rows(data.frame(labels = "**Chronic illnesses**")) %>% 
  bind_rows(chronic_section) %>% 
  mutate(labels = fct_inorder(labels)) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Filter out p-value not lower than 0.1 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {if (plot_only_p_below_0.1 == TRUE)
  {group_by(., group) %>% 
      filter(min.p.value < 0.1 | group == "cat_age" | group == "cat_sex") %>% 
      ungroup()}
    else {.}
  } %>% 
  # row ids
  mutate(rowid = rev(1:nrow(.)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ Multivariate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



vars_to_paste <- unique(all_sections_univariate$group)
vars_to_paste <- vars_to_paste[!vars_to_paste %in% "mcat_occup"]
signif_vars <- 
  paste(vars_to_paste, "+", collapse = " ") %>% 
  str_sub(end = -2)

all_regn <-
  yao_regn %>%
  glm(formula = paste("has_severe_symptoms_num ~", signif_vars),
      data = .,
      family = "binomial") %>% 
  broom.mixed::tidy() %>%
  filter(row_number()!= 1 ) %>%   # drop intercept row
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
                                               "Female", 
                                               "No response",
                                               "No comorbidity",
                                               "Cité Verte",
                                               "Non-smoker"),
                                 "*Reference*",
                                 .x )
  ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# change text size
update_geom_defaults("text", list(size = 3))
update_geom_defaults("richtext", list(size = 3))
theme_set(theme_void())

# y axis expansion (common to all plots)
y_axis_expansion <- c(0.5,2.5)


table_sec1 <-
  all_sections %>%
  mutate(symptomatic_over_n = ifelse(!is.na(n), paste0(symptomatic, " / ", n ), NA )) %>% 
  ggplot() +
  # overall group labels
  geom_richtext(data = subset(all_sections,  is.na(n)), aes(y = labels, x = 0, label = labels), hjust = 0, fill = NA, label.color = NA) +
  # group subsections
  geom_richtext(data = subset(all_sections, !is.na(n)), aes(y = labels, x = 0.2, label = labels), hjust = 0, fill = NA, label.color = NA) +
  # n count
  geom_text(aes(y = labels, x = 3.3, label = n)) +
  # symptomatic count
  geom_text(aes(y = labels, x = 4, label = symptomatic)) +
  # table column names
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Variable", hjust = 0, fontface = "bold") +
  annotate("text", x = 3.3, y = max(all_sections$rowid) + 1, label = "n Seropos.", hjust = 0.5, fontface = "bold") +
  annotate("text", x = 4, y = max(all_sections$rowid) + 1, label = "n with\nSevere Symp.", hjust = 0.5, fontface = "bold") +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = y_axis_expansion)) +
  scale_x_continuous(expand = expansion(add = c(0.2, 0.2))) + 
  coord_cartesian(clip = "off")


symptomatic_pct_plot <- 
  all_sections %>% 
  mutate(pct_fill = ifelse(!is.na(pct), 100, NA)) %>% 
  ggplot() + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_col(aes(y = labels, x = pct_fill), fill = alpha(my_green, 0.2)) + 
  geom_col(aes(y = labels, x = pct), fill = my_green) + 
  geom_text(aes(y = labels, x = pct, label = pct), color = my_darkgreen, size = 3, hjust = -0.2 ) +
  annotate("text", x = 50, y = max(all_sections$rowid) + 1, label = "% Symp.",  fontface = "bold", hjust = 0.5) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "") + #placeholder
  scale_y_discrete(limits = rev(all_sections$labels), 
                   expand = expansion(add = y_axis_expansion)) + 
  scale_x_continuous(expand = expansion(add = c(30, 40))) + 
  theme_void() + 
  coord_cartesian(clip = "off")


univariate_odds_tab <-
  all_sections %>%
  ggplot() +
  geom_richtext(aes(y = labels, x = 0, label = estimate_and_CI), hjust = 0.5, fill = NA, label.color = NA) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Univariate\nOR (95% CI)", 
           hjust = 0.5, vjust = 0.2, fontface = "bold") +
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
                     expand = expansion(add = c(0.1,0.25)), 
                     breaks = c(1, 2, 4, 8), 
                     labels = c(1,2,4,8)) + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_pointrange(aes(y = labels, x = estimate, xmin = lower_CI, xmax = upper_CI), 
                  shape = 22, fill = "black", color = "black", size = 0.3) + 
  geom_point(data = subset(all_sections, signif == TRUE), 
             aes(y = labels, x = upper_CI), position = position_nudge(x = 0.15), shape = 8, size = 1.8) + 
  annotate("text", x = 1, y = max(all_sections$rowid) + 1, label = "Univariate\nOR plot",  fontface = "bold", 
           hjust = 0.3, vjust = 0.2) +
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
  geom_richtext(aes(y = labels, x = 0, label = multi_estimate_and_CI), hjust = 0.5, fill = NA, label.color = NA) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Multivariate\nOR (95% CI)", 
           hjust = 0.5, vjust = 0.2, fontface = "bold") +
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
                     expand = expansion(add = c(0.1,0.25)), 
                     breaks = c(1, 2, 4, 8), 
                     labels = c(1,2,4,8)) + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_pointrange(aes(y = labels, x = multi_estimate, xmin = multi_lower_CI, xmax = multi_upper_CI), 
                  shape = 22, fill = "black", color = "black", size = 0.3) + 
  geom_point(data = subset(all_sections, multi_signif == TRUE), 
             aes(y = labels, x = multi_upper_CI), position = position_nudge(x = 0.15), shape = 8, size = 1.8) + 
  annotate("text", x = 1, y = max(all_sections$rowid) + 1, label = "Multivariate\nOR plot",  fontface = "bold", 
           hjust = 0.3, vjust = 0.2) +
  theme_void() + 
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "") + #placeholder
  annotate("segment", x = 1, xend = 1,  y = 0.5, yend =  max(all_sections$rowid) , linetype = "dashed") +
  scale_y_discrete(limits = rev(all_sections$labels), 
                   expand = expansion(add = y_axis_expansion)) + 
  theme(axis.text.x = element_text(size = 8), 
        axis.line.x = element_line(), 
        axis.ticks.x = element_line(size = 0.1)) + 
  coord_cartesian(clip = "off")

severe_symp_regn_plot <- patchwork::wrap_plots(table_sec1,
                                               symptomatic_pct_plot, 
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







