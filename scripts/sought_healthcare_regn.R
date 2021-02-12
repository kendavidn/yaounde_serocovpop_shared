
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Regression subset  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


yao_regn <- 
  yao %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # cascade answers about household revenue from hhld reps to other members of hhld. Need to be done before filtering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mutate(has_rev_dropped = ifelse(has_rev_dropped == "NR", NA, has_rev_dropped)) %>% 
  mutate(is_hhld_rep = ifelse(id_ind %in% yao_hhld$id_ind, 
                              "Yes", 
                              "No")) %>% 
  group_by(id_hhld) %>% 
  # make a new variable that mirrors has_rev_dropped, but only applies to hhld heads
  mutate(
    has_rev_dropped_head = NA,
    has_rev_dropped_head = ifelse(is_hhld_rep == "Yes" & has_rev_dropped == "Yes", "Yes", has_rev_dropped_head), 
    has_rev_dropped_head = ifelse(is_hhld_rep == "Yes" & has_rev_dropped == "No", "No", has_rev_dropped_head)) %>% 
  # then cascade that variable to others in the hhld
  mutate(has_rev_dropped = first(na.omit(has_rev_dropped_head))) %>% 
  mutate(has_rev_dropped = recode(has_rev_dropped, 
                                  "Yes" = "Revenue decrease",
                                  "No" = "No revenue decrease"
                                  )) %>% 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Other things ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # regression is only run on those with COVID like symptoms
  filter(has_COVID_symptoms == "Yes") %>% 
  # clean up dependent variable
  mutate(has_consult_formal_care = ifelse(has_consult_formal_care == TRUE, "Yes", "No"), 
         has_consult_formal_care_num = ifelse(has_consult_formal_care == "Yes", 1, 0 )) %>% 
  # remove rows with missing independent variables
  filter(!is.na(is_fearful_stigma)) %>% 
  filter(!is.na(has_rev_dropped))

  
  
consulted_count <- 
  yao_regn %>% 
  filter(has_consult_formal_care_num == 1) %>% 
  nrow()

sample_for_consult_regn <- nrow(yao_regn)

# records dropped
yao %>% 
  filter(has_COVID_symptoms == "Yes") %>% nrow() - 
  sample_for_consult_regn
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
                consulted_care = sum(has_consult_formal_care_num)) %>%
      mutate(pct = consulted_care / n,
             pct = 100 * pct,
             pct = round(pct, 1),
             consulted_care_pct = paste0(consulted_care, " (", pct, ")")) %>% 
      rename("labels" = {{ col }})
  }



regn_per_group <- 
  function(df, col, the_formula = NULL, col_string = NULL) {
    
    if(is.null(col_string)) {
      col_string <- deparse(substitute(col))
    }
    
    if(is.null(the_formula)) {
      the_formula <- paste("has_consult_formal_care_num ~", col_string)
    }
    # run regn and clean output
    df %>% 
      glm(formula = the_formula,
          data = .,
          family = "binomial") %>% 
      broom.mixed::tidy() %>% 
      filter(row_number()!= 1) %>%  # drop first row
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
#~  Is worried about COVID stigma  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fearful_stigma_count <- 
  yao_regn %>% 
  count_per_group(is_fearful_stigma)

fearful_stigma_regn <- 
  yao_regn %>%
  regn_per_group(is_fearful_stigma)

fearful_stigma_count_and_regn <- 
  fearful_stigma_count %>% 
  left_join(fearful_stigma_regn) %>% 
  clean_count_and_regn(label = "Are you worried about stigma from COVID diagnosis?", group = "is_fearful_stigma")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Revenue drop during pandemic ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rev_dropped_count <- 
  yao_regn %>% 
  count_per_group(has_rev_dropped)

rev_dropped_regn <- 
  yao_regn %>%
  regn_per_group(has_rev_dropped)

rev_dropped_count_and_regn <- 
  rev_dropped_count %>% 
  left_join(rev_dropped_regn) %>% 
  clean_count_and_regn(label = "Has household revenue dropped since March 1st?", group = "has_rev_dropped")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Since March 1st, have you had difficulty in paying for medical costs?  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diffic_pay_medic_count <- 
  yao_regn %>% 
  count_per_group(has_diffic_pay_medic)

diffic_pay_medic_regn <- 
  yao_regn %>%
  regn_per_group(has_diffic_pay_medic)

diffic_pay_medic_count_and_regn <- 
  diffic_pay_medic_count %>% 
  left_join(diffic_pay_medic_regn) %>% 
  clean_count_and_regn(label = "Difficulty in affording care or medication", group = "has_diffic_pay_medic")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Difficulty in travelling to care  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

has_diffic_travel_care_count <- 
  yao_regn %>% 
  count_per_group(has_diffic_travel_care)

has_diffic_travel_care_regn <- 
  yao_regn %>%
  regn_per_group(has_diffic_travel_care)

has_diffic_travel_care_count_and_regn <- 
  has_diffic_travel_care_count %>% 
  left_join(has_diffic_travel_care_regn) %>% 
  clean_count_and_regn(label = "Difficulty in travelling to health centres", group = "has_diffic_travel_care")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Thinks clinics are dangerous due to COVID ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

thinks_clinics_dangerous_COVID_count <- 
  yao_regn %>% 
  count_per_group(thinks_clinics_dangerous_COVID)

thinks_clinics_dangerous_COVID_regn <- 
  yao_regn %>%
  regn_per_group(thinks_clinics_dangerous_COVID)

thinks_clinics_dangerous_COVID_count_and_regn <- 
  thinks_clinics_dangerous_COVID_count %>% 
  left_join(thinks_clinics_dangerous_COVID_regn) %>% 
  clean_count_and_regn(label = "Are health centres dangerous due to exposure to COVID-19?", group = "thinks_clinics_dangerous_COVID")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Combine and plot  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_only_p_below_0.1 <- TRUE


all_sections_univariate <- 
  sex_count_and_regn %>% 
  bind_rows(age_count_and_regn) %>% 
  bind_rows(fearful_stigma_count_and_regn) %>% 
  bind_rows(rev_dropped_count_and_regn) %>% 
  bind_rows(diffic_pay_medic_count_and_regn) %>% 
  bind_rows(has_diffic_travel_care_count_and_regn) %>% 
  bind_rows(thinks_clinics_dangerous_COVID_count_and_regn) %>% 
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
  # set reference level for regressions
  glm(formula = paste("has_consult_formal_care_num ~", signif_vars),
      data = .,
      family = "binomial") %>% 
  broom.mixed::tidy() %>%
  filter(row_number()!= 1 ) %>% # drop first row (intercept)
  mutate(labels = str_replace(term, paste(unique(all_sections_univariate$group), collapse = "|"), "")) %>%
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
                .fns = ~ if_else(labels %in% c("Female",
                                               "30 - 44", # age
                                               "18.5 - 24.9", # BMI
                                               "No comorbidity",
                                               "Not worried about stigma",
                                               "No revenue decrease",
                                               "No travel difficulty",
                                               "Not dangerous",
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
  mutate(consulted_care_over_n = ifelse(!is.na(n), paste0(consulted_care, " / ", n ), NA )) %>% 
  ggplot() +
  # overall group labels
  geom_richtext(data = subset(all_sections,  is.na(n)), aes(y = labels, x = 0, label = labels), hjust = 0, fill = NA, label.color = NA) +
  # group subsections
  geom_richtext(data = subset(all_sections, !is.na(n)), aes(y = labels, x = 0.2, label = labels), hjust = 0, fill = NA, label.color = NA) +
  # n count
  geom_text(aes(y = labels, x = 3.3, label = n)) +
  # consulted_care count
  geom_text(aes(y = labels, x = 4, label = consulted_care)) +
  # table column names
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Variable", hjust = 0, fontface = "bold") +
  annotate("text", x = 3.3, y = max(all_sections$rowid) + 1, label = "n", hjust = 0.5, fontface = "bold") +
  annotate("text", x = 4, y = max(all_sections$rowid) + 1, label = "Consulted\ncare", vjust = 0.2, fontface = "bold") +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = y_axis_expansion)) +
  scale_x_continuous(expand = expansion(add = c(0.2, 0.2))) + 
  coord_cartesian(clip = "off")


consulted_care_pct_plot <- 
  all_sections %>% 
  mutate(pct_fill = ifelse(!is.na(pct), 100, NA)) %>% 
  ggplot() + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_col(aes(y = labels, x = pct_fill), fill = alpha(my_green, 0.2)) + 
  geom_col(aes(y = labels, x = pct), fill = my_green) + 
  geom_text(aes(y = labels, x = pct, label = pct), color = my_darkgreen, size = 3, hjust = -0.2 ) +
  annotate("text", x = 50, y = max(all_sections$rowid) + 1, label = "% Consulted\ncare",  fontface = "bold", vjust = 0.2) +
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
  mutate(truncated_upper_CI = ifelse(upper_CI> 20, 20, upper_CI)) %>% 
  ggplot() +
  scale_x_continuous(trans = pseudolog10_trans, 
                     expand = expansion(add = c(0, 0.5)), 
                     breaks = c(1, 2, 4, 8)) + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_pointrange(aes(y = labels, x = estimate, xmin = lower_CI, xmax = truncated_upper_CI), 
                  shape = 22, fill = "black", color = "black", size = 0.3) + 
  geom_point(data = subset(all_sections, signif == TRUE), 
             aes(y = labels, x = upper_CI), position = position_nudge(x = 0.1), shape = 8, size = 1.8) + 
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
  mutate(truncated_multi_upper_CI = ifelse(multi_upper_CI> 20, 20, multi_upper_CI)) %>% 
  ggplot() +
  scale_x_continuous(trans = pseudolog10_trans, 
                     expand = expansion(add = c(0, 0.5)), 
                     breaks = c(1, 2, 4, 8)) + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_pointrange(aes(y = labels, x = multi_estimate, xmin = multi_lower_CI, xmax = truncated_multi_upper_CI), 
                  shape = 22, fill = "black", color = "black", size = 0.3) + 
  geom_point(data = subset(all_sections, multi_signif == TRUE), 
             aes(y = labels, x = multi_upper_CI), position = position_nudge(x = 0.1), shape = 8, size = 1.8) + 
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

consulted_care_regn_plot <- patchwork::wrap_plots(table_sec1,
                                               consulted_care_pct_plot, 
                                               univariate_odds_tab,
                                               univariate_odds_plot, 
                                               multivariate_odds_tab,
                                               multivariate_odds_plot,
                                               nrow = 1, widths = c(4.8,
                                                                    2.5,
                                                                    2,
                                                                    1.8,
                                                                    2, 
                                                                    1.8
                                               ))








