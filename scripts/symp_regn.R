
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Regression subset  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# regression is only run on folks who are positive for the bug
yao_symp_regn <- 
  yao %>% 
  # drop folks who are not positive
  filter(cat_pos_num == 1 ) %>% 
  # replace NA
  mutate(mcat_chronic = replace_na(mcat_chronic, "None")) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# baseline risk for converting from odds ratio to relative risk
base_risk_symp <- 
  tabyl(yao_symp_regn$has_COVID_symptoms_num) %>% 
  pull(percent) %>% 
  .[2]

count_per_group <- 
  function(df, col) {
    df %>%
      group_by({{ col }}) %>%
      summarise(n = n(),
                symptomatic = sum(has_COVID_symptoms_num)) %>%
      mutate(pct = symptomatic / n,
             pct = 100 * pct,
             pct = round(pct, 1),
             symptomatic_pct = paste0(symptomatic, " (", pct, ")"))
  }


regn_per_group <- 
  function(df, col, the_formula = NULL, col_string = NULL) {
    
    if(is.null(col_string)) {
      col_string <- deparse(substitute(col))
    }
    
    if(is.null(the_formula)) {
      the_formula <- paste("has_COVID_symptoms_num ~", col_string, "+ (1 | id_hhld)") %>% as.formula()
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
             avg_rel_risk = estimate/( 1 - base_risk_symp + ( base_risk_symp * estimate) ),
             signif = (upper_CI > 1 & lower_CI > 1) | (upper_CI < 1 & lower_CI < 1)
      ) %>% 
      mutate(estimate = format(estimate, digits = 2 ), 
             upper_CI = format(upper_CI, digits = 2 ),
             lower_CI = round(lower_CI, digits = 2),
             lower_CI = format(lower_CI, digits = 2 ), 
             avg_rel_risk = format(avg_rel_risk, digits = 2 )
      ) %>% 
      mutate(term = str_replace_all(term, col_string, "")) %>% 
      mutate(estimate_and_CI = paste0(estimate, " (", lower_CI, " - ", upper_CI, ")")) %>% 
      select(term, estimate_and_CI, lower_CI, estimate, upper_CI, avg_rel_risk, signif)
  }

clean_count_and_regn <- 
  function(df, internal_name_str, print_name_str){
    df %>% 
      rename("labels" = all_of(internal_name_str) ) %>% # all_of is just to silence an error message
      bind_rows(c(labels = paste0("**", print_name_str ,"**") )) %>% 
      arrange(labels != paste0("**", print_name_str ,"**") ) %>% 
      mutate(estimate = as.numeric(estimate),
             lower_CI = as.numeric(lower_CI), 
             upper_CI = as.numeric(upper_CI)) %>% 
      mutate(rowid = rev(1:nrow(.)))
  }



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Gender ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sex_count <- yao_symp_regn %>% 
  count_per_group(cat_sex)

sex_regn <- 
  yao_symp_regn %>% 
  regn_per_group(cat_sex)

sex_count_and_regn <- 
  sex_count %>% 
  left_join(sex_regn, by = c("cat_sex"  = "term") ) %>% 
  clean_count_and_regn("cat_sex", "Sex")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Ages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

age_count <- yao_symp_regn %>% 
  count_per_group(cat_age)

age_regn <- 
  yao_symp_regn %>% 
  regn_per_group(cat_age)

age_count_and_regn <- 
  age_count %>% 
  left_join(age_regn, by = c("cat_age"  = "term") ) %>% 
  clean_count_and_regn("cat_age", "Age") %>% 
  # reorder for table
  mutate(labels = factor(labels, levels = c("**Age**" ,"5 - 15", "16 - 29", "30 - 45", "46 - 65", "\\> 65"))) %>% 
  arrange(labels)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  BMI classes  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BMI_count <- 
  yao_symp_regn %>% 
  # drop impossible BMIs
  filter(!is.na(cat_BMI)) %>% 
  count_per_group(cat_BMI)


BMI_regn <- 
  yao_symp_regn %>%
  # drop impossible BMIs
  filter(!is.na(cat_BMI)) %>% 
  regn_per_group(cat_BMI)


BMI_count_and_regn <- 
  BMI_count %>% 
  left_join(BMI_regn, by = c("cat_BMI"  = "term") ) %>% 
  clean_count_and_regn("cat_BMI", "BMI group") %>% 
  # reorder for table
  mutate(labels = factor(labels, levels = c("**BMI group**", "\\< 18.5 (Underweight)", "18.5 - 24.9", "25 - 30 (Overweight)", " \\> 30 (Obese)"))) %>% 
  arrange(labels)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Education  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

educ_count <- 
  yao_symp_regn %>% 
  filter(!(cat_educ %in% c("Doctorate", "No response", "Other"))) %>% 
  count_per_group(cat_educ)


educ_regn <- 
  yao_symp_regn %>%
  filter(!(cat_educ %in% c("Doctorate", "No response", "Other"))) %>% 
  regn_per_group(cat_educ)


educ_count_and_regn <- 
  educ_count %>% 
  left_join(educ_regn, by = c("cat_educ"  = "term") ) %>% 
  clean_count_and_regn("cat_educ", "Highest education level")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Occupation  ----
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
                   the_formula = as.formula(paste("has_COVID_symptoms_num ~", col_string, "+ (1 | id_hhld)")),
                   col_string = col_string)
  
  chronic_count_and_regn <-
    chronic_count %>%
    bind_cols(chronic_regn) %>% 
    mutate("{{ col }}" := chronic_name) %>% 
    mutate(estimate = as.numeric(estimate),
           lower_CI = as.numeric(lower_CI), 
           upper_CI = as.numeric(upper_CI)) %>% 
    rename("labels" = 1 )
  
  return(chronic_count_and_regn)
}

hypertension_section <- yao_symp_regn %>% occup_count_and_regn(is_chronic_hypert2, "Hypertension") 
resp_section <- yao_symp_regn %>% occup_count_and_regn(is_chronic_resp, "Respiratory illness") 
diab_section <- yao_symp_regn %>% occup_count_and_regn(is_chronic_diab2, "Diabetes") 


chronic_section <- 
  bind_rows(list(hypertension_section, 
                 resp_section, 
                 diab_section
  ))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Smoker?  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

smoker_count <- 
  yao_symp_regn %>% 
  filter(!is.na(is_smoker)) %>% 
  count_per_group(is_smoker)

smoker_regn <- 
  yao_symp_regn %>%
  filter(!is.na(is_smoker)) %>% 
  regn_per_group(is_smoker)


smoker_count_and_regn <- 
  smoker_count %>% 
  left_join(smoker_regn, by = c("is_smoker"  = "term") ) %>% 
  clean_count_and_regn("is_smoker", "Do you smoke?")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Pregnant?  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pregnant_count <- 
  yao_symp_regn %>% 
  filter(!is.na(is_pregnant)) %>% 
  count_per_group(is_pregnant)

pregnant_regn <- 
  yao_symp_regn %>%
  filter(!is.na(is_pregnant)) %>% 
  regn_per_group(is_pregnant)


pregnant_count_and_regn <- 
  pregnant_count %>% 
  left_join(pregnant_regn, by = c("is_pregnant"  = "term") ) %>% 
  clean_count_and_regn("is_pregnant", "Are you pregnant?")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Combine and plot  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_sections <- 
  sex_count_and_regn %>% 
  bind_rows(age_count_and_regn) %>% 
  bind_rows(educ_count_and_regn)  %>% 
  bind_rows(BMI_count_and_regn)  %>% 
  bind_rows(smoker_count_and_regn) %>% 
  bind_rows(pregnant_count_and_regn) %>% 
  bind_rows(data.frame(labels = "**Chronic illnesses**")) %>% 
  bind_rows(chronic_section) %>% 
  mutate(rowid = rev(1:nrow(.))) %>% 
  mutate(labels = fct_inorder(labels)) %>% 
  # Add "reference" to reference rows
  mutate(estimate_and_CI = if_else(labels %in% c("30 - 45", 
                                                 "18.5 - 24.9",
                                                 "No formal instruction", 
                                                 "Female", 
                                                 "Non-smoker", 
                                                 "No", 
                                                 "Cité Verte", 
                                                 "No children"),
                                   "*Reference*",
                                   estimate_and_CI
  ))


# change text size
update_geom_defaults("text", list(size = 3))
update_geom_defaults("richtext", list(size = 3))
theme_set(theme_void())



table_sec1 <-
  all_sections %>%
  mutate(symptomatic_over_n = ifelse(!is.na(n), paste0(symptomatic, " / ", n ), NA )) %>% 
  ggplot() +
  geom_richtext(data = subset(all_sections,  is.na(n)), aes(y = labels, x = 0, label = labels), hjust = 0, fill = NA, label.color = NA) +
  geom_richtext(data = subset(all_sections, !is.na(n)), aes(y = labels, x = 0.2, label = labels), hjust = 0, fill = NA, label.color = NA) +
  geom_text(aes(y = labels, x = 3, label = n)) +
  geom_text(aes(y = labels, x = 4.5, label = symptomatic)) +
  annotate("text", x = 0, y = max(all_sections$rowid) + 1, label = "Variable", hjust = 0, fontface = "bold", size = 3.3) +
  annotate("text", x = 3, y = max(all_sections$rowid) + 1, label = "Positive", hjust = 0.5, fontface = "bold", size = 3.3) +
  annotate("text", x = 4.5, y = max(all_sections$rowid) + 1, label = "Symptomatic", hjust = 0.5, fontface = "bold", size = 3.3) +
  theme_void() +
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") +
  scale_y_discrete(limits = rev(all_sections$labels),
                   expand = expansion(add = c(0.5, 2))) +
  scale_x_continuous(expand = expansion(add = c(0.5, 0.9)))



positive_barplot <- 
  all_sections %>% 
  mutate(pct_fill = ifelse(!is.na(pct), 100, NA)) %>% 
  ggplot() + 
  geom_stripes(aes(y = labels), odd = "#11111111", even = "#00000000") + 
  geom_col(aes(y = labels, x = pct_fill), fill = "#d7e6f5") + 
  geom_col(aes(y = labels, x = pct), fill = "dodgerblue4") + 
  geom_text(aes(y = labels, x = pct, label = pct), color = "dodgerblue4", size = 3, hjust = -0.2 ) +
  annotate("text", x = 50, y = max(all_sections$rowid) + 1, label = "% Symptomatic",  fontface = "bold", size = 3.3, hjust = 0.5) +
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
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1)))


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
                      nrow = 1, widths = c(6,2,2,1.5,1))




