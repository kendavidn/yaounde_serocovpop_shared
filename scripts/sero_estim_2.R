source(here::here("scripts/infection_regn_2.R"))

## Sensitivity validation data from a panel of formerly hospitalized COVID-19 patients. 
## <https://www.sciencedirect.com/science/article/pii/S1386653220303875#bib0060>
positives <- 82 
true_positives <- 75
sensit <- true_positives/positives
sensit_conf_int <- binom.test(x = true_positives, n = positives)$conf.int

##  Specificity validation data from own work (Projet EPICO)
negatives <- 246
true_negatives <- 230
specif <- true_negatives/negatives
specif_conf_int <- binom.test(x = true_negatives, n = negatives)$conf.int

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Seroprevalence calculate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## build list over which to iterate
## for each grouping, filter the table 
## then calculate crude seroprevalence, population-weighted seroprevalence, 
## and population-weighted, test-adjusted seroprevalence


unique_vals_to_df <- function(df, varnames) {
  df_out <- data.frame()
  for (varname in varnames) {
    df_sub <- df[varname]
    labels <- unique(df_sub[[varname]])
    ## to keep factors arranged as they came in
    if (is.factor(df_sub[[varname]])) {labels <- levels(df_sub[[varname]])}
    rows_to_bind <- data.frame(varname, labels)
    df_out <- bind_rows(df_out, rows_to_bind)
  }
  return(df_out)
}

## arrange to match the order for risk factor analysisw
yao_arranged <- 
  yao %>% 
  ## arrange cat
  mutate(cat_age = factor(cat_age, levels = c("5 - 14", "15 - 29", 
                                              "30 - 44", "45 - 64", "65 +"))) %>% 
  mutate(cat_BMI = as.character(cat_BMI)) %>% 
  mutate(cat_BMI = replace_na(cat_BMI, "Missing BMI")) %>% 
  mutate(cat_BMI = factor(cat_BMI, levels = c("\\< 18.5 (Underweight)", 
                                              "18.5 - 24.9",
                                              "25 - 30 (Overweight)", 
                                              " \\> 30 (Obese)", 
                                              "Missing BMI"))) %>% 
  mutate(cat_n_hhld_indiv = factor(cat_n_hhld_indiv, levels = c("1 - 2", "3 - 5", "\\> 5")))

seroprev_iterator <-
  unique_vals_to_df(yao_arranged,
                    varnames = c(
                      "all_respondents",
                      "cat_sex",
                      "cat_age",
                      #"cat_BMI",
                      #"has_contact_traveler",
                      "loc_hhld_area"
                      #,
                      #"cat_n_hhld_indiv"
                    ))

## initialize
seroprev_table <- data.frame()

for (i in 1:nrow(seroprev_iterator)){
  
  curr_group <- seroprev_iterator[i, ]

  yao_subset <- yao_arranged[yao_arranged[[curr_group[[1]]]] == as.character(curr_group[[2]]), ]
  
  ## create svy objects within loop because they can't be filtered for some reason
  yao_unweighted <-
    survey::svydesign(ids = ~id_hhld,  ## sampling unit
                      strata = NULL,
                      weights = ~1,
                      data = yao_subset)
  
  yao_weighted <-
    survey::svydesign(ids = ~id_hhld,  ## sampling unit
                      strata = NULL,
                      weights = ~weight_per_individual,
                      data = yao_subset)
  
  
  # ~~~~ crude ----
  seroprev_crude <-
    survey::svyciprop(formula = ~cat_igg_result_num,
                      design = yao_unweighted,
                      method = "logit")
  
  seroprev_crude_ci <- attr(seroprev_crude, "ci")
  
  # ~~~~ population weighted ----
  seroprev_pop_weighted <-
    survey::svyciprop(formula = ~ cat_igg_result_num,
                      design = yao_weighted,
                      method = "logit")
  
  seroprev_pop_weighted_ci <- attr(seroprev_pop_weighted, "ci")
  
  # ~~~~ population weighted, test_adjusted ----
  
  seroprev_pop_weighted_test_adj <- 
    bootstrap_results <- 
    bootComb::adjPrevSensSpecCI(
      prev =  seroprev_pop_weighted[[1]], ## observed prevalence
      sens = sensit, ## observed sensitivity from validation study
      spec = specif, ## observed specificity from validation study
      prevCI = seroprev_pop_weighted_ci[1:2], ## prevalence 95% confidence interval
      sensCI = sensit_conf_int[1:2], ## sensitivity 95% confidence interval
      specCI = specif_conf_int[1:2], ## specificity 95% confidence interval
      N = 1e5, ## number of bootstrap samples  
      method = "quantile", ## use quantiles to compute seroprev 95% CI
      alpha = 0.05,
      doPlot = FALSE ## no plot
    )
  seroprev_pop_weighted_test_adj_ci <- seroprev_pop_weighted_test_adj$conf.int
  
  # ~~~~ combine into single row of table ----
  
  seroprev_crude_print <-
    paste0(formatC(100*seroprev_crude[[1]], digits = 1, format = "f"),"%",
           " (", 
           formatC(100*seroprev_crude_ci[[1]], digits = 1, format = "f"),
           " - ",
           formatC(100*seroprev_crude_ci[[2]], digits = 1, format = "f"),
           ")")
  
  seroprev_pop_weighted_print <- 
    paste0(formatC(100*seroprev_pop_weighted, digits = 1, format = "f"),"%",
           " (", 
           formatC(100*seroprev_pop_weighted_ci[[1]], digits = 1, format = "f"),
           " - ",
           formatC(100*seroprev_pop_weighted_ci[[2]], digits = 1, format = "f"),
           ")")
  
  seroprev_pop_weighted_test_adj_print <- 
    paste0(formatC(100*seroprev_pop_weighted_test_adj[[1]], digits = 1, format = "f"),"%",
           " (", 
           formatC(100*seroprev_pop_weighted_test_adj_ci[[1]], digits = 1, format = "f"),
           " - ",
           formatC(100*seroprev_pop_weighted_test_adj_ci[[2]], digits = 1, format = "f"),
           ")")
  
  total_sample <- nrow(yao_subset)
  num_positive <- nrow(filter(yao_subset, cat_igg_result == "Positive"))

  if (curr_group[[1]] == "all_respondents") {labels <- "Total"} else {labels <- curr_group[[2]]}
  
  table_row <- 
    data.frame(grouping = curr_group[[1]],
               labels = labels,
               total_sample, 
               num_positive, 
               seroprev_crude_print, 
               seroprev_pop_weighted_print, 
               seroprev_pop_weighted_test_adj_print)
  
  seroprev_table <- rbind(seroprev_table, table_row)

}

sex_label <- data.frame(labels = "Sex")
age_group_label <- data.frame(labels = "Age group")
neighborhood_label <- data.frame(labels = "Neighborhood")

igg_pos_table_print <- 
  bind_rows(seroprev_table[1,],
            sex_label, 
            seroprev_table[2:3,], 
            age_group_label,
            seroprev_table[4:8,], 
            neighborhood_label, 
            seroprev_table[9:17,]
  ) %>% 
  select(-"grouping") %>% 
  rename(Crude = seroprev_crude_print, 
         `Age-sex-weighted` = seroprev_pop_weighted_print, 
         `Age-sex-weighted,\ntest-adjusted` = seroprev_pop_weighted_test_adj_print) %>% 
  huxtable() %>% 
  set_contents(1, 1:3, c("", "", "") ) %>% 
  insert_row("", "n", "Seropos.", "Seroprevalence (95% confidence interval)", "", "", after = 0) %>% 
  merge_cells(1, 4:6) %>% 
  set_bold(1, col = everywhere) %>% 
  theme_basic() %>% 
  set_latex_float("h!") %>% 
  set_all_padding(0.5) 


# ~~~~ combine into single row of table ----

ggplot_sens_spec_prev <- 
  ggplot_adjPrevSensSpecCI(
    prev =  seroprev_pop_weighted[[1]], ## observed prevalence
    sens = sensit, ## observed sensitivity from validation study
    spec = specif, ## observed specificity from validation study
    prevCI = seroprev_pop_weighted_ci[1:2], ## prevalence 95% confidence interval
    sensCI = sensit_conf_int[1:2], ## sensitivity 95% confidence interval
    specCI = specif_conf_int[1:2], ## specificity 95% confidence interval
    N = 1e5, ## number of bootstrap samples  
    method = "quantile", ## use quantiles to compute seroprev 95% CI
    alpha = 0.05,
    doPlot = FALSE ## no plot
  )
