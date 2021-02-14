
yao_not_NA <- 
  yao %>% 
  filter(!is.na(cat_igg_result) & !is.na(mcat_symp))

# seropos_symptoms_df <-  
#   yao_not_NA %>% 
#   summarise(
#     sero_alone = sum(has_acute_symp == "No" &  has_COVID_symptoms == "No" & cat_igg_result == "Positive"),
#     acute_alone = sum(has_acute_symp == "Yes" & has_COVID_symptoms == "No" & cat_igg_result == "Negative"), 
#     COVIDlike_alone = 0, # all those with COVID-suspect symptoms must be subset of acute symptoms group
#     sero_and_acute = sum(has_acute_symp == "Yes" &  has_COVID_symptoms == "No" & cat_igg_result == "Positive"),
#     sero_and_COVIDlike = 0, # all those with COVID-suspect and comorbidites must also be subset of acute sypmtoms group
#     acute_and_COVIDlike = sum(has_acute_symp == "Yes" &  has_COVID_symptoms == "Yes" & cat_igg_result == "Negative"),
#     sero_and_acute_and_COVIDlike = sum(has_acute_symp == "Yes" &  has_COVID_symptoms == "Yes" & cat_igg_result == "Positive"),
#     everyone = nrow(yao)) %>% 
#   rowwise() %>% 
#   mutate(any_symp_or_sero = rowSums(.[1:7]))
# 
# intersecting_inside <- 
#   euler(c(
#     # technically the "No symptoms of condition" set should be called the "everyone" set. But leaving it this way makes the final venn diagram easier to interpret
#     "IgG seronegative, no symptoms" = seropos_symptoms_df$everyone - seropos_symptoms_df$any_symp_or_sero, # all respondents alone
#     # "IgG seropositive" = 0, # Impossible (not part of everyone set)
#     #  "With Any acute symptom(s)" = 0,  # Impossible (not part of everyone set)
#     #  "With COVID-suspect symptom(s)" = 0,  # Impossible (not part of everyone set),
#     "IgG seronegative, no symptoms&IgG seropositive" = seropos_symptoms_df$sero_alone,
#     "IgG seronegative, no symptoms&Any acute symptom(s)" = seropos_symptoms_df$acute_alone,
#     # "IgG seronegative, no symptoms&With COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
#     "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)" = seropos_symptoms_df$sero_and_acute,
#     "IgG seronegative, no symptoms&IgG seropositive&COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
#     "IgG seronegative, no symptoms&Any acute symptom(s)&COVID-suspect symptom(s)" = seropos_symptoms_df$acute_and_COVIDlike, 
#     "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)&COVID-suspect symptom(s)" = seropos_symptoms_df$sero_and_acute_and_COVIDlike))


intersecting_inside <- 
  euler(c(
    # technically the "No symptoms of condition" set should be called the "everyone" set. But leaving it this way makes the final venn diagram easier to interpret
    "IgG seronegative, no symptoms" = 
      yao_not_NA %>% 
      filter(cat_igg_result == "Negative") %>% 
      filter(has_COVID_symptoms =="No") %>%  
      filter(has_acute_symp == "No") %>% 
      nrow(),
    # "IgG seropositive" = 0, # Impossible (not part of everyone set)
    #  "With Any acute symptom(s)" = 0,  # Impossible (not part of everyone set)
    #  "With COVID-suspect symptom(s)" = 0,  # Impossible (not part of everyone set),
    "IgG seronegative, no symptoms&IgG seropositive" = #seropos_symptoms_df$sero_alone,
      yao_not_NA %>% 
      filter(cat_igg_result == "Positive") %>% 
      filter(has_COVID_symptoms =="No") %>%  
      filter(has_acute_symp == "No") %>% 
      nrow(),
    "IgG seronegative, no symptoms&Any acute symptom(s)" = #seropos_symptoms_df$acute_alone,
      yao_not_NA %>% 
      filter(has_COVID_symptoms =="No") %>%  
      filter(cat_igg_result == "Negative") %>% 
      filter(has_acute_symp == "Yes") %>% 
      nrow(),
    # "IgG seronegative, no symptoms&With COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
    "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)" = #seropos_symptoms_df$sero_and_acute,
      yao_not_NA %>% 
      filter(cat_igg_result == "Positive") %>% 
      filter(has_COVID_symptoms =="No") %>%  
      filter(has_acute_symp == "Yes") %>% 
      nrow(),
    "IgG seronegative, no symptoms&IgG seropositive&COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
    "IgG seronegative, no symptoms&Any acute symptom(s)&COVID-suspect symptom(s)" = #seropos_symptoms_df$acute_and_COVIDlike, 
      yao_not_NA %>% 
      filter(cat_igg_result == "Negative") %>% 
      filter(has_COVID_symptoms =="Yes") %>%  
      filter(has_acute_symp == "Yes") %>% 
      nrow(),
    "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)&COVID-suspect symptom(s)" = #seropos_symptoms_df$sero_and_acute_and_COVIDlike
      yao_not_NA %>% 
      filter(cat_igg_result == "Positive") %>% 
      filter(has_COVID_symptoms =="Yes") %>%  
      filter(has_acute_symp == "Yes") %>% 
      nrow()
      ))


seropos_symptoms_euler_plot <- 
  plot(intersecting_inside,
     fills = list(fill = c(alpha("dodgerblue2", 0.5),
                           alpha("firebrick3", 0.55),
                           alpha("darkgoldenrod1", 0.3),
                           alpha("darkgoldenrod1", 0.8)
     )),   
     legend = list(side = "bottom",
                   fontfamily = "Avenir Next", font =  1,
                   cex=0.85),
     # labels = list(fontfamily = "Avenir Next", 
     #               cex=0.7
     # ),
     quantities = list(type = c("counts"),
                       cex= c(0.7,0.7,0.7, 0.7) ,
                       font = 1, 
                       fontfamily = "Avenir Next"
     ), 
     edges = list(col = "transparent"))


# 
# set.seed(2)
# intersecting_inside <- euler(c("No symptom or condition" = healthcare_needing_pop$everyone - healthcare_needing_pop$either, 
#                                "Chronic condition" = 0, # Chronic condition alone (not part of everyone set)
#                                "Any symptom" = 0,  # Any condition alone (not part of everyone set)
#                                "No symptom or condition&Chronic condition" = healthcare_needing_pop$with_comorbid_alone, 
#                                "No symptom or condition&Any symptom"  = healthcare_needing_pop$with_Any_symptoms_alone, 
#                                "Chronic condition&Any symptom" = 0,   # Any condition & Chronic cond alone (not part of everyone set)
#                                "No symptom or condition&Chronic condition&Any symptom" = healthcare_needing_pop$both))
# 
# 
