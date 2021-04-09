# 
# set.seed(4)
# intersecting_inside <- 
#   euler(c(
#     # technically the "No symptoms of condition" set should be called the "everyone" set. But leaving it this way makes the final venn diagram easier to interpret
#     "IgG seronegative, no symptoms" = 
#       yao %>% 
#       filter(cat_igg_result == "Negative") %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(has_acute_symp == "No") %>% 
#       nrow(),
#     # "IgG seropositive" = 0, # Impossible (not part of everyone set)
#     #  "With Any acute symptom(s)" = 0,  # Impossible (not part of everyone set)
#     #  "With COVID-suspect symptom(s)" = 0,  # Impossible (not part of everyone set),
#     "IgG seronegative, no symptoms&IgG seropositive" = #seropos_symptoms_df$sero_alone,
#       yao %>% 
#       filter(cat_igg_result == "Positive") %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(has_acute_symp == "No") %>% 
#       nrow(),
#     "IgG seronegative, no symptoms&Any acute symptom(s)" = #seropos_symptoms_df$acute_alone,
#       yao %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(cat_igg_result == "Negative") %>% 
#       filter(has_acute_symp == "Yes") %>% 
#       nrow(),
#     # "IgG seronegative, no symptoms&With COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
#     "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)" = #seropos_symptoms_df$sero_and_acute,
#       yao %>% 
#       filter(cat_igg_result == "Positive") %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(has_acute_symp == "Yes") %>% 
#       nrow(),
#     "IgG seronegative, no symptoms&IgG seropositive&COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
#     "IgG seronegative, no symptoms&Any acute symptom(s)&COVID-suspect symptom(s)" = #seropos_symptoms_df$acute_and_COVIDlike, 
#       yao %>% 
#       filter(cat_igg_result == "Negative") %>% 
#       filter(has_COVID_symptoms =="Yes") %>%  
#       filter(has_acute_symp == "Yes") %>% 
#       nrow(),
#     "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)&COVID-suspect symptom(s)" = #seropos_symptoms_df$sero_and_acute_and_COVIDlike
#       yao %>% 
#       filter(cat_igg_result == "Positive") %>% 
#       filter(has_COVID_symptoms =="Yes") %>%  
#       filter(has_acute_symp == "Yes") %>% 
#       nrow()
#       ))
# 
# seropos_symptoms_euler_plot <- 
#   plot(intersecting_inside,
#      fills = list(fill = c(alpha("dodgerblue2", 0.5),
#                            alpha("firebrick3", 0.55),
#                            alpha("darkgoldenrod1", 0.3),
#                            alpha("darkgoldenrod1", 0.8)
#      )),   
#      legend = list(side = "bottom",
#                    fontfamily = "Avenir Next", font =  1,
#                    cex=0.7),
#      # labels = list(fontfamily = "Avenir Next", 
#      #               cex=0.7
#      # ),
#      quantities = list(type = c("counts", "percent"),
#                        cex= 0.6 ,
#                        font = 1, 
#                        fontfamily = "Avenir Next"
#      ), 
#      edges = list(col = "transparent"))
# 

# Simplified Euler diagram. Shows anyone with a covid-compatible symptom. 
# Leaves out



set.seed(11)
intersecting_inside <- euler(c(
  "IgG negative, no symptoms" =  
    yao %>% 
    filter(cat_igg_result == "Negative") %>% 
    filter(has_acute_symp == "No") %>% 
    nrow(), 
  
  "IgG positive" = 0,
  "Any COVID-compatible symptom" = 0,
  
  
  "IgG negative, no symptoms&IgG positive" =    
    yao %>% 
    filter(cat_igg_result == "Positive") %>% 
    filter(has_acute_symp == "No") %>% 
    nrow(),
  
  
  "IgG negative, no symptoms&Any COVID-compatible symptom"  = 
    yao %>% 
    filter(cat_igg_result == "Negative") %>% 
    filter(has_acute_symp == "Yes") %>% 
    nrow(),
  
  
  "IgG positive&Any COVID-compatible symptom" = 0, 
  
  
  "IgG negative, no symptoms&IgG positive&Any COVID-compatible symptom" =
    yao %>% 
    filter(cat_igg_result == "Positive") %>% 
    filter(has_acute_symp == "Yes") %>% 
    nrow()
    
    ))

seropos_euler_plot <- 
  plot(intersecting_inside,
       fills = list(fill = c(alpha("dodgerblue2", 0.5),
                             alpha("firebrick3", 0.6),
                             alpha("darkgoldenrod1", 0.8))),
       legend = list(side = "bottom",
                     fontfamily = "Avenir Next", font =  1,
                     cex=0.7),
       # labels = list(fontfamily = "Avenir Next", 
       #               cex=0.7
       # ),
       quantities = list(type = c("counts", "percent"),
                         cex= 0.6 ,
                         font = 1, 
                         fontfamily = "Avenir Next"
       ), 
       edges = list(col = "transparent"))
  

# 
# 
# set.seed(4)
# intersecting_inside <- 
#   euler(c(
#     # technically the "No symptoms of condition" set should be called the "everyone" set. But leaving it this way makes the final venn diagram easier to interpret
#     "IgG seronegative, no symptoms" = 
#       yao %>% 
#       filter(cat_igg_result == "Negative") %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(has_acute_symp == "No") %>% 
#       nrow(),
#     # "IgG seropositive" = 0, # Impossible (not part of everyone set)
#     #  "With Any acute symptom(s)" = 0,  # Impossible (not part of everyone set)
#     #  "With COVID-suspect symptom(s)" = 0,  # Impossible (not part of everyone set),
#     "IgG seronegative, no symptoms&IgG seropositive" = #seropos_symptoms_df$sero_alone,
#       yao %>% 
#       filter(cat_igg_result == "Positive") %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(has_acute_symp == "No") %>% 
#       nrow(),
#     "IgG seronegative, no symptoms&Any acute symptom(s)" = #seropos_symptoms_df$acute_alone,
#       yao %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(cat_igg_result == "Negative") %>% 
#       filter(has_acute_symp == "Yes") %>% 
#       nrow(),
#     # "IgG seronegative, no symptoms&With COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
#     "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)" = #seropos_symptoms_df$sero_and_acute,
#       yao %>% 
#       filter(cat_igg_result == "Positive") %>% 
#       filter(has_COVID_symptoms =="No") %>%  
#       filter(has_acute_symp == "Yes") %>% 
#       nrow(),
#     "IgG seronegative, no symptoms&IgG seropositive&COVID-suspect symptom(s)" = 0, # impossible (not part of acute symptoms set)
#     "IgG seronegative, no symptoms&Any acute symptom(s)&COVID-suspect symptom(s)" = #seropos_symptoms_df$acute_and_COVIDlike, 
#       yao %>% 
#       filter(cat_igg_result == "Negative") %>% 
#       filter(has_COVID_symptoms =="Yes") %>%  
#       filter(has_acute_symp == "Yes") %>% 
#       nrow(),
#     "IgG seronegative, no symptoms&IgG seropositive&Any acute symptom(s)&COVID-suspect symptom(s)" = #seropos_symptoms_df$sero_and_acute_and_COVIDlike
#       yao %>% 
#       filter(cat_igg_result == "Positive") %>% 
#       filter(has_COVID_symptoms =="Yes") %>%  
#       filter(has_acute_symp == "Yes") %>% 
#       nrow()
#   ))
# 
# seropos_symptoms_euler_plot <- 
#   plot(intersecting_inside,
#        fills = list(fill = c(alpha("dodgerblue2", 0.5),
#                              alpha("firebrick3", 0.55),
#                              alpha("darkgoldenrod1", 0.3),
#                              alpha("darkgoldenrod1", 0.8)
#        )),   
#        legend = list(side = "bottom",
#                      fontfamily = "Avenir Next", font =  1,
#                      cex=0.7),
#        # labels = list(fontfamily = "Avenir Next", 
#        #               cex=0.7
#        # ),
#        quantities = list(type = c("counts", "percent"),
#                          cex= 0.6 ,
#                          font = 1, 
#                          fontfamily = "Avenir Next"
#        ), 
#        edges = list(col = "transparent"))
# 

