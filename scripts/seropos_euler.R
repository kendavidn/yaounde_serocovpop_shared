intersecting_inside <- euler(c("Negative" = yao %>% filter(cat_igg_result == "Negative" & cat_igm_result == "Negative") %>% nrow(), 
                               "IgG Positive" = 0, #B
                               "IgM Positive" = 0,  #c
                               "Negative&IgG Positive" = yao %>% filter(cat_igg_result == "Positive") %>% nrow(), 
                               "Negative&IgM Positive"  = yao %>% filter(cat_igm_result == "Positive") %>% nrow(), 
                               "IgG Positive&IgM Positive" = 0,  # Not part of the all_respondents set
                               "Negative&IgG Positive&IgM Positive" = yao %>% filter(cat_igg_result == "Positive" & cat_igm_result == "Positive") %>% nrow()))



seropos_euler_plot <- 
  plot(intersecting_inside,
     fills = list(fill = c(alpha("dodgerblue2", 0.4),
                           alpha("firebrick3", 0.55),
                           alpha("darkgoldenrod1", 0.8),
                           "",
                           "",
                           "",
                           alpha("chocolate3", 0.9) )),
     legend = list(side = "right"), 
     quantities = list(type = "counts",
                       font = 3, 
                       family = "Avenir Next"), 
     edges = list(col = "transparent", 
                  family = "Avenir Next"))

