

set.seed(11)
intersecting_inside <- euler(c(
                                "Negative" = sum(yao$cat_igg_result == "Negative" & yao$cat_igm_result == "Negative", na.rm = T), 
                               "IgG Positive" = 0, #B
                               "IgM Positive" = 0,  #c
                               "Negative&IgG Positive" = sum(yao$cat_igg_result == "Positive" & yao$cat_igm_result == "Negative", na.rm = T),
                               
                               
                               "Negative&IgM Positive"  = sum(yao$cat_igm_result == "Positive" & yao$cat_igg_result == "Negative", na.rm = T), 
                               "IgG Positive&IgM Positive" = 0,  # Not part of the all_respondents set
                               "Negative&IgG Positive&IgM Positive" = sum(yao$cat_igg_result == "Positive" & yao$cat_igm_result == "Positive", na.rm = T) ))

seropos_euler_plot <- 
  plot(intersecting_inside,
     fills = list(fill = c(alpha("dodgerblue2", 0.5),
                           alpha("firebrick3", 0.6),
                           alpha("lightseagreen", 0.8),
                           "",
                           "",
                           "",
                           alpha("darkslategrey", 0.8) )),
     #legend = list(side = "right"), 
     labels = list(fontfamily = "Avenir Next", 
                   cex=0.7
                   ),
     quantities = list(type = c("counts", "percent"),
                       cex= 0.6 ,
                       font = 1, 
                       fontfamily = "Avenir Next"
                       ), 
     edges = list(col = "transparent"))


# 
# yao %>% 
#   select(id_ind, cat_igg_result, cat_igm_result) %>% 
#   mutate(across(.fns = ~ replace_na(.x, "NA"))) %>% 
#   mutate(cat_igg_positive = ifelse(cat_igg_result == "Positive", TRUE, FALSE)) %>% 
#   mutate(cat_igg_NA = ifelse(cat_igg_result == "NA", TRUE, FALSE)) %>% 
#   mutate(cat_igm_positive = ifelse(cat_igm_result == "Positive", TRUE, FALSE)) %>% 
#   mutate(cat_igm_NA = ifelse(cat_igm_result == "NA", TRUE, FALSE)) %>% 
#   ggvenn::ggvenn(c("cat_igg_positive", "cat_igg_NA", 
#                    "cat_igm_positive", "cat_igm_NA"
#                    ))

