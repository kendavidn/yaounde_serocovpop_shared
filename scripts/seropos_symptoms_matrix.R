
df <- 
  yao %>% 
  group_by(cat_igg_result, has_COVID_compatible_symp) %>% 
  summarise(count = n()) %>%
  mutate(cat_igg_result_count = sum(count),
         prop = count/sum(count)) %>%
  ungroup() %>% 
  mutate(has_COVID_compatible_symp = recode(has_COVID_compatible_symp, 
                                 "Yes" = "with symptoms", 
                                 "No" = "no symptoms"
                                 )) %>% 
  mutate(text_paste = glue::glue("IgG {str_to_lower(cat_igg_result)}<br>{has_COVID_compatible_symp} <br> <b> {percent(prop, 0.1)} </b> ({count}) "))

df_pos <- 
  df %>% 
  filter(cat_igg_result == "Positive") %>% 
  arrange(-row_number()) %>% 
  mutate(cum_prop = cumsum(prop))
  

df_neg <- df %>% 
  filter(cat_igg_result == "Negative") %>% 
  arrange(-row_number()) %>% 
  mutate(cum_prop = cumsum(prop))


igg_neg_color <- alpha("dodgerblue2", 0.5)
igg_pos_color <- alpha("firebrick3", 0.6)
symp_color <- alpha("darkgoldenrod1", 0.8)
  

plot_pos <- 
  df_pos %>% 
  ggplot(aes(x = 1, y = prop, fill = has_COVID_compatible_symp)) +
  geom_col(colour = "white") +
  geom_richtext(aes(x = 1, y = cum_prop, label = text_paste), 
                fill = alpha("white", 0.7), vjust = 1.5, size = 2.5
                ) + # if labels are desired
  scale_fill_manual(values = c(alpha(igg_pos_color, 0.3), alpha(igg_pos_color, 0.7))) +
  theme_void() + 
  theme(legend.position = "none")

scale_df_pos <- df_pos$cat_igg_result_count[1]

plot_neg <- 
  df_neg %>% 
  ggplot(aes(x = 1, y = prop, fill = has_COVID_compatible_symp)) +
  geom_col(colour = "white") +
  geom_richtext(aes(x = 1, y = cum_prop, label = text_paste), 
                fill = alpha("white", 0.5), vjust = 1.5, hjust = 0.5, size = 2.5,
  ) + # if labels are desired
  scale_fill_manual(values = c(alpha(igg_neg_color, 0.3), alpha(igg_neg_color, 0.7))) +
  theme_void() + 
  theme(legend.position = "none")


scale_df_neg <- df_neg$cat_igg_result_count[1]


seropos_symptoms_matrix_plot <- 
  plot_grid(plot_pos, plot_neg, 
          labels = c("IgG positive", "IgG negative"), 
          label_size = 7.7,
          label_x = c(-0.18, 0.18),
          rel_widths = c(scale_df_pos, scale_df_neg))

