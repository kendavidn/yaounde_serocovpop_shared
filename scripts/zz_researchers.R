yao %>%
  group_by(name_researcher, cat_igg_result) %>%
  count() %>%
  pivot_wider(id_cols = name_researcher, names_from = cat_igg_result, values_from = n) %>%
  mutate(pct_pos = 100 *( Positive / (Negative + Positive + `NA`)), 
         total = Positive + Negative + `NA`, 
         pct_pos_paste = format(pct_pos, digits = 2), 
         pct_pos_paste = paste(pct_pos_paste, "%"), 
         Positive_paste = paste0("(", Positive, " out of ", Positive + Negative, ")"),
  ) %>%
  arrange(total) %>% 
  ungroup() %>% 
  mutate(row = row_number(), 
         letter = LETTERS[row]
  ) %>% 
  mutate(name_researcher = paste("Researcher", letter)) %>% 
  {ggplot(.) +
      geom_text(aes(x = pct_pos, y = name_researcher, label = pct_pos_paste), position = position_nudge(x = 6)) +
      geom_text(aes(x = pct_pos, y = name_researcher, label = Positive_paste), position = position_nudge(x = 24
      )) +
      geom_col(aes(x = pct_pos, y = name_researcher), fill = my_green) +
      annotate("col", y = .$name_researcher, x = 100, fill = alpha(my_green, 0.2)) + 
      labs(x = "Percentage positive")}

yao %>%
  group_by(name_researcher, cat_igg_result) %>%
  count() %>%
  pivot_wider(id_cols = name_researcher, names_from = cat_igg_result, values_from = n) %>%
  mutate(pct_pos = 100 *( Positive / (Negative + Positive + `NA`)), 
         total = Positive + Negative + `NA`, 
         pct_pos_paste = format(pct_pos, digits = 2), 
         pct_pos_paste = paste(pct_pos_paste, "%"), 
         Positive_paste = paste0("(", Positive, " out of ", Positive + Negative, ")"),
  ) %>% 
  ungroup() %>% 
  select(Negative, Positive) %>% 
  as.data.frame() %>% 
  chisq.test()
