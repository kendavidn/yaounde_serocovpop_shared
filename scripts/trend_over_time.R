# from Kenya paper https://science.sciencemag.org/content/371/6524/79

igg_over_time <- 
  yao %>% 
  mutate(week = week(dt_quest)) %>% 
  filter(!is.na(cat_igg_result)) %>% 
  group_by(week) %>% 
  summarise(pos = sum(cat_igg_result == "Positive"), 
            neg = sum(cat_igg_result == "Negative"),
            prop_pos = pos/sum(counter)) %>% 
  mutate(wilsonCI = DescTools::BinomCI(n = pos + neg, x = pos)) %>% 
  rowwise() %>% 
  mutate(wilsonCI_lower = wilsonCI[[2]]) %>% 
  mutate(wilsonCI_upper = wilsonCI[[3]]) %>% 
  ungroup() %>% 
  select(-wilsonCI) %>% 
  mutate(type = "igg") %>% 
  select(type, week, prop_pos, wilsonCI_upper, wilsonCI_lower)

igm_over_time <- 
  yao %>% 
  mutate(week = week(dt_quest)) %>% 
  filter(!is.na(cat_igm_result)) %>% 
  group_by(week) %>% 
  summarise(pos = sum(cat_igm_result == "Positive"), 
            neg = sum(cat_igm_result == "Negative"),
            prop_pos = pos/sum(counter)) %>% 
  mutate(wilsonCI = DescTools::BinomCI(n = pos + neg, x = pos)) %>% 
  rowwise() %>% 
  mutate(wilsonCI_lower = wilsonCI[[2]]) %>% 
  mutate(wilsonCI_upper = wilsonCI[[3]]) %>% 
  ungroup() %>% 
  select(-wilsonCI) %>% 
  mutate(type = "igm") %>% 
  select(type, week, prop_pos, wilsonCI_upper, wilsonCI_lower)


seroprev_over_time <- 
  igg_over_time %>% 
  bind_rows(igm_over_time) 

week_labels <-  
  seroprev_over_time %>% 
  mutate(first_day_monday_std = ISOweek2date(paste0("2020-W", week, "-1"))) %>% 
  mutate(first_day_monday = ISOweek2date(paste0("2020-W", week, "-1"))) %>% 
  mutate(first_day_monday = format.Date(first_day_monday, "%b %d")) %>% 
  mutate(last_day_sunday = ISOweek2date(paste0("2020-W", week, "-7"))) %>% 
  mutate(last_day_sunday = format.Date(last_day_sunday, "%b %d")) %>% 
  mutate(week_start_to_end = paste0(first_day_monday, "-\n", last_day_sunday )) %>% 
  select(week, first_day_monday_std, first_day_monday, week_start_to_end) %>% 
  unique() 


seroprev_over_time_plot <- 
  seroprev_over_time %>% 
  ggplot(aes(x = week, y = prop_pos, colour = type)) + 
  geom_line(aes(group = type), size = 1) + 
  geom_point() + 
  geom_errorbar(aes(ymin = wilsonCI_lower, ymax = wilsonCI_upper), width = 0.25) +
  labs(x = "",  y = "Positive proportion") +
  scale_color_manual(values = c(alpha("firebrick3", 0.6), 
                                alpha("lightseagreen", 0.8)), 
                     labels = c("IgG", "IgM"), name = "") + 
  scale_x_continuous(breaks = c(week_labels$week), 
                     labels = c(week_labels$week_start_to_end))


# number of tests over time

nine_palette <- paletteer_d("awtools::a_palette") %>% as.character() %>% str_sub(end = 7)
nine_palette <- c(nine_palette, "#785549")

tests_over_time_plot <- 
  yao %>% 
  filter(!is.na(cat_igg_result) & !is.na(cat_igm_result)) %>% 
  group_by(dt_quest, loc_hhld_area) %>% 
  summarise(tested = sum(counter)) %>% 
  ggplot() + 
  facet_wrap(~loc_hhld_area, ncol = 1) +
  geom_col(aes(x = dt_quest, y = tested, fill = loc_hhld_area))+ 
  labs(x = "Date",  y = "Daily number of samples") + 
  scale_x_continuous(breaks = c(week_labels$first_day_monday_std), 
                     labels = c(week_labels$first_day_monday)) + 
  scale_fill_manual(values = nine_palette, name = "District", 
                    guide = guide_legend(nrow  = 3)) 
  


trend_over_time_plot <- 
  plot_grid(seroprev_over_time_plot, 
          tests_over_time_plot, ncol = 1, scale = 0.95, 
          labels = "AUTO")
  
  
  
  
  
  
  


