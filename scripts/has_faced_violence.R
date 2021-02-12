#~  Count function ----

count_and_prop_impact <-
  function(df, count_col, group_col) { # colnames as strings so that function works with purrr:map without any other funky syntax decorations
    group_col <- as.name(group_col) 
    count_col <- as.name(count_col)
    df %>%
      group_by({{ group_col }}) %>%
      count({{ count_col }}) %>%
      mutate(pct = 100 * n / sum(n)) %>%
      ungroup() %>%
      mutate(pct = round(pct, 0),
             pct_scale = scales::rescale(pct,
                                         from = c(0, max(pct, na.rm = T)),
                                         to = c(0, 80)),
             pct_scale = as.numeric(pct_scale),
             pct_n = paste0("**", pct, "%**", "<br>", "<span style='font-size:5pt;'>", "n = ", n, "</span>"), # no % or "n = "
             too_small = pct < 15,
             hjust = ifelse(too_small, -0.1, 1.1)) %>%
      # drop NA category
      filter(!is.na({{ group_col }})) %>% 
      rename(var = {{ group_col }})
  }



# ~~ Build sections ---------------------------

t_all <- 
  yao %>% 
  mutate(all_respondents = "All respondents") %>%
  count_and_prop_impact("has_faced_violence", "all_respondents")

t_sex <- 
  yao %>% 
  count_and_prop_impact("has_faced_violence", "cat_sex")

t_age <- 
  yao %>% 
  filter(!is.na(cat_age)) %>%
  # relevel age. Might be unleveled from regresssions
  mutate(cat_age = factor(cat_age, levels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  count_and_prop_impact("has_faced_violence", "cat_age")

t_educ <- 
  yao %>% 
  filter(!is.na(cat_educ)) %>%
  mutate(cat_educ = fct_lump(cat_educ, prop = 0.06, other_level = "Other level"), 
         cat_educ = factor(cat_educ, levels = c("No formal instruction", "Primary", 
                                                "Secondary", "University", "Doctorate", "Other level" ))) %>% 
  count_and_prop_impact("has_faced_violence", "cat_educ")

t_occup <-
  yao %>%
  separate_rows(mcat_occup, sep = "--") %>%
  mutate(mcat_occup = fct_lump(mcat_occup, prop = 0.06, other_level = "Other occupation")) %>% # space before Other so it's unique after combining
  count_and_prop_impact("has_faced_violence", "mcat_occup")

t_symptoms <- 
  yao %>% 
  count_and_prop_impact("has_faced_violence", "has_COVID_symptoms")

t_pos <- 
  yao %>% 
  count_and_prop_impact("has_faced_violence", "cat_pos")


# ~~ Combine sections ---------------------------

t_combined_wide <-
  t_all %>%
  bind_rows(data.frame(var = "Gender", filler = TRUE)) %>%
  bind_rows(t_sex) %>%
  bind_rows(data.frame(var = "Age groups", filler = TRUE)) %>%
  bind_rows(t_age) %>%
  bind_rows(data.frame(var = "Highest education level", filler = TRUE)) %>%
  bind_rows(t_educ) %>%
  bind_rows(data.frame(var = "Occupation", filler = TRUE)) %>%
  bind_rows(t_occup) %>%
  bind_rows(data.frame(var = "Reported COVID-like symptoms", filler = TRUE)) %>%
  bind_rows(t_symptoms) %>%
  bind_rows(data.frame(var = "Antibody test result", filler = TRUE)) %>%
  bind_rows(t_pos) %>%
  mutate(filler = replace_na(filler, FALSE)) %>% 
  pivot_wider(id_cols = c(var, filler), names_from = has_faced_violence, values_from = c(n, pct, pct_scale, pct_n, too_small, hjust)) %>%
  mutate(var = factor(var, levels = rev(fct_inorder(unique(.$var)))))


# ~~ Plot  ---------------------------

label_space <-
  t_combined_wide %>%
  mutate(variable_length = str_length(var)) %>%
  summarise(max_length = max(variable_length, na.rm = T)) %>%
  pull(1) * 2.5

xend <- 140
seg_size <- 6
richtext_size <- 2
top_buffer <- 1

has_faced_violence_plot <- 
  t_combined_wide %>%
  ggplot() +
  geom_blank(aes(y = var)) + # helps "FIX" the order of the axis labels. Filtered dfs lose the factor order
  ## Side Labels
  # Regular labels
  geom_text(data = filter(t_combined_wide, filler == F), aes(y = var, x = label_space - 10, label = var), size = 3.5, color = "black", hjust = 1) +
  # Filler labels
  geom_text(data = filter(t_combined_wide, filler == T), aes(y = var, x = 0, label = var), size = 3.5, color = "black", hjust = 0, fontface = "bold") +
  # Dividers
  geom_segment(data = filter(t_combined_wide, filler == T), aes(y = var, yend = var), color = "gray50", linetype = "dashed", x = 0, xend = xend, size = 0.2, position = position_nudge(y = 0.25)) +
  ## Columns
  # Yes
  annotate("text", y = nrow(t_combined_wide) + top_buffer, x = label_space + 0, label = "% answering Yes", size = 3, color = my_green, hjust = 0, fontface = "bold") +
  geom_segment(aes(y = var, yend = var, x = label_space + 0, xend = label_space + 0 + pct_scale_Yes), size = seg_size, color = my_green) +
  geom_richtext(aes(y = var, x = label_space + 0 + pct_scale_Yes, label = pct_n_Yes, color = too_small_Yes, hjust = hjust_Yes), size = richtext_size, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines")) +
  # # No
  # annotate("text", y = nrow(t_combined_wide) + top_buffer, x = label_space + 100, label = "No", size = 3.5, color = "black", hjust = 0, fontface = "bold") +
  # geom_segment(aes(y = var, yend = var, x = label_space + 100, xend = label_space + 100 + pct_scale_No), size = seg_size, color = "#723E48") +
  # geom_richtext(aes(y = var, x = label_space + 100 + pct_scale_No, label = pct_n_No, color = too_small_No, hjust = hjust_No), size = richtext_size, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines")) +
  # # No response
  # annotate("text", y = nrow(t_combined_wide) + top_buffer, x = label_space + 200, label = "No response", size = 3.5, color = "black", hjust = 0, fontface = "bold") +
  # geom_segment(aes(y = var, yend = var, x = label_space + 200, xend = label_space + 200 + pct_scale_NR), size = seg_size, color = "#7a7a7a") +
  # geom_richtext(aes(y = var, x = label_space + 200 + pct_scale_NR, label = pct_n_NR, color = too_small_NR, hjust = hjust_NR), size = richtext_size, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines")) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, xend), expand = expansion(add = c(10, 0))) +
  scale_color_manual(values = c("white", "black")) +
  coord_cartesian(clip = "off") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.title = element_textbox_simple(size = 14, lineheight = 1, padding = margin(0, 0, 5, 0)),
        plot.subtitle = element_text(face = "italic", color = "gray30", hjust = 0),
        panel.grid = element_blank()) +
  labs(x = "",y = "",
       title = '<br>**Violence in the household** <br> <span style = "font-size:9pt; color:#595959;">*"Since March 1st, have you faced any physical or psychological violence in your household?"* </span>',
       caption = ""
  )

