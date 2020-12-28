
count_and_prop <-
  function(df, count_col, group_col) { # colnames as strings so that function works with purrr:map without any other funky syntax decorations
    group_col <- as.name(group_col) 
    count_col <- as.name(count_col)
    df %>%
      filter(!is.na({{ group_col }})) %>% 
      group_by({{ group_col }}) %>%
      count({{ count_col }}) %>%
      mutate(pct = 100 * n / sum(n)) %>%
      ungroup() %>%
      mutate(pct = round(pct, 0),
             pct_scale = scales::rescale(pct,
                                         from = c(0, max(pct, na.rm = T)),
                                         to = c(0, 80)),
             pct_n = paste0("**", pct, "%**", "<br>", "<span style='font-size:5pt;'>", "n = ", n, "</span>"), # no % or "n = "
             too_small = pct < 15,
             hjust = ifelse(too_small, -0.1, 1.1)) %>%
      rename(var = {{ group_col }})
  }

# ~~ Prep df ---------------------------

yao_prepped <-
  yao %>%
  filter(!is.na(has_confin_stopped_work)) %>%
  mutate(has_confin_stopped_work = recode(has_confin_stopped_work, "No response" = "NR")) %>%
  mutate(all_respondents = "All respondents") %>%
  mutate(cat_educ = fct_lump(cat_educ, prop = 0.07, other_level = "Other"))

# ~~ Build sections ---------------------------

t_all <- yao_prepped %>% count_and_prop("has_confin_stopped_work", "all_respondents")

t_sex <- yao_prepped %>% count_and_prop("has_confin_stopped_work", "cat_sex")

t_age <- yao_prepped %>% count_and_prop("has_confin_stopped_work", "cat_age")

t_educ <- yao_prepped %>% count_and_prop("has_confin_stopped_work", "cat_educ")

t_occup <-
  yao_prepped %>%
  separate_rows(mcat_occup, sep = "--") %>%
  mutate(mcat_occup = fct_lump(mcat_occup, prop = 0.07, other_level = " Other")) %>% # space before Other so it's unique after combining
  count_and_prop("has_confin_stopped_work", "mcat_occup")

t_pos <- yao_prepped %>% count_and_prop("has_confin_stopped_work", "cat_pos")

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
  bind_rows(data.frame(var = "Test result", filler = TRUE)) %>%
  bind_rows(t_pos) %>%
  pivot_wider(id_cols = c(var, too_small, filler, hjust), names_from = has_confin_stopped_work, values_from = c(n, pct, pct_scale, pct_n)) %>%
  mutate(var = factor(var, levels = rev(fct_inorder(unique(.$var))))) %>%
  mutate(filler = replace_na(filler, FALSE))


# ~~ Plot  ---------------------------

label_space <-
  t_combined_wide %>%
  mutate(variable_length = str_length(var)) %>%
  summarise(max_length = max(variable_length, na.rm = T)) %>%
  pull(1) * 2.5

xend <- 350
seg_size <- 6
richtext_size <- 2
rm_buffer <- 3.5

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
  annotate("text", y = nrow(t_combined_wide) - rm_buffer, x = label_space + 0, label = "Yes", size = 3.5, color = "black", hjust = 0, fontface = "bold") +
  geom_segment(aes(y = var, yend = var, x = label_space + 0, xend = label_space + 0 + pct_scale_Yes), size = seg_size, color = "#FF8247") +
  geom_richtext(aes(y = var, x = label_space + 0 + pct_scale_Yes, label = pct_n_Yes, color = too_small, hjust = hjust), size = richtext_size, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines")) +
  # No
  annotate("text", y = nrow(t_combined_wide) - rm_buffer, x = label_space + 100, label = "No", size = 3.5, color = "black", hjust = 0, fontface = "bold") +
  geom_segment(aes(y = var, yend = var, x = label_space + 100, xend = label_space + 100 + pct_scale_No), size = seg_size, color = "#723E48") +
  geom_richtext(aes(y = var, x = label_space + 100 + pct_scale_No, label = pct_n_No, color = too_small, hjust = hjust), size = richtext_size, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines")) +
  # No response
  annotate("text", y = nrow(t_combined_wide) - rm_buffer, x = label_space + 200, label = "No response", size = 3.5, color = "black", hjust = 0, fontface = "bold") +
  geom_segment(aes(y = var, yend = var, x = label_space + 200, xend = label_space + 200 + pct_scale_NR), size = seg_size, color = "#7a7a7a") +
  geom_richtext(aes(y = var, x = label_space + 200 + pct_scale_NR, label = pct_n_NR, color = too_small, hjust = hjust), size = richtext_size, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines")) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, xend), expand = expansion(add = c(50, 0))) +
  scale_color_manual(values = c("white", "black")) +
  coord_cartesian(clip = "off") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.title = element_textbox_simple(size = 14, lineheight = 1, padding = margin(0, 0, 5, 0)),
        plot.subtitle = element_text(face = "italic", color = "gray30", hjust = 0),
        panel.grid = element_blank()) +
  labs(x = "",y = "",
    title = "<br>**Work interruptions during the pandemic** <br> <span style = 'font-size:10pt; color:#595959;'>*Since March 1st, have you stopped working or stopped one of your jobs due to the confinement?* </span>",
    caption = "Note: Answer is missing for NN individuals"
  )
