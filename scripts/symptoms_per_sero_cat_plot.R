
sero_symptom_chi_square <- function(df, symp_name){
  
  p_val <- (df %>% 
              filter(mcat_symp == symp_name) %>% 
              # count those without the symp. There's prob a cleaner way
              add_row(cat_pos = "Positive", 
                      mcat_symp = paste0("No", symp_name), 
                      n = .$sum[which(.$cat_pos == "Positive")] - 
                        .$n[which(.$cat_pos == "Positive")] ) %>% 
              add_row(cat_pos = "Negative", 
                      mcat_symp = paste0("No", symp_name), 
                      n = .$sum[which(.$cat_pos == "Negative")] - 
                        .$n[which(.$cat_pos == "Negative")] ) %>% 
              # pivot into contigency table
              select(-c(sum, pct)) %>% 
              pivot_wider(id_cols = cat_pos, names_from = mcat_symp, values_from = n) %>% 
              column_to_rownames(var="cat_pos") %>% 
              # run chi square
              chisq.test())$p.value
  
  out <- paste(symp_name, p_val, sep = "--")
  
  return(out)
  
}


count_pos <- 
  yao %>% 
  count(cat_pos) %>% 
  rename(sum = n)

pos_cnt_for_caption <- count_pos[1,2]
neg_cnt_for_caption <- count_pos[2,2]
seropos_color <- alpha("firebrick3", 0.7)
seroneg_color <-  alpha("dodgerblue2", 0.7) 

symptoms_per_sero_cat <- 
  yao %>% 
  separate_rows(mcat_symp, sep = "--") %>% 
  group_by(cat_pos, mcat_symp) %>% 
  count() %>%
  ungroup() %>% 
  left_join(count_pos) %>% 
  mutate(pct = 100*n/sum) %>% 
  filter(!is.na(cat_pos)) %>% 
  filter(mcat_symp != "No symptoms") 


symptoms_chi <- map(.x = unique(symptoms_per_sero_cat$mcat_symp), 
                    .f = ~ sero_symptom_chi_square(df = symptoms_per_sero_cat, symp_name = .x) ) %>% 
  flatten_chr() %>% 
  as_tibble() %>% 
  separate(col = value, into = c("mcat_symp", "p_val"), sep = "--", remove = TRUE) %>% 
  mutate(p_val = as.numeric(p_val))


symptoms_per_sero_cat_plot <- 
  symptoms_per_sero_cat %>%
  left_join(symptoms_chi) %>%
  mutate(mcat_symp = fct_reorder(mcat_symp, n)) %>%
  mutate(cat_pos = fct_rev(cat_pos)) %>%
  mutate(cat_pos = recode(cat_pos,
                          "Negative" = "IgG seronegative",
                          "Positive" = "IgG seropositive")) %>%
  mutate(p_val = round(p_val, digits = 2),
         p_val_paste = paste0("p = ", p_val), 
         pct = round(pct, 2),
         pct_1 = round(pct, 1),
         pct_paste = paste0("**", pct_1, "%","**&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;", "<span style='color:gray30'>", 
                            "(",  n, ")", "</span>"),
         #hjust = ifelse(pct < 2, 0, 1), 
         #color = ifelse(hjust == 0, "black", "white"), 
         hjust = -0.1,
         color = "black"
         )%>%
  {
    ggplot(.) +
      # bars
      geom_col(aes(x = pct, y = mcat_symp, fill = cat_pos),
               position = position_dodge(width = 0.8), width = 0.8) +
      # percent label for seropositive
      geom_richtext(data = filter(., cat_pos == "IgG seropositive"), 
                    aes(x = pct, y = mcat_symp, fill = cat_pos, 
                        label = pct_paste, hjust = hjust, color = color), 
                    position = position_nudge(y = +0.2), 
                    size = 2.5, fill = "transparent", label.color = NA,
                    label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")) +
      # percent label for seronegative
      geom_richtext(data = filter(., cat_pos == "IgG seronegative"), 
                    aes(x = pct,  y = mcat_symp, fill = cat_pos, 
                        label = pct_paste, hjust = hjust, color = color), 
                    position = position_nudge(y = -0.2), 
                    size = 2.5, fill = "transparent", label.color = NA,
                    label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines")) +
      # label significant diff
      geom_text(data = filter(., p_val < 0.05 & cat_pos == "IgG seropositive"),
                aes(y = mcat_symp, x = pct + 13.5), color = "black", fontface = "plain",
                label = "*", position = position_nudge(y = -0.05), lineheight = 0.3) +
      # label significant diff
      geom_text(data = filter(., p_val < 0.05 & cat_pos == "IgG seropositive"),
                aes(y = mcat_symp, x = pct + 12.5), color = "black", fontface = "plain",
                label = "I", position = position_nudge(y = 0), lineheight = 0.3, size = 10) +
      # # label significant diff
      # geom_text(data = filter(., p_val < 0.01 & cat_pos == "IgG seropositive"),
      #           aes(y = mcat_symp), x = -0.7,  color = "black", fontface = "plain",
      #           label = "**",  position = position_nudge(y = -0.05)) +
      scale_x_continuous(expand = expansion(add = c(1.2, 8))) +
      scale_fill_manual(values = c(seroneg_color, seropos_color)) +
      scale_color_manual(values = c("black", "white")) +
      labs(x = "Percentage with symptom", y = "", fill = "", 
           caption = paste("of", pos_cnt_for_caption, "seropositive and", 
                           neg_cnt_for_caption, "seronegative individuals"
           )
      ) + 
      theme(axis.text.y = element_text(face = "plain", color = "black", hjust = 1, vjust = 0.3),
            axis.title.x = element_text( face = "bold", size = 9), 
            plot.title.position = "plot",
            plot.subtitle = element_text(hjust = 0, size = 12, face = "bold"), 
            legend.position = c(0.75, 0.5), 
            legend.direction = "vertical", 
            plot.caption = element_text(face = "plain", color = "gray10")) +
      guides(fill = guide_legend(reverse = T), 
             color = F) 
  }
