
pos_age_sex <- 
  yao %>% 
  mutate(cat_age = factor(cat_age, levels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  filter(!is.na(cat_igg_result) & !is.na(cat_igm_result)) %>% 
  # create a counter column
  # mutate(pos_test = if_else(cat_igg_result == "Positive" | cat_igm_result == "Positive", 1, 0)) %>% 
  # mutate(neg_test = if_else(cat_igg_result == "Negative" & cat_igm_result == "Negative", 1, 0)) %>% 
  # if igg result is positive and igm is negative, count as 1
  mutate(pos_igg_only = 0,
         pos_igg_only = ifelse(cat_igg_result == "Positive" &  cat_igm_result == "Negative", 1, pos_igg_only)) %>% 
  # if igm result is positive and igg is negative, count as 1
  mutate(pos_igm_only = 0,
         pos_igm_only = ifelse(cat_igm_result == "Positive" &  cat_igg_result == "Negative", 1, pos_igm_only)) %>% 
  # if both are positive, count as 1
  mutate(pos_igg_and_igm = 0,
         pos_igg_and_igm = ifelse(cat_igm_result == "Positive" &  cat_igg_result == "Positive", 1, pos_igg_and_igm)) %>%
  # if both are negative, count as 1
  mutate(neg_all = 0,
         neg_all = ifelse(cat_igm_result == "Negative" &  cat_igg_result == "Negative", 1, pos_igg_and_igm)) %>%
  group_by(cat_age, cat_sex) %>% 
  summarise(pos_igg_only = sum(pos_igg_only, na.rm = T), 
            pos_igm_only = sum(pos_igm_only, na.rm = T), 
            pos_igg_and_igm = sum(pos_igg_and_igm, na.rm = T),
            neg_all = sum(neg_all, na.rm = T)) %>% 
  mutate(total_count = pos_igg_only + pos_igm_only + pos_igg_and_igm + neg_all) %>% 
  ungroup() %>% 
  mutate(cat_sex = recode(cat_sex, "femme" = "Female", "homme" = "Male"))



pos_age_sex_long <- 
  pos_age_sex %>% 
  select(-total_count) %>% 
  pivot_longer(names_to = "classification", cols = c(pos_igg_only, pos_igm_only, pos_igg_and_igm, neg_all)) %>% 
  mutate(classification  = factor(classification, levels = c("pos_igg_only", "pos_igg_and_igm", "pos_igm_only","neg_all"))) 


pos_igg_only_color <- alpha("firebrick3", 0.6)
pos_igg_and_igm_color <- alpha("darkslategrey", 0.8)
pos_igm_only_color <- alpha("lightseagreen", 0.8)
neg_color <- alpha("dodgerblue2", 0.7) 
text_color <- "black"

pos_age_sex_pyramid_plot <- 
  pos_age_sex_long %>%
  ggplot() +
  # males
  geom_bar(data=subset(pos_age_sex_long, cat_sex=="Male"),
           aes(y=cat_age,x=value,fill=classification),
           stat="identity",position = "stack", color=NA, width = 0.985) +
  # females
  geom_bar(data=subset(pos_age_sex_long, cat_sex=="Female"),
           # negative values for the women
           aes(y=cat_age, x=-value,fill=classification), 
           stat="identity",position = "stack", color=NA, width = 0.985) +
  # scale
  scale_fill_manual(  values = c(pos_igg_only_color, pos_igg_and_igm_color, pos_igm_only_color,  neg_color), 
                      labels = c("IgG positive", "IgG & IgM positive", "IgM positive", "Negative")) +
  # Pos rate labels for males and females (bold font)
  geom_label(data=subset(pos_age_sex,cat_sex=="Male"),
             aes(y=cat_age, x=total_count, label = paste0(
                                                          #"Pos: ", 
                                                          round(100* (pos_igg_only + pos_igg_and_igm + pos_igm_only)/total_count, 1), "%")),
             size = 2.5,  fontface = "bold", color = text_color,
             hjust = 0, vjust = 0.2, label.size = 0, fill = "transparent") +
  geom_label(data=subset(pos_age_sex,cat_sex=="Female"),
             aes(y=cat_age, x=-total_count, label = paste0(
                                                          #"Pos: ", 
                                                          round(100* (pos_igg_only + pos_igg_and_igm + pos_igm_only)/total_count, 1), "%")),
             size = 2.5,  fontface = "bold", color = text_color,  
             hjust = 1, vjust = 0.2, label.size = 0, fill = "transparent") +
  # Pos and total count labels (plain font)
  geom_label(data=subset(pos_age_sex,cat_sex=="Male"),
             aes(y=cat_age, x=total_count, label = paste0("(",(pos_igg_only + pos_igg_and_igm + pos_igm_only), " of ", total_count, ")")),
             size = 2.6,  fontface = "plain", color = alpha(text_color, 0.8) , 
             hjust = 0, vjust = 1, label.size = 0, fill = "transparent") +
  geom_label(data=subset(pos_age_sex,cat_sex=="Female"),
             aes(y=cat_age, x=-total_count, label = paste0("(",(pos_igg_only + pos_igg_and_igm + pos_igm_only), " of ", total_count, ")")),
             size = 2.6,  fontface = "plain", color = alpha(text_color, 0.8), 
             hjust = 1, vjust = 1, label.size = 0, fill = "transparent") +
  # Female and male labels
  # We put in manual annotations for where to place the FEMALE and MALE headers.
  # these are anchored to the mean value of case counts * 2.2
  annotate("text", y= 5.4, x = -mean(pos_age_sex$total_count)* 1.7, label = "Female", size = 4, fontface = "bold", colour = "black") +
  annotate("text", y= 5.4, x = mean(pos_age_sex$total_count)* 1.7, label = "Male", size = 4 , fontface = "bold", colour = "black") +
  annotate("segment", y = 0.5, yend = 5.5, x = 0, xend = 0, color = "white", size = 0.5) +
  labs(x = "Count, IgG and/or IgM seropositivity", y = "Age group\n") +
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8, color = "black"), 
        axis.text.x = element_text(size = 8, color = "black")) +
  # expand the scale so that text labels are not cut off
  scale_x_continuous(limits = range(c(pos_age_sex$total_count * 1.35, -pos_age_sex$total_count* 1.35 ),na.rm=T) ,
                     labels = abs) +
  scale_y_discrete(expand = expansion(add = c(0,1.4))) + 
  guides(fill = guide_legend(override.aes = list(alpha = c(0.5) )))





