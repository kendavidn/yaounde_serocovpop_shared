
pos_age_sex <- 
  yao %>% 
  mutate(cat_age = factor(cat_age, levels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  # create a counter column
  # mutate(pos_test = if_else(cat_igg_result == "Positive" | cat_igm_result == "Positive", 1, 0)) %>% 
  # mutate(neg_test = if_else(cat_igg_result == "Negative" & cat_igm_result == "Negative", 1, 0)) %>% 
  mutate(pos_test = NA,
         pos_test = ifelse(cat_igg_result == "Positive", 1, pos_test), 
         pos_test = ifelse(cat_igg_result == "Negative", 0, pos_test)) %>% 
  mutate(neg_test = NA,
         neg_test = ifelse(cat_igg_result == "Negative", 1, neg_test), 
         neg_test = ifelse(cat_igg_result == "Positive", 0, neg_test)) %>% 
  group_by(cat_age, cat_sex) %>% 
  summarise(neg_test = sum(neg_test, na.rm = T), 
            pos_test = sum(pos_test, na.rm = T)) %>% 
  mutate(total_count = neg_test + pos_test) %>% 
  ungroup() %>% 
  mutate(cat_sex = recode(cat_sex, "femme" = "Female", "homme" = "Male"))



pos_age_sex_long <- 
  pos_age_sex %>% 
  select(-total_count) %>% 
  pivot_longer(names_to = "classification", cols = c(neg_test, pos_test)) %>% 
  mutate(classification  = fct_rev(classification)) 

neg_color <- my_lightgreen
pos_color <- my_green
text_color <- my_darkgreen

pos_age_sex_pyramid <- 
  pos_age_sex_long %>%
  ggplot() +
  # males
  geom_bar(data=subset(pos_age_sex_long, cat_sex=="Male"),
           aes(y=cat_age,x=value,fill=classification),
           stat="identity",position = "stack", color="white", width = 1,size = 0.1) +
  # add a scale. We don't want a legend for cases. Only deaths, so we pass only a single value to the breaks argument
  scale_fill_manual( breaks = c("Positive"), values = c(pos_color, neg_color )) +
  # ggnewscale allows us to fill males and females with a slightly different color for cases
  ggnewscale::new_scale_fill() +
  # females
  geom_bar(data=subset(pos_age_sex_long, cat_sex=="Female"),
           # negative values for the women
           aes(y=cat_age, x=-value,fill=classification), 
           stat="identity",position = "stack", color="white", width = 1,size = 0.1) +
  # add a scale. We don't want a legend for cases. Only deaths, so we pass only a single value to the breaks argument
  scale_fill_manual( breaks = c("Positive"), values = c(pos_color, neg_color)) +
  # Pos rate labels for males and females (bold font)
  geom_label(data=subset(pos_age_sex,cat_sex=="Male"),
             aes(y=cat_age, x=total_count, label = paste0("Pos: ", round(100* pos_test/total_count, 1), "%")),
             size = 2.7,  fontface = "bold", color = text_color,
             hjust = 0, vjust = 0.2, label.size = 0, fill = "transparent") +
  geom_label(data=subset(pos_age_sex,cat_sex=="Female"),
             aes(y=cat_age, x=-total_count, label = paste0("Pos: ", round(100* pos_test/total_count, 1), "%")),
             size = 2.7,  fontface = "bold", color = text_color,  
             hjust = 1, vjust = 0.2, label.size = 0, fill = "transparent") +
  # Pos and total count labels (plain font)
  geom_label(data=subset(pos_age_sex,cat_sex=="Male"),
             aes(y=cat_age, x=total_count, label = paste0("(",pos_test, " of ", total_count, ")")),
             size = 2.5,  fontface = "plain", color = text_color, 
             hjust = 0, vjust = 1, label.size = 0, fill = "transparent") +
  geom_label(data=subset(pos_age_sex,cat_sex=="Female"),
             aes(y=cat_age, x=-total_count, label = paste0("(",pos_test, " of ", total_count, ")")),
             size = 2.5,  fontface = "plain", color = text_color, 
             hjust = 1, vjust = 1, label.size = 0, fill = "transparent") +
  # Female and male labels
  # We put in manual annotations for where to place the FEMALE and MALE headers.
  # these are anchored to the mean value of case counts * 2.2
  annotate("label", y= 6, x = -mean(pos_age_sex$total_count)* 1.2, label = "Female", size = 4, fontface = "bold", colour = "white", fill =  "black") +
  annotate("label", y= 6, x = mean(pos_age_sex$total_count)* 1.2, label = "Male", size = 4 , fontface = "bold", colour = "white", fill = "black" ) +
  labs(x = "Count of IgG seropositive and seronegative individuals", y = "") +
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        axis.text.y = element_text(face = "bold", size = 10, color = "black")) +
  # expand the scale so that text labels are not cut off
  scale_x_continuous(limits = range(c(pos_age_sex$total_count * 1.25, -pos_age_sex$total_count* 1.25 ),na.rm=T) ,
                     labels = abs) +
  scale_y_discrete(expand = expansion(add = c(0,1.4)))
