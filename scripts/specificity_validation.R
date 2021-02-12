# Specificity validation 

spec_val <- read_excel(here("data/specificity_validation_yaounde.xls"), sheet = 1)




pos_age_sex <- 
  spec_val %>% 
  rename(cat_sex = Gender, 
         val_age = Age, 
         igg = `Ig G_Covid-19`) %>% 
  mutate(cat_age = cut(val_age, 
                       breaks = c(4.9,14,29,44,65,100), 
                       labels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  mutate(cat_sex = recode(cat_sex, "1" = "Male", "2" = "Female"), 
         igg = recode(igg, "1" = "Positive", "2" ="Negative")) %>% 
  mutate(pos_test = NA,
         pos_test = ifelse(igg == "Positive", 1, pos_test), 
         pos_test = ifelse(igg == "Negative", 0, pos_test)) %>% 
  mutate(neg_test = NA,
         neg_test = ifelse(igg == "Negative", 1, neg_test), 
         neg_test = ifelse(igg == "Positive", 0, neg_test)) %>% 
  group_by(cat_age, cat_sex) %>% 
  summarise(neg_test = sum(neg_test, na.rm = T), 
            pos_test = sum(pos_test, na.rm = T)) %>% 
  mutate(total_count = neg_test + pos_test) %>% 
  ungroup()


# chi square gender
chisquare_sex_pval <- 
  (pos_age_sex %>% 
  group_by(cat_sex) %>% 
  summarise(neg_test = sum(neg_test), 
            pos_test = sum(pos_test)) %>% 
  ungroup() %>% 
  column_to_rownames("cat_sex") %>% 
  chisq.test())$p.value


# chi square age groups
chisquare_age_pval <- 
  (pos_age_sex %>% 
     group_by(cat_age) %>% 
     summarise(neg_test = sum(neg_test), 
               pos_test = sum(pos_test)) %>% 
     ungroup() %>% 
     column_to_rownames("cat_age") %>% 
     filter(row_number() != nrow(.)) %>% 
     chisq.test())$p.value


# chi square by donor category (not sure what the categories actually mean)

spec_val %>% 
  transmute(cat_donor = `Donor code`, 
         igg = `Ig G_Covid-19`) %>% 
  mutate(cat_donor = case_when(str_detect(cat_donor, "hcd") ~ "hcd", 
                               str_detect(cat_donor, "st") ~ "st", 
                               str_detect(cat_donor, "CNPS") ~ "CNPS", 
                               str_detect(cat_donor, "CME") ~ "CME", 
                               str_detect(cat_donor, "HDMB") ~ "HDMB", 
                               str_detect(cat_donor, "CMAN") ~ "CMAN", 
                               str_detect(cat_donor, "CSCB") ~ "CSCB",
                               TRUE ~ "no prefix"
                               )
         ) %>% 
  group_by(cat_donor) %>% 
  count(igg) %>% 
  pivot_wider(id_cols = cat_donor, names_from = igg, values_from =  n) %>% 
  mutate(pct = round(100* `1` / (`1`+`2`), 2) )
  



pos_age_sex_long <- 
  pos_age_sex %>% 
  select(-total_count) %>% 
  pivot_longer(names_to = "classification", cols = c(neg_test, pos_test)) %>% 
  mutate(classification  = fct_rev(classification)) 

neg_color <- "dodgerblue2"
pos_color <- "firebrick1"
text_color <- "firebrick4"

#pos_age_sex_pyramid <- 
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
  labs(x = "Count of IgG false positives and true negatives", y = "", 
       title = "Abbott PanBio IgG specificity evaluation") +
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        axis.text.y = element_text(face = "bold", size = 10, color = "black")) +
  # expand the scale so that text labels are not cut off
  scale_x_continuous(limits = range(c(pos_age_sex$total_count * 1.25, -pos_age_sex$total_count* 1.25 ),na.rm=T) ,
                     labels = abs) +
  scale_y_discrete(expand = expansion(add = c(0,1.4)))



pos_age_sex <- 
  spec_val %>% 
  rename(cat_sex = Gender, 
         val_age = Age, 
         igm = `Ig M_Covid-19`) %>% 
  mutate(cat_age = cut(val_age, 
                       breaks = c(4.9,14,29,44,65,100), 
                       labels = c("5 - 14", "15 - 29", "30 - 44", "45 - 64", "65 +"))) %>% 
  mutate(cat_sex = recode(cat_sex, "1" = "Male", "2" = "Female"), 
         igm = recode(igm, "1" = "Positive", "2" ="Negative")) %>% 
  mutate(pos_test = NA,
         pos_test = ifelse(igm == "Positive", 1, pos_test), 
         pos_test = ifelse(igm == "Negative", 0, pos_test)) %>% 
  mutate(neg_test = NA,
         neg_test = ifelse(igm == "Negative", 1, neg_test), 
         neg_test = ifelse(igm == "Positive", 0, neg_test)) %>% 
  group_by(cat_age, cat_sex) %>% 
  summarise(neg_test = sum(neg_test, na.rm = T), 
            pos_test = sum(pos_test, na.rm = T)) %>% 
  mutate(total_count = neg_test + pos_test) %>% 
  ungroup()



pos_age_sex_long <- 
  pos_age_sex %>% 
  select(-total_count) %>% 
  pivot_longer(names_to = "classification", cols = c(neg_test, pos_test)) %>% 
  mutate(classification  = fct_rev(classification)) 

neg_color <- "dodgerblue2"
pos_color <- "firebrick1"
text_color <- "firebrick4"

#pos_age_sex_pyramid <- 
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
  labs(x = "Count of false positives and true negatives from IgM validation", y = "", 
       title = "Abbott PanBio IgM specificity evaluation") +
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        axis.text.y = element_text(face = "bold", size = 10, color = "black")) +
  # expand the scale so that text labels are not cut off
  scale_x_continuous(limits = range(c(pos_age_sex$total_count * 1.25, -pos_age_sex$total_count* 1.25 ),na.rm=T) ,
                     labels = abs) +
  scale_y_discrete(expand = expansion(add = c(0,1.4)))

