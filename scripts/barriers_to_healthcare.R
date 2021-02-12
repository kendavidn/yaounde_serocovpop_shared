
yes_count <-
  function(df, count_col, ...) {
    
    yes_recode <- quos(...)
    
    df %>%
      count({{ count_col }}) %>%
      mutate(pct = 100 * n / sum(n)) %>%
      ungroup() %>%
      mutate(pct = round(pct, 0),
             pct_scale = scales::rescale(pct,
                                         from = c(0, max(pct, na.rm = T)),
                                         to = c(0, 80)),
             pct_scale = as.numeric(pct_scale),
             pct_n = paste0("**", pct, "%**", "<br>", "<span style='font-size:8pt;'>", "n = ", n, "</span>"), # no % or "n = "
             too_small = pct < 15,
             hjust = ifelse(too_small, -0.1, 1.1)) %>% 
      mutate("{{ count_col }}" := case_when(!!!yes_recode)) %>% 
      filter({{ count_col }} == "Yes")
  }

yes_plot <- 
  function(df, count_col, question_label, title = NULL, subtitle = NULL, caption = NULL){
    df %>% 
      {ggplot(.) + 
          geom_col(aes(x = pct, y = {{ count_col }}), fill = my_green) + 
          geom_col(aes(x = 100, y = {{ count_col }}), fill = alpha(my_lightgreen, 0.3) ) + 
          geom_richtext(
            data = .,
            #data = filter(., too_small), 
            aes(x = pct, y = {{ count_col }}, label = pct_n),
            hjust = -0.1, size = 3, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines"), color = "black") +
      #    geom_richtext(data = filter(., !too_small), aes(x = pct, y = {{ count_col }}, label = pct_n, color = too_small, hjust = hjust), 
       #                 size = 5, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines"), color = "white") +
          scale_x_continuous(limits  = c(0,100), expand = expansion(add = c(0,0))) + 
          #  scale_color_manual(values = c("white", "black")) +
          coord_cartesian(clip = "off") +
          theme_void() +
          theme(
            plot.title = element_textbox_simple(hjust = 0,
                                                face = "plain", 
                                                color = "black", 
                                                padding = margin(10, 4, 8, 0),
                                                size = 14, 
                                                family = "Avenir Next",
                                                lineheight = 0.0),
            plot.subtitle = element_textbox_simple(hjust = 0.5,
                                                   vjust = 0.5,
                                                   face = "bold", 
                                                   color = my_darkgreen, 
                                                   family = "Avenir Next",
                                                   size = 10),
            axis.title.y = element_textbox_simple( hjust = 0,
                                                   minwidth = unit(1, "in"),
                                                   maxwidth = unit(3.3, "in"),
                                                   padding = margin(4, 4, 2, 4),
                                                   margin = margin(0, 0, 2, 0), 
                                                   face = "bold.italic", 
                                                   color = "gray20", 
                                                   family = "Avenir Next",
                                                   size = 9), 
            axis.line.y = element_line()) + 
          labs(y = question_label, title = title, subtitle = subtitle, caption = caption)}
    
  }


has_diffic_pay_medic_plot <- 
  yao_has_chron_or_symp %>% 
  yes_count(has_diffic_pay_medic, has_diffic_pay_medic == "Some financial difficulty" ~ "Yes") %>% 
  yes_plot(count_col = has_diffic_pay_medic, 
           question_label = "Since March 1st, have you had financial difficulty in paying for medical costs?", 
           title = "**Barriers to care**<br><span style='font-size:8pt;'>among the *healthcare-needing* population</span>",
           subtitle = '**% Answering "Yes"**') 


has_insur_plot <- 
  yao_has_chron_or_symp %>% 
  yes_count(has_insur, has_insur == "Yes" ~ "Yes") %>% 
  yes_plot(count_col = has_insur, 
           question_label = "Do you have medical insurance?") 



has_delay_care_finance_plot <- 
  yao_has_chron_or_symp %>% 
  yes_count(has_delay_care_finance, has_delay_care_finance == "Yes" ~ "Yes") %>% 
  yes_plot(count_col = has_delay_care_finance, 
           question_label = "Since March 1st, have you had to delay medical care for financial reasons?") 


has_diffic_travel_care_plot <- 
  yao_has_chron_or_symp %>% 
  yes_count(has_diffic_travel_care, has_diffic_travel_care == "Some travel difficulty" ~ "Yes") %>% 
  yes_plot(has_diffic_travel_care, 
           question_label = "Have you had difficulty in accessing care due to inavailability, or price-hiking, of transportation?")

thinks_clinics_dangerous_COVID_plot <- 
  yao_has_chron_or_symp %>% 
  yes_count(thinks_clinics_dangerous_COVID, thinks_clinics_dangerous_COVID == "Yes, dangerous" ~ "Yes") %>% 
  yes_plot(thinks_clinics_dangerous_COVID, 
           question_label = "Since March 1st, have you felt that going into health care centers is dangerous because of COVID-19?")
# 
# thinks_clinics_closed_COVID_plot <- 
#   yao_has_chron_or_symp %>% 
#   yes_count(thinks_clinics_closed_COVID, thinks_clinics_closed_COVID == "Yes, closed" ~ "Yes") %>% 
#   yes_plot(thinks_clinics_closed_COVID, 
#            question_label = "Since March 1st, have you felt that going into health care centers is closed because of COVID-19?")
# 

barriers_to_healthcare_plot <- 
  has_diffic_pay_medic_plot / 
  has_delay_care_finance_plot/
  has_insur_plot/
  has_diffic_travel_care_plot / 
  thinks_clinics_dangerous_COVID_plot 

#/
  #thinks_clinics_closed_COVID_plot

