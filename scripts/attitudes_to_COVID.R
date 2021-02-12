
yes_count <-
  function(df, count_col, ...) {
    
    yes_recode <- quos(...)
    
    df %>%
      mutate("{{ count_col }}" := case_when(!!!yes_recode)) %>% 
      count({{ count_col }}) %>%
      mutate(pct = 100 * n / sum(n)) %>%
      ungroup() %>%
      mutate(pct = round(pct, 0),
             pct_scale = scales::rescale(pct,
                                         from = c(0, max(pct, na.rm = T)),
                                         to = c(0, 80)),
             pct_scale = as.numeric(pct_scale),
             pct_n = paste0("**", pct, "%**", "<br>", "<span style='font-size:9pt;'>", "n = ", n, "</span>"), # no % or "n = "
             too_small = pct < 15,
             hjust = ifelse(too_small, -0.1, 1.1)) %>% 
      filter({{ count_col }} == "Yes")
  }

yes_plot <- 
  function(df, count_col, question_label, title = NULL, subtitle = NULL, caption = NULL){
    df %>% 
      {ggplot(.) + 
          geom_col(aes(x = pct, y = {{ count_col }}), fill = my_green) + 
          geom_col(aes(x = 100, y = {{ count_col }}), fill = alpha(my_lightgreen, 0.3) ) + 
          geom_richtext(data = filter(., too_small), aes(x = pct, y = {{ count_col }}, label = pct_n, hjust = hjust),
                        size = 4, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines"), color = "black") +
          geom_richtext(data = filter(., !too_small), aes(x = pct, y = {{ count_col }}, label = pct_n, hjust = hjust), 
                        size = 4, lineheight = 0, fill = "transparent", label.colour = NA, label.padding = unit(c(0.2, 0, 0.1, 0), "lines"), color = "white") +
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
                                                   maxwidth = unit(4.4, "in"),
                                                   padding = margin(4, 4, 2, 4),
                                                   margin = margin(0, 0, 2, 0), 
                                                   face = "bold.italic", 
                                                   color = "gray20", 
                                                   family = "Avenir Next",
                                                   size = 9), 
            axis.line.y = element_line()) + 
          labs(y = question_label, title = title, subtitle = subtitle, caption = caption)}
    
  }

# 
# is_respecting_hygiene_plot <- 
#   yao %>% 
#   yes_count(is_respecting_hygiene, 
#             is_respecting_hygiene == "Definitely yes" ~ "Yes", 
#             is_respecting_hygiene == "Partly" ~ "Yes" 
#             ) %>% 
#   yes_plot(count_col = is_respecting_hygiene, 
#            question_label = "Do you respect hygiene protocols (wash hands frequently, sneeze in your elbow, use a disposable tissue?)", 
#            title =  "**Attitudes and behaviors**<br><span style='font-size:8pt;'>regarding COVID-19 among all survey respondents</span>", 
#            subtitle = ' % answering "Yes"') 
# 
# 
# is_respecting_distancing_plot <- 
#   yao %>% 
#   yes_count(is_respecting_distancing, 
#             is_respecting_distancing == "Definitely yes" ~ "Yes", 
#             is_respecting_distancing == "Partly" ~ "Yes" ) %>%   
#   yes_plot(is_respecting_distancing, 
#            question_label = "Do you follow social distancing recommendations (avoid handshakes or hugs, avoid unnecessary outings, etc.)")

# 
# is_staying_confined_plot <- 
#   yao %>% 
#   yes_count(is_staying_confined, 
#             is_staying_confined == "Definitely yes" ~ "Yes", 
#             is_staying_confined == "Partly" ~ "Yes" ) %>%   
#   yes_plot(is_staying_confined, 
#            question_label = "Since March 1st have you gone into confinement?")


is_fearful_COVID_plot <- 
  yao %>% 
  yes_count(is_fearful_COVID, 
            is_fearful_COVID == "Yes" ~ "Yes") %>%   
  yes_plot(is_fearful_COVID, 
           question_label = "Since March 1st, have you been worried about contracting COVID-19?", 
                       title =  "**Attitudes and behaviors**<br><span style='font-size:8pt;'>regarding COVID-19 among all survey respondents</span>", 
                       subtitle = ' % answering "Yes"')


is_agreement_stigma_problem_plot <- 
  yao %>% 
  yes_count(is_agreement_stigma_problem, 
            is_agreement_stigma_problem == "Yes" ~ "Yes") %>%   
  yes_plot(is_agreement_stigma_problem, 
           question_label = "Do you think that people with COVID-19 are stigmatized and ostracized by others?")


is_fearful_stigma_plot <- 
  yao %>% 
  yes_count(is_fearful_stigma, 
            is_fearful_stigma == "Worried about stigma" ~ "Yes") %>%   
  yes_plot(is_fearful_stigma, 
           question_label = "Since March 1st, have you been worried about the reaction of those around you if you were to get COVID-19?")


is_agreement_hide_COVID_plot <- 
  yao %>% 
  yes_count(is_agreement_hide_COVID, 
            is_agreement_hide_COVID == "Yes" ~ "Yes") %>%   
  yes_plot(is_agreement_hide_COVID, 
           question_label = "Would you hide your illness if you were contaminated by COVID-19?")



attitudes_to_COVID_plot <- 
  # is_respecting_hygiene_plot / 
  # is_respecting_distancing_plot /
  #is_staying_confined_plot /
  is_fearful_COVID_plot /
  is_agreement_stigma_problem_plot /
  is_fearful_stigma_plot/
  is_agreement_hide_COVID_plot



