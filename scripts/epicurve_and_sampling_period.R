library(tidyverse)

## start date of sampling
start_date <- as.Date("2020-10-14")
end_date <- as.Date("2020-11-26")
mid_date = start_date + -(start_date- end_date)/2

epicurve_and_sampling <- 
  rio::import(here::here("data/owid_covid_data.csv")) %>% 
  tibble::as_tibble() %>% 
  filter(iso_code == "CMR") %>% 
  select(date, new_cases) %>% 
  mutate(year = lubridate::epiyear(date)) %>% 
  mutate(epiweek = lubridate::epiweek(date)) %>% 
  group_by(year, epiweek) %>% 
  mutate(cases_per_week = sum(new_cases)) %>% 
  ungroup() 



  
epicurve_and_sampling_plot <- 
  epicurve_and_sampling %>% 
  ggplot() + 
  geom_col(aes(x = date, cases_per_week), color = "dodgerblue4", alpha = 0.5) + 
  scale_x_date(date_breaks = "8 weeks", 
               date_labels = "%b %d %Y", 
               expand = expansion(add = c(0, 20))) + 
  annotate("rect", xmin = start_date, xmax = end_date, ymin= 0,
           ymax = max(epicurve_and_sampling$cases_per_week), fill = "sienna3",
           alpha = 0.2) + 
  annotate("text", x = mid_date, y = 5000, angle = 90, hjust = 0.5,label = "Study period",
           color = "black", size = 4, fontface = "bold") + 
  labs(x = "Date", y = "Nationwise COVID-19 cases per week")
  
