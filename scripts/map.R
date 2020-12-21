
yao_map <- read_sf(here("maps/cite_verte_maps_shp/cite_verte_maps_poly.shp"))

# extract centroids
# centroid_points_in <- 
#   cbind(yao_map, st_coordinates(st_centroid(yao_map$geometry)))
# 
# centroid_points <- 
#   data.frame( region = centroid_points_in[[2]],
#               long = centroid_points_in[[4]], 
#               lat = centroid_points_in[[5]]) %>% 
#   as_tibble()

region_centroids <- read_excel(here("maps/adjusted_region_labels.xlsx"))

region_prevalence <-
  yao %>%
  group_by(loc_hhld_area) %>%
  summarise(n_pos = sum(cat_pos_num, na.rm = T),
            prev = n_pos / sum(counter)) %>%
  mutate(title = loc_hhld_area)

hhlds_pie <- 
  yao %>% 
  group_by(id_hhld) %>% 
  mutate(n_pos_in_hhld = sum(cat_pos_num, na.rm = T), 
         prop_pos = n_pos_in_hhld/sum(counter), 
         prop_neg = 1-prop_pos) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(id_hhld, loc_hhld_lat, loc_hhld_long, prop_pos, prop_neg, n_hhld_indiv) %>% 
  filter(!is.na(loc_hhld_lat)) %>% 
  filter(loc_hhld_lat > 3.872 & 
           loc_hhld_long < 11.515)  %>% 
  # add jitter to latitude. Otherwise geom_scatterpie behaves funny
  mutate(loc_hhld_lat = loc_hhld_lat + sample( (1:10e5)/10e9, size = nrow(.)))
 

yao_map %>% 
  left_join(region_prevalence) %>% 
  ggplot() + 
  geom_sf(aes(fill = prev), color = "gray50", size = 0.4) +
  annotation_scale(location = "bl", width_hint = 0.5, ) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text(data = region_centroids, 
            aes(long_adjusted, lat_adjusted, label = region), color = "black") + 
  geom_segment(data = subset(region_centroids, segment == "Yes") , 
               aes(long, lat, xend = long_adjusted, yend = lat_adjusted - 0.001), color = "black") +
  scale_size(range = c(1, 7), breaks = c(1,2,5,10)) + 
  labs(x = "", y = "" , 
       title = "Household size and proportion of household that tested 
       <span style = 'color:#ba210d;'>positive </span>and<span style = 'color:#1759d4;'> negative </span> <br>for SARS-CoV-2 antibodies", 
       subtitle = "Fill color indicates overall prevalence in region"
       ) + 
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(linetype = "dashed", color = "gray93"), 
        axis.line.x.bottom = element_blank(),
        legend.title = element_text(size = 10), 
        legend.position = c(0.88, 0.64), 
        plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0), 
        plot.subtitle = element_text(hjust = 0)
  ) + 
  scale_x_continuous(expand = expansion(add = c(0.001, 0.006))) + 
  #scale_fill_viridis_c(option = "A", begin = 0.7, end = 1, direction = -1, alpha = 0.9, 
  #                     name = "Regional\nprevalence") + 
  # scale_fill_gradient(low = "#CEFFFF", high = "#FDC0C0", name = "Regional\nprevalence") +
  #   scale_fill_viridis_c(option = "A", begin = 0.8, end = 1, direction = -1, alpha = 0.9, 
  #                        name = "Regional\nprevalence") + 
  scale_fill_gradientn(colours = c("#FDFDC5", "#FFD8A1", "#fa9866"), name = "Regional\nprevalence") +
  #scale_fill_gradientn(colours = c("#DBC4D6", "white", "#9CE3DD"), name = "Regional\nprevalence") +
  #scale_fill_gradientn(colours = c("#9CE3DD", "#F5F5F5", "#FDC0C0"), name = "Regional\nprevalence") +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(data = hhlds_pie, 
                  aes(x = loc_hhld_long,
                      y = loc_hhld_lat, group = id_hhld, r = sqrt(n_hhld_indiv/20000000)), 
                  cols = c("prop_pos", "prop_neg"),  color= NA) +
  scale_fill_manual(values = c(alpha("firebrick3", 0.7), alpha("dodgerblue2", 0.6)) ,
                    labels = c("Positive", "Negative"), name = "Test result" ) +
  scale_color_manual(values = c(alpha("firebrick3", 0.5), alpha("dodgerblue2", 0.5)) ) + 
  geom_scatterpie_legend(sqrt(hhlds_pie$n_hhld_indiv/20000000), 
                         x=11.512, 
                         y=3.9155, 
                         n=2, 
                         labeller=function(x) round(((x)^2)*20000000, 0) )  + 
  annotate("text", label = "No. of residents", x = 11.515, y = 3.918, fontface = "bold", size = 3.5 )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  moon map ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# region_prevalence <-
#   yao %>%
#   group_by(loc_hhld_area) %>%
#   summarise(n_pos = sum(cat_pos_num, na.rm = T),
#             prev = n_pos / sum(counter)) %>%
#   mutate(title = loc_hhld_area)
# 
# hhlds_for_map1 <- 
#   yao %>% 
#   group_by(id_hhld) %>% 
#   mutate(n_pos_in_hhld = sum(cat_pos_num, na.rm = T), 
#          prop = n_pos_in_hhld/sum(counter)) %>% 
#   slice_head(n = 1) %>% 
#   ungroup() %>% 
#   filter(!is.na(loc_hhld_lat)) %>% 
#   filter(loc_hhld_lat > 3.872 & 
#            loc_hhld_long < 11.515
#            ) %>% 
#   select(n_hhld_indiv, prop, id_hhld, loc_hhld_lat, loc_hhld_long) %>% 
#   mutate(`Test result` = "Pos.",right = FALSE)
# 
# hhlds_for_map <- 
#   hhlds_for_map1 %>% 
#   mutate(prop = 1- prop) %>% 
#   mutate(`Test result` = "Neg.", right = TRUE) %>% 
#   bind_rows(hhlds_for_map1) %>% 
#   mutate(`Test result` = factor(`Test result`, levels = c("Pos.", "Neg.")))
# 
# 
# 
# yao_map %>% 
#   left_join(region_prevalence) %>% 
#   ggplot() + 
#   geom_sf(aes(fill = prev), color = "gray50", size = 0.4) +
#   annotation_scale(location = "bl", width_hint = 0.5, ) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   geom_text(data = centroid_points, 
#             aes(long_adjusted, lat_adjusted, label = region), color = "black") + 
#   geom_segment(data = subset(centroid_points, segment == "Yes") , 
#                aes(long, lat, xend = long_adjusted, yend = lat_adjusted - 0.001), color = "black") +
#   scale_size(range = c(1, 7), breaks = c(1,2,5,10)) + 
#   labs(size = "No. of hhld\nmembers", 
#        x = "", y = "", 
#        title = "Household size and proportion of household that tested 
#        <span style = 'color:#ba210d;'>positive </span>and<span style = 'color:#1759d4;'> negative </span> <br>for SARS-CoV-2 antibodies", 
#        subtitle = "Fill color indicates overall prevalence in region") + 
#   theme(panel.background = element_rect(fill = "white"), 
#         panel.grid.major = element_line(linetype = "dashed"), 
#         axis.line.x.bottom = element_blank(),
#         legend.title = element_text(size = 10), 
#         legend.position = "right", 
#         plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0), 
#         plot.subtitle = element_text(hjust = 0)
#   ) + 
#   scale_x_continuous(expand = expansion(add = c(0.001, 0.006))) + 
#   scale_fill_viridis_c(option = "A", begin = 0.7, end = 1, direction = -1, alpha = 0.9, 
#                        name = "Regional\nprevalence") + 
#   ggnewscale::new_scale_fill() +
#   geom_moon(data = hhlds_for_map, 
#             aes(x = loc_hhld_long, 
#                 y = loc_hhld_lat,  
#                 size = n_hhld_indiv, 
#                 ratio = prop, 
#                 color = `Test result`,
#                 fill = `Test result`,
#                 right = right),
#             key_glyph = draw_key_full_moon) + 
#   scale_fill_manual(values = c(alpha("firebrick3", 0.5), alpha("dodgerblue2", 0.5))) +
#   scale_color_manual(values = c(alpha("firebrick3", 0.5), alpha("dodgerblue2", 0.5)) ) + 
#   guides(size = guide_legend(order = 1))

  


 