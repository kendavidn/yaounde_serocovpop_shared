
#yao_map <- read_sf(here("maps/cite_verte_maps_shp_official/cite_verte_maps_poly.shp"))


yao_map <- 
  read_sf(here("maps/cite_verte_maps_shp_official/9aires_de_sante.shp")) %>% 
  rename(id = Id, 
         title = Nom_AS) %>% 
  mutate(title = recode(title, 
                        "Cite verte" = "Cité Verte", 
                        "Tsinga oliga" = "Tsinga Oliga"))

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

scale_down_by <- 30e6

hhlds_pie <- 
  yao %>% 
  group_by(id_hhld) %>% 
  mutate(n_pos_in_hhld = sum(cat_pos_num, na.rm = T), 
         prop_pos = n_pos_in_hhld/sum(counter), 
         prop_neg = 1-prop_pos, 
         loc_hhld_lat = first(na.omit(loc_hhld_lat)), # coords recorded for at least one person in hhld
         loc_hhld_long = first(na.omit(loc_hhld_long))) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(id_hhld, loc_hhld_lat, loc_hhld_long, prop_pos, prop_neg, n_hhld_indiv) %>% 
  filter(!is.na(loc_hhld_lat)) %>%  # 1 household had missing coordinates
  # 4 households had coordinates outside the shape boundaries. remove for neatness
  filter(loc_hhld_lat > 3.86 & loc_hhld_long < 11.515) %>%
  filter(!(loc_hhld_lat < 3.871 & loc_hhld_long < 11.504)) %>%  
  # # add jitter to longitude and latitude to preserve location anonymity
  mutate(loc_hhld_long = loc_hhld_long + sample( (1:10e5)/10e8, size = nrow(.))) %>% 
  mutate(loc_hhld_long = loc_hhld_long - sample( (1:10e5)/10e8, size = nrow(.))) %>% 
  mutate(loc_hhld_lat = loc_hhld_lat + sample( (1:10e5)/10e8, size = nrow(.))) %>% 
  mutate(loc_hhld_lat = loc_hhld_lat - sample( (1:10e5)/10e8, size = nrow(.))) %>% 
  # nudge locations below 3.873 upwards as they are falling off plot area
  mutate(loc_hhld_lat = ifelse(loc_hhld_lat < 3.873, loc_hhld_lat + 0.001, loc_hhld_lat)) %>% 
  mutate(loc_hhld_long = ifelse(loc_hhld_long > 11.513, loc_hhld_long - 0.001, loc_hhld_long)) %>% 
  # we must define the radius precisely (based on the long-lat coord system from the original map)
  mutate(radius = sqrt(n_hhld_indiv/scale_down_by))
 

circles_repelled <- 
  circleRepelLayout(x = hhlds_pie[,c("loc_hhld_long", "loc_hhld_lat", "radius") ], 
                  xlim = range(hhlds_pie$loc_hhld_long),
                  ylim = range(hhlds_pie$loc_hhld_lat), 
                  xysizecols = c(1,2,3), 
                  sizetype = "radius", 
                  wrap = FALSE)

hhlds_pie$loc_hhld_long_repel <- circles_repelled$layout$x
hhlds_pie$loc_hhld_lat_repel <- circles_repelled$layout$y


map_plot <- 
  yao_map %>% 
  left_join(region_prevalence) %>% 
  ggplot() + 
  geom_sf(aes(fill = prev), color = "gray50", size = 0.4) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  geom_segment(data = subset(region_centroids, segment == "Yes") , 
               aes(long, lat, xend = long_adjusted, yend = lat_adjusted - 0.001), color = "black") +
  scale_size(range = c(1, 7), breaks = c(1,2,5,10)) + 
  labs(x = "", y = ""
      # , 
      # title = "Household size and proportion of household that tested 
      # <span style = 'color:#ba210d;'>positive </span>and<span style = 'color:#1759d4;'> negative </span> <br>for SARS-CoV-2 antibodies", 
      # subtitle = "Fill color indicates overall prevalence in region"
       ) + 
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.line.x.bottom = element_blank(),
        legend.title = element_text(size = 10), 
        legend.position = c(0.88, 0.78), 
        plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0), 
        plot.subtitle = element_text(hjust = 0)) + 
  scale_x_continuous(limits = c(11.465, 11.52)) + 
  scale_fill_gradientn(colours = c("#FDFDC5", "#FFD8A1", "#fa9866"), name = "Regional\nprevalence") +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(data = hhlds_pie, 
                  aes(x = loc_hhld_long_repel,
                      y = loc_hhld_lat_repel, group = id_hhld, r = radius), 
                  cols = c("prop_pos", "prop_neg"),  color= NA) +
  scale_fill_manual(values = c(alpha("firebrick3", 0.8), alpha("dodgerblue2", 0.8)) ,
                    labels = c("Positive", "Negative"), name = "IgG test result" ) +
  scale_color_manual(values = c(alpha("firebrick3", 0.8), alpha("dodgerblue2", 0.8)) ) + 
  geom_scatterpie_legend(hhlds_pie$radius, 
                         x=11.512, 
                         y=3.8955, 
                         n=2, 
                         labeller=function(x) round(((x)^2)*scale_down_by, 0) )  + 
  annotate("text", label = "No. of residents", x = 11.509, y = 3.898, fontface = "bold", size = 3.5, 
           hjust = 0) + 
  geom_text(data = region_centroids, 
            aes(long_adjusted, lat_adjusted, label = region), color = "black", fontface = "bold")

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

  


 