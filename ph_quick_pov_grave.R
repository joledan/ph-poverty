
# 
# col <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d")
# 
# # plot - geography
# f1 <- ggplot() + 
#   theme_minimal(base_family = "Noto Sans") +
#   geom_sf(data = df_pov_ph, 
#           aes(fill = qt),
#           color="#999999",
#           linewidth = 0.1) +
#   scale_fill_manual(values = col,
#                     breaks = c("-40", "-30", "-20", "-10", "0", "10", "20"),
#                     labels = c("-40", "", "-20", "", "0", "", "+20")) +
#   geom_sf(data = places, 
#           aes(color = "black"),
#           color = "black",
#           shape = places$shape) +
#   geom_sf_text_repel(data = places, 
#                      aes(label = NAME, 
#                          geometry = geometry,
#                          family = "Noto Sans"), 
#                      nudge_x = c(-200000), 
#                      nudge_y = c(0),
#                      segment.size = .5, 
#                      size = 3) +
#   coord_sf(datum = NA,
#            xlim = c(-200000, 1000000),
#            ylim = c(500000, 2400000)) + # adjust size of plot?
#   theme(plot.title = element_text(face="bold",
#                                   size=12,
#                                   margin=margin(t=10, r=0, b=3, l=10, "pt")),
#         plot.subtitle = element_text(size=10,
#                                      margin=margin(t=0,r=0,b=0,l=0, "pt")),
#         #plot.margin = unit(c(t=10,r=10,b=10,l=10), "pt"),
#         plot.caption = element_markdown(hjust = 0,
#                                     size = 6,
#                                     color="grey",
#                                     margin=margin(t=0,r=0,b=0,l=0, "pt")),
#         plot.caption.position = "plot",
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title = element_blank()) +
#   guides(color=guide_legend(nrow = 1),
#          fill=guide_legend(nrow = 1,
#                            label.position = "bottom",
#                            label.hjust = 1)) +
#   theme(legend.position = c(0.73,.94), # where legend goes in plot
#         legend.margin= margin(t=0,r=0,b=0,l=0, "pt"), # adjust padding in legend
#         legend.justification = "right",
#         legend.direction = "horizontal", # horizontal legend
#         legend.title = element_blank(), # no legend title
#         legend.spacing.y= unit(1, "pt"), # modify keys in legend
#         legend.spacing.x= unit(0, "pt"),
#         legend.key.width = unit(25, "pt"),
#         legend.key.height = unit(10, "pt"),
#         legend.text = element_text(size = 8)) +  
#   labs(title = "Poverty rates in the Philippines",
#        subtitle = "Percentage point change, 2015-2021",
#        caption = "**Source:** Philippine Statistics Authority • **Visual:** Jan Oledan") +
#   annotate("text", 
#            x = 590000, 
#            y = 2450000, 
#            label = "Increased →",
#            family = "Noto Sans",
#            size = 8/3) +
#   annotate("text", 
#            x = -130000, 
#            y = 2450000, 
#            label = "← Decreased",
#            family = "Noto Sans",
#            size = 8/3)
# 
# f1


##### plot 3 - urbanisation and poverty #####
df_urban_file <- paste(dataraw,
                       "1_2020%20CPH_Urban%20Population_PR%20Statistical%20Tables_RML_063022_ONS%20%282%29.xlsx",
                       sep = "/")

# read urban population data
df_urban <- read_excel(path = df_urban_file,
                       sheet = "Table A",
                       skip = 4) %>%
  rename(province = "...1",
         total_pop_2020 = "Total Population",
         total_pop_2015 = "...3",
         urban_pop_2020 = "Urban Population",
         urban_pop_2015 = "...6",
         pct_urban_2020 = "Percent Urban",
         pct_urban_2015 = "...9") %>%
  filter(!is.na(province), !is.na(total_pop_2020)) %>%
  select(-c("...4", "...7")) %>%
  mutate(province = case_when(
    province == "PHILIPPINES 1, 2" ~ "PHILIPPINES", 
    province == "CORDILLERAADMINISTRATIVEREGION(CAR)" ~ "CORDILLERA ADMINISTRATIVE REGION (CAR)",
    province == "IN MUSLIM MINDANAO (BARMM)" ~ "Autonomous Region in Muslim Mindana Bangsamoro Autonomous Region in Muslim Mindanao (ARMM/BARMM)",
    province == "Samar (Western Samar)" ~ "Samar",
    province == "Davao de Oro (Compostela Valley)" ~ "Davao de Oro",
    province == "Cotabato (North Cotabato)" ~ "North Cotabato",
    province == "NATIONAL CAPITAL REGION (NCR)" ~ "Metropolitan Manila",
    province == "Basilan (excluding City of Isabela)" ~ "Basilan",
    province == "Maguindanao (including City of Cotabato)" ~ "Maguindanao",
    T ~ province
  )) %>%
  mutate(across(c(total_pop_2020:pct_urban_2015), ~as.numeric(.)),
         region = ifelse(str_detect(province, "Region|REGION|PHILIPPINES"), 1, 0)) %>%
  filter(!str_detect(province, "City|Municipality"),
         region == 0, 
         province %notin% c("Interim Province", "Davao Occidental"))


# get results from string matching
urban_pop_string <- string_match(from = unique(df_urban$province),
                                 to = unique(ph$NAME_1)) %>%
  mutate(NAME_1 = ifelse(province=="Davao de Oro", "Davao de Oro", NAME_1))

# join with string matched province names 
df_urban1 <- df_urban %>%
  left_join(urban_pop_string) %>%
  select(-rank, -Similarity, -province) %>%
  full_join(df_pov1, relationship = "many-to-many") %>%
  relocate(NAME_1, province, island_group) %>%
  filter(province %notin% c("1st District", "2nd District", "3rd District", "4th District")) %>%
  mutate(province = ifelse(NAME_1 == "Metropolitan Manila", "Metropolitan Manila", province),
         island_group = ifelse(island_group == "Luzon", "Luzon*", island_group),
         pct_urban_change = pct_urban_2020 - pct_urban_2015) 
# isabela city, cotabato city are in the PH data, but not in shapefile data
# drop from data

##### plotting plot 3 #####
# want to see urban x poverty plotted
highlights_line <- c("Luzon*"="#56B4E9",
                     "NCR" = "#E69F00",
                     "Visayas" = "#009E73",
                     "Mindanao" = "#CC79A7",
                     "Philippines" = "#999999")

cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# line plot - overtime, major island groups, geography

#### segment plot - too messy? ####
# doesn't say much
# f3 <- ggplot(data = df_urban1) +
#   theme_minimal(base_family = "Noto Sans") +
#   # geom_point(aes(x = pct_urban_2020, 
#   #                y = poverty_incidence_2021,
#   #                color = island_group, 
#   #                group = island_group),
#   #           size = 2,
#   #           alpha = 0.7) + 
#   # geom_point(aes(x = pct_urban_2015, 
#   #                y = poverty_incidence_2015,
#   #                color = island_group, 
#   #                group = island_group),
#   #            size = 2,
#   #            alpha = 0.7) +
#   geom_segment(aes(x = pct_urban_2015, 
#                    y = poverty_incidence_2015, 
#                    xend = pct_urban_2020, 
#                    yend = poverty_incidence_2021, 
#                    colour = island_group),
#                arrow = arrow(length = unit(5, "pt"))) +
# # 
# #   geom_smooth(aes(y = pct_urban_2020,
# #                   x = poverty_incidence_2021),
# #               method = "lm", se = FALSE) +
#   scale_color_manual(values = highlights_line) +
#   scale_x_continuous(position = "bottom",
#                      expand = c(0,1),
#                      limits = c(1, 110)) +
#   scale_y_continuous(position = "right") + 
#   theme(panel.border = element_blank(),
#         axis.title.x = element_blank(), # adjust x axis title
#         axis.title.y = element_blank(), # adjust y axis title
#         axis.ticks.x = element_line(colour="black"),
#         axis.ticks.y = element_blank(), # remove y axis ticks
#         axis.line.x = element_line(), # adjust x axis line
#         #axis.ticks.length.x = unit(.1, "cm"), # adjust tick length
#         #panel.grid.major.x = element_blank(), # remove major x lines
#         panel.grid.minor.x = element_blank(), # remove minor x lines
#         panel.grid.minor.y = element_blank(),
#         plot.margin = margin(0, 10, 0, 10),
#         #legend.position = "none",
#         axis.text.x = element_text(size=8),
#         axis.text.y = element_text(size=8,
#                                    vjust = -.5,
#                                    hjust = 0)) +
#   theme(axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = -11))) +
#   th +
#   # trick to move axis labels into plot!!
#   labs(title = "Urban poverty",
#        subtitle = "Urbanization rate and poverty rate, 2015",
#        caption = "**Source:** Philippine Statistics Authority • **Visual:** Jan Oledan") +
#   theme(plot.title = element_text(face="bold",
#                                   size=12,
#                                   margin=margin(t=10, r=0, b=2, l=10, "pt")),
#         plot.subtitle = element_text(size=10,
#                                      margin=margin(t=0,r=0,b=0,l=0, "pt")),
#         #plot.margin = unit(c(t=10,r=10,b=10,l=10), "pt"),
#         plot.caption = element_markdown(hjust = 0,
#                                         size = 6,
#                                         color="grey",
#                                         margin=margin(t=0,r=0,b=0,l=0, "pt")),
#         plot.caption.position = "plot",
#         axis.title = element_blank()) 
# 
# f3


#### scatter plot ####
f3 <- ggplot(aes(y = pct_urban_2020, 
                 x = poverty_incidence_2021),
             data = df_urban1) +
  geom_point(aes(colour = island_group, 
                 group = island_group), 
             alpha = 0.7,
             size = 3) + 
  # geom_text_repel(aes(label = NAME_1),
  #                 color = "gray20",
  #                 data = subset(df_urban1, NAME_1 %in% label),
  #                 force = 5) +
  # geom_smooth(aes(x = pct_urban_2020,
  #                 y = poverty_incidence_2021,
  #                 group = island_group,
  #                 colour = island_group),
  #             method = "lm", 
  #             se = FALSE,
  #             linetype = "dashed") +
coord_cartesian(clip = "off") +
  th +
  scale_color_manual(values = highlights_line) +
  scale_x_continuous(position = "bottom",
                     expand = c(0,.1),
                     limits = c(0, 66),
                     breaks = seq(0, 60, 20)) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  labs(title = "Urban poverty",
       subtitle = "Urbanization rate and poverty rate by province, 2015",
       caption = "**Source:** Philippine Statistics Authority • **Visual:** Jan Oledan")

f3

# 
# label <- c("Apayao", "Sulu", "Metropolitan Manila")