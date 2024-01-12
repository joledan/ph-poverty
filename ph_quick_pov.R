# title: clean
# date: december 2023
# by: jan oledan
# desc: 

##### install and load packages #####
rm(list=ls())
# Package names
packages <- c("sf","tidyverse", "ggrepel", "devtools",
              "sysfonts", "extrafont", "readxl", 
              "patchwork", "lubridate", "ragg",
              "camcorder", "devtools", "geodata", "reticulate",
              "tidyterra", "janitor", "rnaturalearth",
              "ggtext")

# install packages not yet installed
install_package <- packages %in% rownames(installed.packages())
if (any(install_package == FALSE)) {
  install.packages(packages[!install_package])
}
invisible(lapply(packages, library, character.only = TRUE))

# python
# make a virtual environment for python (to keep all packages)
# also makes it easier (installing python on diff machines)
#reticulate::install_python(version = "3.9:latest")
#virtualenv_install("r-reticulate", "polyfuzz")
POLYFUZZ <- reticulate::import("polyfuzz")

# install ggsflabel from github to label SF plots
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

extrafont::loadfonts("win")

##### set up directories ######
main <- case_when(Sys.getenv("USERNAME") == "jgole" ~ "C:/Users/jgole/Dropbox/Portfolio",
                  Sys.getenv("USER") == "janoledan" ~ "/Users/janoledan/Dropbox/Portfolio")
dataout <- paste(main, "ph-quick/dataout", sep = "/")
dataraw <- paste(main, "ph-quick/dataraw", sep = "/")
plots <- paste(main, "ph-quick/plots", sep = "/")
code <- paste(main, "ph-quick/code", sep = "/")

# define %notin%
`%notin%` <- Negate(`%in%`)



##### theme define main ggplot theme components #####
th <- theme_minimal(base_family = "Noto Sans") + 
  theme(panel.border = element_blank(),
        #plot.background = element_rect(fill = '#FFFFFF'),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        plot.title = element_text(face = "bold", # plot title, subtitle, caption
                                  size = 12,
                                  margin = margin(t=0, r=0, b=2, l=0, "pt")),
        plot.subtitle = element_text(size = 10,
                                     margin = margin(t=0,r=0,b=5,l=0, "pt")),
        plot.caption = element_markdown(hjust = 0,
                                        size = 6,
                                        color = "#999999",
                                        margin = margin(t=5,r=0,b=0,l=0, "pt")),
        plot.caption.position = "plot") +
  theme(axis.title = element_blank(),
        axis.title.x = element_blank(), # adjust x axis title
        axis.title.y = element_blank(), # adjust y axis title
        axis.text.x = element_text(size = 8, # make axis text (labels) size 8, black font
                                   colour = "#000000"),
        axis.text.y = element_text(size = 8,
                                   colour = "#000000",
                                   vjust = -.5,
                                   hjust = 0),
        axis.ticks.length.x = unit(4, "pt"), # define tick length and colour for x-axis
        axis.ticks.x = element_line(colour="#000000"),
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.line.x = element_line(colour = "#000000"), # adjust x axis line
        panel.grid.major.x = element_blank(), # remove major x lines
        panel.grid.minor.x = element_blank(), # remove minor x lines
        panel.grid.minor.y = element_blank(),
        axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = -11, unit = "pt"))) # adjust tick length) )


#### intialize camcorder to set up and output plots ####
camcorder::gg_record(
  dir = plots,
  device = "png",
  scale = 1,
  width = 6,
  height = 3.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


##### plot 1 - province level poverty read data #####
# read admin1 ph data
ph <- as_sf(gadm(country = "PHL",
                 level = 1,
                 path = dataraw)) %>%
  st_transform(crs="EPSG:32651") %>%
  select(NAME_1, geometry)

# load places from Natural Earth, keep 3 major cities in the Ph
places <- ne_download(
  scale = "large",
  type = "populated_places",
  category = "cultural",
  load = T,
  returnclass = "sf") %>%
  filter(ADM0_A3 == "PHL") %>%
  filter(NAME %in% c("Manila")) %>%
  select(NAME, geometry) %>%
  mutate(shape = case_when(
    NAME == "Manila" ~ 18
  ),
  NAME = case_when(
    NAME == "Manila" ~ "NCR"
  ))

# read poverty incidence (%) and subsistence incidence files
# define island groups
luzon <- c("Region I (Ilocos Region)", "Region II (Cagayan Valley)",
           "Region III (Central Luzon)", "Region IV-A (CALABARZON)",
           "MIMAROPA Region", "Cordillera Administrative Region (CAR)",
           "Region V (Bicol Region)")
visayas <- c("Region VI (Western Visayas)", "Region VII (Central Visayas)", "Region VIII (Eastern Visayas)")
mindanao <- c("Region IX (Zamboanga Peninsula)", "Region X (Northern Mindanao)",
              "Region XI (Davao Region)", "Region XII (SOCCSKSARGEN)",
              "Region XIII (Caraga)", 
              "Autonomous Region in Muslim Mindana Bangsamoro Autonomous Region in Muslim Mindanao (ARMM/BARMM)")

# poverty incidence (rate) data
df_pov <- paste(dataraw,
                "1E3DF020.csv",
                sep = "/") %>%
  read_delim(delim=";",
             skip = 2) %>%
  clean_names() %>%
  rename_with(.fn = ~str_replace(.x, "2021p", "2021"),
              .cols = ends_with("2021p")) %>%
  rename_with(.fn = ~ paste0("poverty_incidence_", str_extract(., "(\\d+)$")), 
              .cols = starts_with("poverty_incidence")) %>%
  rename_with(.fn = ~ paste0("annual_pc_threshold_", str_extract(., "(\\d+)$")), 
              .cols = starts_with("annual_per_capita_poverty_threshold"))  %>%
  mutate(across(c(annual_pc_threshold_2015:poverty_incidence_2021), ~ as.numeric(.)),
         incidence_pctpoint_2018_2021 = poverty_incidence_2021-poverty_incidence_2018,
         incidence_pctpoint_2015_2021 = poverty_incidence_2021-poverty_incidence_2015,
  ) %>%
  filter(!is.na(annual_pc_threshold_2015)) %>%
  mutate(region_dummy = if_else(str_detect(geolocation, "Region|region|PHILIPPINES"), 1, 0),
         province_dummy = if_else(region_dummy == 0, 1, 0),
         geolocation = str_replace_all(geolocation, c("[a-z]\\/" = "" ,"[0-9]\\/" = "" ,"[\\.\\,]" = "")),
         geolocation = str_trim(geolocation),
         region = if_else(region_dummy == 1, geolocation, NA),
         province = if_else(province_dummy == 1, geolocation, NA)) %>%
  fill(region, .direction = "down") %>%
  mutate(island_group = case_when(
    region == "National Capital Region (NCR)" ~ "NCR",
    region %in% luzon ~ "Luzon",
    region %in% visayas ~ "Visayas",
    region %in% mindanao ~ "Mindanao"
  ))



#### match strings between the two data sets ####
# tf-idf 
# break up words into characters of different lengths,
# match with other strings and show top 3 matches 
# define model
string_match <- function(from, to) {
  tfModel <- POLYFUZZ$models$TFIDF(n_gram_range = list(4L, 6L), top_n = 1L, clean_string = TRUE)
  
  # # from vector - words from IO table we want to match
  # from <- unique(df_pov$province)
  # # to vector - words to crosswalk we want to match to 
  # to <- unique(ph$NAME_1)
  
  # run model on different levels of NAH 
  # with 3 digit to-vector
  # make df tidy
  tf_results <- POLYFUZZ$PolyFuzz(tfModel)$match(from, to)$get_matches() %>%
    rename(To_1 = "To", 
           Similarity_1 = "Similarity") %>%
    pivot_longer(
      cols = !From, 
      names_to = c(".value", "rank"),
      names_sep = "_"
    ) %>%
    rename(province = From,
           NAME_1 = To) %>%
    filter(!is.na(NAME_1)) 
  
  return(tf_results)
}

# get results from string matching
pov_incidence_string <- string_match(from = unique(df_pov$province),
                                     to = unique(ph$NAME_1))

# join with string matched province names 
df_pov1 <- df_pov %>%
  left_join(pov_incidence_string) %>%
  select(-rank, -Similarity) %>%
  mutate(NAME_1 = case_when(
    region == "National Capital Region (NCR)" ~ "Metropolitan Manila",
    province == "Davao de Oro" ~ "Davao de Oro",
    str_detect(province, "District") ~ NA, 
    TRUE ~ NAME_1), 
    across(starts_with("poverty"), ~as.numeric(.))) %>%
  filter(province_dummy == 1 | NAME_1 == "Metropolitan Manila") %>%
  select(NAME_1, province, island_group, starts_with("incidence"), starts_with("poverty")) %>%
  filter(!is.na(NAME_1), province %notin% c("Cotabato City", "Isabela City")) 
# isabela city, cotabato city are in the PH data, but not in shapefile data
# drop from data

# merge with shapefile
df_pov_ph <- ph %>%
  mutate(in_ph = 1) %>%
  mutate(NAME_1 = case_when(
    NAME_1 == "Compostela Valley" ~ "Davao de Oro",
    TRUE ~ NAME_1)) %>%
  full_join(df_pov1) %>%
  mutate(qt = case_when(
    incidence_pctpoint_2015_2021 <= -40 ~ "-40",
    incidence_pctpoint_2015_2021 > -40 & incidence_pctpoint_2015_2021 <= -30 ~ "-30",
    incidence_pctpoint_2015_2021 > -30 & incidence_pctpoint_2015_2021 <= -20 ~ "-20",
    incidence_pctpoint_2015_2021 > -20 & incidence_pctpoint_2015_2021 <= -10 ~ "-10",
    incidence_pctpoint_2015_2021 > -10 & incidence_pctpoint_2015_2021 <= 0 ~ "0",
    incidence_pctpoint_2015_2021 > 0 & incidence_pctpoint_2015_2021 <= 10 ~ "10",
    incidence_pctpoint_2015_2021 > 10 ~ "20"
  ))

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

# okabe-ito
# palette.colors(palette = "Okabe-Ito")
cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

highlights <- c("Luzon"="#56B4E9",
                "NCR" = "#E69F00",
                "Visayas" = "#009E73",
                "Mindanao" = "#CC79A7")

# prepare island group df
df_island_group <- df_pov_ph %>%
  group_by(island_group) %>%
  summarise()

##### map by major island group #####
# label NCR in yellow 
# plot - geography
f2pa <- ggplot(data = df_island_group) + 
  th +
  geom_sf(aes(fill = island_group),
          color="#FFFFFF",
          linewidth = 0.1) +
  # geom_sf(data = places,
  #         aes(fill = NAME),
  #         color="#E69F00",
  #         shape=18) +
  scale_fill_manual(values = highlights) +
  coord_sf(datum = NA,
           xlim = c(-200000, 1000000),
           ylim = c(600000, 2300000),
           expand = F) + # adjust size of plot?
  theme(legend.position = "none",
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  annotate(geom = "text", 
           x = c(200000, 280000, 600000, 60000), 
           y = c(800000, 1100000, 1800000, 1570000), 
           label = c("Mindanao", "Visayas", "Luzon", "NCR"),
           colour = c("#CC79A7", "#009E73", "#56B4E9","#E69F00"),
           family = "Noto Sans",
           fontface = "bold",
           size = 6/.pt)

f2pa

##### plot 2 - poverty by major island group #####
df_pov_f2 <- paste(dataraw,
                   "1E3DF130.csv",
                   sep = "/") %>%
  read_delim(delim=";",
             skip = 2) %>%
  clean_names() %>%
  rename(incidence_families = poverty_incidence_among_families_percent,
         incidence_popn = poverty_incidence_among_population_percent) %>%
  mutate(major_island_group = str_replace(major_island_group, "\\*", ""),
         year = as.numeric(str_replace(year, "p", ""))) %>%
  filter(major_island_group != "Luzon") %>%
  mutate(major_island_group = case_when(
    major_island_group == "Rest of Luzon" ~ "Luzon*",
    major_island_group == "PHILIPPINES" ~ str_to_title(major_island_group),
    T ~ major_island_group 
  ))


highlights_line <- c("Luzon*"="#56B4E9",
                     "NCR" = "#E69F00",
                     "Visayas" = "#009E73",
                     "Mindanao" = "#CC79A7",
                     "Philippines" = "#999999")

# line plot - overtime, major island groups, geography
f2pb <- ggplot(data = df_pov_f2,
               aes(x = year, 
                   y = incidence_popn,
                   color = major_island_group, 
                   group = major_island_group)) +
  th +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = highlights_line) +
  scale_x_continuous(position = "bottom", 
                     breaks = c(2015, 2018, 2021),
                     limits = c(2015, 2021.5),
                     expand = c(0,.01)) +
  scale_y_continuous(position = "right",
                     breaks = c(0, 10, 20, 30, 40),
                     limits = c(0, 40),
                     expand = c(0, 0)) +
  theme(legend.position = "none",
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  annotate(geom = "text",
           x = c(2015.8),
           y = c(19),
           label = c("Philippines"),
           colour = c("#999999"),
           family = "Noto Sans",
           fontface = "bold",
           size = 8/.pt) +
  # annotate(geom = "text",
  #          x = c(2017, 2017),
  #          y = c(37, 35.5),
  #          label = c("Mindanao", "declined\npoverty rates\nover time"),
  #          hjust = 0,
  #          lineheight = .7,
  #          colour = c("#CC79A7", "#999999"),
  #          family = "Noto Sans",
  #          fontface = "bold",
  #          size = 8/.pt) +
  # annotate(geom = "text", 
  #          x = c(2015.75, 2016.3, 2015.8, 2015.55, 2015.38), 
  #          y = c(34.5, 29, 19, 14.5, 5), 
  #          label = c("Mindanao", "Visayas", "Philippines", "Luzon*", "NCR"),
  #          colour = c("#CC79A7", "#009E73", "#999999", "#56B4E9","#E69F00"),
  #          family = "Noto Sans",
  #          fontface = "bold",
  #          size = 8/.pt) +
  annotate(
    "text",
    x = 2020.8,
    y = 37,
    label = "Decreased\npoverty\nrate",
    hjust = 1,
    lineheight = .7,
    family = "Noto Sans",
    fontface = 'bold', 
    size = 6/.pt,
    colour = "#999999"
  ) +
  annotate(
    "segment",
    x = 2020.9, # Play around with the coordinates until you're satisfied
    y = 38.5,
    yend = 33,
    xend =  2020.9,
    linewidth = .5,
    arrow = arrow(length = unit(5, 'pt')),
    colour = "#999999"
  )

f2pb

# need an arrow and  text 
f2 <- f2pa + f2pb +
  plot_annotation(
    title = "Philippines",
    subtitle = "Average poverty rate by region, 2015-2021 %",
    caption = "NCR = National Capital Region <br> **Source:** Philippine Statistics Authority • **Visual:** Jan Oledan",
    theme = th)

f2
# 
# ggsave(
#   filename = "test1.png",
#   bg = "#FFFFFF",
#   plot = f2,
#   device = "png",
#   path = plots,
#   scale = 1,
#   width = 6,
#   height = 3.5,
#   units = "in",
#   dpi = 300
# )

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

##### look at incidence by region, urban v rural #####

df_urban <- paste(dataraw,
                  "1E3DB005.csv",
                  sep = "/") %>%
  read_delim(delim = ";",
             skip = 2,
             locale = locale(encoding = "windows-1252")) %>%
  clean_names() %>%
  rename_with(.fn = ~str_replace(., "estimate_percent", "urban_pov"),
              .cols = starts_with("estimate")) %>%
  mutate(geolocation = str_replace_all(geolocation, "[\\.\\?]|[¹²]", "") %>%
           str_trim(),
         across(starts_with("urban"), ~as.numeric(.)))

df_rural <- paste(dataraw,
                  "1E3DB006.csv",
                  sep = "/") %>%
  read_delim(delim = ";",
             skip = 2,
             locale = locale(encoding = "windows-1252")) %>%
  clean_names() %>%
  rename_with(.fn = ~str_replace(., "estimate_percent", "rural_pov"),
              .cols = starts_with("estimate")) %>%
  mutate(geolocation = str_replace_all(geolocation, "[\\.\\?]|[¹²]", "") %>%
           str_trim(),
         across(starts_with("rural"), ~as.numeric(.)))

# regions ordered
region_factored <- c("PHILIPPINES", "NCR", "CAR",
                     "Region I", "Region II",
                     "Region III", "Region IV-A",
                     "MIMAROPA", "Region V",
                     "Region VI", "Region VII",
                     "Region VIII", "Region IX",
                     "Region X", "Region XI",
                     "Region XII",
                     "Caraga",
                     "BARMM*")

df_urb_rur <- df_urban %>%
  left_join(df_rural) %>%
  mutate(geolocation = case_when(
    geolocation == "Region XII excluding Cotabato City" ~ "Region XII",
    geolocation == "BARMM including Cotabato City" ~ "BARMM*",
    T ~ geolocation
  )) %>%
  #select(geolocation) %>%
  mutate(rural_pov_2021 = replace_na(rural_pov_2021, 0),
         diff = rural_pov_2021-urban_pov_2021,
         geolocation = fct_relevel(geolocation, region_factored))


cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### dumbbell plot ####
f4 <- ggplot(data = df_urb_rur) +
  geom_point(aes(x = urban_pov_2021,
                 y = geolocation), 
             colour = "#0072B2",
             alpha = 0.7,
             size = 3) + 
  geom_point(aes(x = rural_pov_2021,
                 y = geolocation), 
             colour = "#E69F00",
             alpha = 0.7,
             size = 3) + 
  geom_segment(
    aes(x = urban_pov_2021, xend = rural_pov_2021, y = geolocation, yend = geolocation),
    col = 'grey60',
    linewidth = 1
  ) + 
  th +
  theme(axis.text.y = element_text(size = 8,
                                   colour = "#000000",
                                   vjust = .5,
                                   hjust = 0)) +
  scale_y_discrete(limits = rev(region_factored)) +
  labs(title = "Urban/rural poverty",
       subtitle = "Poverty rate in urban and rural areas, 2021",
       caption = "**Source:** Philippine Statistics Authority • **Visual:** Jan Oledan") +
  theme(plot.title = element_text(hjust = -0.22),
        plot.subtitle = element_text(hjust= -0.33))



f4


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
