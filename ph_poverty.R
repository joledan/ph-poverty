# title: ph_poverty
# date: january 2024
# by: jan oledan
# desc: load data to construct figures for PH poverty write up

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
        axis.text.x = element_text(size = 8, # make axis text (labels) size 8
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
        axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = -11, unit = "pt"))) # adjust tick length to go on top of line


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


##### data for plot 1 - island group level poverty data #####
#### map and line chart ####
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
         geolocation = str_replace_all(geolocation, c("[a-z]\\/" = "" ,"[0-9]\\/" = "" ,"[\\.\\,]" = "")) %>%
                                         str_trim(),
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
  filter(province_dummy == 1 | NAME_1 == "Metropolitan Manila",
         !is.na(NAME_1), province %notin% c("Cotabato City", "Isabela City")) %>%
  select(NAME_1, province, island_group, starts_with("incidence"), starts_with("poverty"))
# isabela city, cotabato city are in the PH data, but not in shapefile data
# drop from data

# merge with shapefile
df_pov_ph <- ph %>%
  mutate(in_ph = 1, # adjust naming to merge properly
         NAME_1 = case_when(
           NAME_1 == "Compostela Valley" ~ "Davao de Oro",
           TRUE ~ NAME_1)) %>%
  full_join(df_pov1)

# remove from environment to clear space
rm(df_pov, df_pov1, ph, places, pov_incidence_string)

# prepare island group df for plot
df_island_group <- df_pov_ph %>%
  group_by(island_group) %>%
  summarise()

# prepare line chart data
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
  filter(major_island_group != "Luzon") %>% # rename Rest of Luzon as Luzon
  mutate(major_island_group = case_when(
    major_island_group == "Rest of Luzon" ~ "Luzon",
    major_island_group == "PHILIPPINES" ~ str_to_title(major_island_group),
    T ~ major_island_group 
  ))


##### plot 1 - map and line chart #####
# define colours
highlights <- c("Luzon"="#56B4E9",
                "NCR" = "#E69F00",
                "Visayas" = "#009E73",
                "Mindanao" = "#CC79A7")

# label NCR in yellow 
# plot - geography
f2pa <- ggplot(data = df_island_group) + 
  th +
  geom_sf(aes(fill = island_group),
          color="#FFFFFF",
          linewidth = 0.1) +
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


# highlights for line plot
highlights_line <- c("Luzon"="#56B4E9",
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

# view individual plots if you want
#f2pa
#f2pb

# need an arrow and  text 
f1 <- f2pa + f2pb +
  plot_annotation(
    title = "Philippines",
    subtitle = "Average poverty rate by region, 2015-2021 %",
    caption = "NCR = National Capital Region. Luzon values exclude the NCR. Poverty rate in percentage points. <br> **Source:** Philippine Statistics Authority • **Visual:** Jan Oledan",
    theme = th)

# write out plot
f1

##### data for plot 2 - regional poverty, urban v rural ####
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
region_factored <- c("NCR", "CAR",
                     "Region I", "Region II",
                     "Region III", "Region IV-A",
                     "MIMAROPA", "Region V",
                     "Region VI", "Region VII",
                     "Region VIII", "Region IX",
                     "Region X", "Region XI",
                     "Region XII",
                     "CARAGA",
                     "BARMM")

# read poverty incidence (%) and subsistence incidence files
# define island groups
luzon <- c("CAR", "Region I", "Region II", "Region III", "Region IV-A", "MIMAROPA", "Region V")
visayas <- c("Region VI", "Region VII", "Region VIII")
mindanao <- c("Region IX", "Region X", "Region XI", "Region XII", "CARAGA", "BARMM")

# prepare data for urban/rural plot
df_urb_rur <- df_urban %>%
  left_join(df_rural) %>%
  mutate(geolocation = case_when(
    geolocation == "Region XII excluding Cotabato City" ~ "Region XII",
    geolocation == "BARMM including Cotabato City" ~ "BARMM",
    geolocation == "Caraga" ~ "CARAGA",
    geolocation == "PHILIPPINES" ~ "Philippines",
    T ~ geolocation
  )) %>%
  #select(geolocation) %>%
  mutate(island_group = case_when(
    geolocation %in% luzon ~ "Luzon",
    geolocation %in% visayas ~"Visayas",
    geolocation %in% mindanao ~ "Mindanao",
    geolocation == "Philippines" ~ "Philippines",
    geolocation == "NCR" ~ "Luzon"),
  geolocation = fct_relevel(geolocation, rev(region_factored)),
  island_group = fct_relevel(island_group, rev(c("Mindanao", "Visayas", "Luzon")))) %>%
  filter(geolocation %notin% c("Philippines"))


##### plot 2 - regional urban, rural poverty, dumbbell plot #####
f2 <- ggplot(data = df_urb_rur) +
  th +
  geom_segment(
    aes(x = urban_pov_2021,
        xend = rural_pov_2021,
        y = geolocation,
        yend = geolocation),
    col = 'grey60',
    linewidth = 1) +
  geom_point(aes(x = urban_pov_2021,
                 y = geolocation), 
             colour = "#0072B2",
             alpha = 1,
             size = 2) +
  geom_point(aes(x = rural_pov_2021,
                 y = geolocation),
             colour = "#D55E00",
             alpha = 1,
             size = 2) +
  scale_x_continuous(position = "top",
                     expand = c(0,0),
                     limits = c(0, 45),
                     breaks = seq(0, 40, 10)) +
  geom_vline(aes(xintercept = 11.6),
             linetype = "dashed",
             colour = "#0072B2") +
  geom_vline(aes(xintercept = 25.7),
             linetype = "dashed",
             colour = "#D55E00",) +
  theme(axis.text.y = element_text(size = 8,
                                   colour = "#000000",
                                   vjust = .5,
                                   hjust = 1)) +
  facet_grid(rows = vars(island_group),
             scales = "free_y",
             space = "free_y",
             switch = "y") + # Let the x axis vary across facets.
  theme(strip.text.y.left = element_text(angle = 0, 
                                         vjust = 1,
                                         hjust = 0,
                                         margin=margin(t=3,b=0,l=0,r=0)),
        strip.placement = "outside") +
  labs(title = "On the wrong side",
       subtitle = "Regional poverty rate in <span style='color: #0072B2'><b>urban</b></span> and <span style='color:#D55E00'><b>rural</b></span> areas, 2021 %",
       caption = "NCR has no rural areas. \\* indicates averages for the whole country. Poverty rates in percentage points. <br> **Source:** Philippine Statistics Authority • **Visual:** Jan Oledan") +
  theme(panel.spacing = unit(0,'pt'),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(),
        plot.title.position = "plot",
        panel.grid.major.x = element_line(colour = "grey90"), # remove major x lines
        axis.line.x = element_line(colour = "grey90"),
        axis.ticks.x = element_line(colour="grey90"),
        axis.ticks.length.x = unit(0, "pt"), # define tick length and colour for x-axis
        axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid'))

# make plot
f2

gg_stop_recording()

### end of code ###