# title: clean
# date: december 2023
# by: jan oledan
# desc: 

##### install and load packages #####
rm(list=ls())
# Package names
packages <- c("sf","readr","tidyr","dplyr","ggplot2",
              "stringr","magrittr", "ggrepel", "devtools",
              "sysfonts","extrafont", "readxl", "cowplot",
              "patchwork", "Cairo", "lubridate", "ragg",
              "camcorder", "devtools", "geodata", "reticulate",
              "tidyterra", "janitor", "rnaturalearth", "cartogram")
extrafont::loadfonts("win")
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

##### set up directories ######
main <- case_when(Sys.getenv("USERNAME") == "jgole" ~ "C:/Users/jgole/Dropbox/Portfolio",
                  Sys.getenv("USER") == "janoledan" ~ "/Users/janoledan/Dropbox/Portfolio")
dataout <- paste(main, "ph-quick/dataout", sep = "/")
dataraw <- paste(main, "ph-quick/dataraw", sep = "/")
plots <- paste(main, "ph-quick/plots", sep = "/")
code <- paste(main, "ph-quick/code", sep = "/")

# read admin1 ph data
ph <- as_sf(gadm(country = "PHL",
                 level = 1,
                 path = dataraw)) %>%
  st_transform(crs="EPSG:32651") %>%
  select(NAME_1, geometry)

str(ph)

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
  filter(!is.na(NAME_1), !(province %in% c("Cotabato City", "Isabela City"))) 
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
  
summary(df_pov_ph$incidence_pctpoint_2018_2021)
summary(df_pov_ph$incidence_pctpoint_2015_2021)


# plotting normalized index for 2019
# camcorder to set up and output plots
camcorder::gg_record(
  dir = plots,
  device = "png",
  scale = 2,
  width = 2,
  height = 3,
  units = "in",
  dpi = 300,
  bg = "white"
)


col <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d")

# plot - geography
f1 <- ggplot() + 
  theme_minimal(base_family = "Noto Sans") +
  geom_sf(data = df_pov_ph, 
          aes(fill = qt),
          color="#ABABAB",
          linewidth = 0.1) +
  scale_fill_manual(values = col,
                    breaks = c("-40", "-30", "-20", "-10", "0", "10", "20"),
                    labels = c("-40", "", "-20", "", "0", "", "+20")) +
  geom_sf(data = places, 
          aes(color = "black"),
          color = "black",
          shape = places$shape) +
  geom_sf_text_repel(data = places, 
                     aes(label = NAME, 
                         geometry = geometry,
                         family = "Noto Sans"), 
                     nudge_x = c(-200000), 
                     nudge_y = c(0),
                     segment.size = .5, 
                     size = 3) +
  coord_sf(datum = NA,
           xlim = c(-200000, 1000000),
           ylim = c(500000, 2400000)) + # adjust size of plot?
  theme(plot.title = element_text(face="bold",
                                  size=12,
                                  margin=margin(t=10, r=0, b=3, l=10, "pt")),
        plot.subtitle = element_text(size=10,
                                     margin=margin(t=0,r=0,b=0,l=0, "pt")),
        #plot.margin = unit(c(t=10,r=10,b=10,l=10), "pt"),
        plot.caption = element_text(hjust = 0,
                                    size = 6,
                                    color="grey",
                                    margin=margin(t=0,r=0,b=0,l=0, "pt")),
        plot.caption.position = "plot",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank()) +
  guides(color=guide_legend(nrow = 1),
         fill=guide_legend(nrow = 1,
                           label.position = "bottom",
                           label.hjust = 1)) +
  theme(legend.position = c(0.73,.94), # where legend goes in plot
        legend.margin= margin(t=0,r=0,b=0,l=0, "pt"), # adjust padding in legend
        legend.justification = "right",
        legend.direction = "horizontal", # horizontal legend
        legend.title = element_blank(), # no legend title
        legend.spacing.y= unit(1, "pt"), # modify keys in legend
        legend.spacing.x= unit(0, "pt"),
        legend.key.width = unit(25, "pt"),
        legend.key.height = unit(10, "pt"),
        legend.text = element_text(size = 8)) +  
  labs(title = "Poverty rates in the Philippines",
       subtitle = "Percentage point change, 2015-2021",
       caption = bquote(paste(bold("Source:")~"Philippine Statistics Authority •",
                              ~bold("By:")~"Jan Oledan",
                              sep="\n"))) +
  annotate("text", 
           x = 590000, 
           y = 2450000, 
           label = "Increased →",
           family = "Noto Sans",
           size = 8/3) +
  annotate("text", 
           x = -130000, 
           y = 2450000, 
           label = "← Decreased",
           family = "Noto Sans",
           size = 8/3)


f1


# okabe-ito
z <- palette.colors(palette = "Okabe-Ito")
cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

highlights <- c("Luzon"="#56B4E9",
                "NCR" = "#E69F00",
                "Visayas" = "#009E73",
                "Mindanao" = "#CC79A7")

df_island_group <- df_pov_ph %>%
  group_by(island_group) %>%
  summarise()

# plot - geography
f2pa <- ggplot(data = df_island_group) + 
  theme_minimal(base_family = "Noto Sans") +
  geom_sf(aes(fill = island_group),
          color="#FFFFFF",
          linewidth = 0.1) +
  scale_fill_manual(values = highlights) +
  coord_sf(datum = NA,
           xlim = c(-200000, 1000000),
           ylim = c(600000, 2300000)) + # adjust size of plot?
  theme(plot.title = element_text(face="bold",
                                  size=12,
                                  margin=margin(t=10, r=0, b=3, l=10, "pt")),
        plot.subtitle = element_text(size=10,
                                     margin=margin(t=0,r=0,b=0,l=0, "pt")),
        #plot.margin = unit(c(t=10,r=10,b=10,l=10), "pt"),
        plot.caption = element_text(hjust = 0,
                                    size = 6,
                                    color="grey",
                                    margin=margin(t=0,r=0,b=0,l=0, "pt")),
        plot.caption.position = "plot",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") # specify colour scheme


##### by major island group
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
f2pb <- ggplot(data = df_pov_f2) +
  theme_minimal(base_family = "Noto Sans") +
  geom_line(aes(x = year, 
                y = incidence_popn,
                color = major_island_group, 
                group = major_island_group),
            size = 1) +
  scale_color_manual(values = highlights_line) +
  scale_x_continuous(position = "bottom",   # move the x axis labels up top
                     breaks = c(2015, 2018, 2021),
                     limits = c(2015, 2021),
                     expand = c(0,.01)) +
  scale_y_continuous(position = "right") +   # move the x axis labels up top
  theme(panel.border = element_blank(),
        axis.title.x = element_blank(), # adjust x axis title
        axis.title.y = element_blank(), # adjust y axis title
        axis.ticks.x = element_line(colour="black"),
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.line.x = element_line(), # adjust x axis line
        #axis.ticks.length.x = unit(.1, "cm"), # adjust tick length
        panel.grid.major.x = element_blank(), # remove major x lines
        panel.grid.minor.x = element_blank(), # remove minor x lines
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0, 10, 0, 10),
        legend.position = "none") +
  annotate(geom = "text", 
           x = c(2015.75, 2016, 2015.8, 2015.55, 2015.38), 
           y = c(35, 29, 19.5, 15, 4.6), 
           label = c("Mindanao", "Visayas", "Philippines", "Luzon*", "NCR"),
           colour = c("#CC79A7", "#009E73", "#999999", "#56B4E9","#E69F00"),
           family = "Noto Sans",
           fontface = "bold")


f2 <- f2pa + f2pb +
  plot_layout(widths = c(1, 1),
                heights = c(3, 2))
f2

