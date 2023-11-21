## HEADER---------------------------
## Script name: landcover_chkl_mapping
##
## Purpose of script: prepare landcover and other landscape data and plot the ebutterfly checklist data
##
## Author: Andrew Habrich
##
## Date Created: 2023-11-21
## Date last Modified: 2023-11-21
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

## 1. Load relevant packages--------
### Get Landcover data from ESA
library(geodata)
library(mapview)
library(terra)
library(sf)
library(tidyverse)
library(ggspatial)

### Download geodata for visualizing
#geodata::landcover(var = "built", path = "./raw_data/")
#geodata::landcover(var = "trees", path = "./raw_data/")
# can <- osm("CAN", "places", "./output/")
# plot(can)

## 2. ebutterfly import ebutterfly data ----
### import and convert the geometry column into point coordinates for spatial feature management
urb_chkls <- read_csv("./output/02-ebfly_cancities_chkl.csv") %>%
  mutate(geometry = str_remove_all(geometry, "[c\\(\\)]")) %>%
  separate(geometry,
           into = c("x", "y"),
           sep = ",",
           convert = TRUE) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
glimpse(urb_chkls)
### create a subset of just 1 city to visualize
# how many unique cities are there
urb_chkls %>% 
  group_by(PCNAME.x) %>%
  summarize(uniquechkl = n_distinct(eventID)) %>%
  arrange(desc(uniquechkl))

# subset the most abundant one
city <- urb_chkls %>% 
  group_by(PCNAME.x) %>%
  summarize(uniquechkl = n_distinct(eventID)) %>%
  arrange(desc(uniquechkl)) %>% slice(1) %>% pull(PCNAME.x)

city_chkl <- urb_chkls %>% 
  filter(PCNAME.x == city) %>% 
  select(-c("countryCode", "stateProvince",
            "PCTYPE", "PCCLASS", "XPRuid", "PRUID", 
            "DGUID","DGUIDP","complete_chkl"))

# vis on map
xy <- st_coordinates(city_chkl) %>% as_tibble() ##need to convert to xy to get density map
fig_point_density <- city_chkl %>% ggplot() + 
  annotation_map_tile(zoom = 10) +
  geom_sf() + 
  geom_density2d_filled(data = xy, aes(X, Y), alpha = 0.35, binwidth = 5, contour_var = "density", show.legend = F) +
 # stat_density_2d_filled(data = xy, aes(X, Y), geom = "raster", aes(fill = after_stat(density)), contour = FALSE) +
  theme_bw() + labs(x = "Longitude", y = "Latitude")
ggsave(filename = "./output/figures/03-fig_point_density.png", plot = fig_point_density)

# what years have the most observations
fig_ottawa_chkls <- city_chkl %>% group_by(year) %>% summarize(count = n_distinct(eventID)) %>% 
  ggplot() + geom_bar(aes(x = year, y = count), stat = "identity") + 
  scale_x_continuous(name="Year", breaks=seq(2001, 2023, 1), guide = guide_axis(angle = 45)) +
  scale_y_continuous(name="Count", breaks=seq(0, 600, 50)) +
  ggtitle(label = "Unique checklists in Ottawa by year") + 
  theme_bw()
ggsave(filename = "./output/figures/03-fig_ottawa_chkls_by_year.png", plot = fig_ottawa_chkls)

### Plot interactively with leaflet or mapview
mapview(city_chkl, zcol = "year", col.regions = RColorBrewer::brewer.pal(9,name = "PRGn"), cex = 4)

#### RASTER LAYER MANAGEMENT ####
### check to what landcover data layers are already downloaded
list.files("./raw_data/", pattern = ".tif") #RASTER FILE IN .tif format

### Import raster into R
lc_b <- rast("./raw_data/WorldCover_built_30s.tif")
lc_t <- rast("./raw_data/WorldCover_trees_30s.tif")

### TO DO ####

###calc species richness estimates (see pivot_wider.sf), remember geometries are STICKY to data
###american census geography, check tidycensus package and 'urban areas' in get_acs()