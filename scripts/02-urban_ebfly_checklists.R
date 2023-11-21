## HEADER---------------------------
## Script name: urban ebfly checklists
##
## Purpose of script: Identify checklists in and around urban areas, removing entries in rural or non-urban landscapes.
##
## Author: Andrew Habrich
##
## Date Created: 2023-11-13
## Date last Modified: 2023-11-16
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

## 1. Load relevant packages--------
library(tidyverse)
library(terra)
library(sf)

##2. Import dataframe ####
#### Fix the quebec accent issue on import too
df_chkl <- read_csv("./output/01-ebutterfly_completechecklist.csv") %>% 
           mutate(stateProvince = str_replace(stateProvince, "Québec", "Quebec"))
glimpse(df_chkl)

## How many checklists are there by province/state
df_chkl %>% group_by(stateProvince, countryCode) %>% filter(complete_chkl == T) %>% 
  summarize(n_chkl = n_distinct(eventID)) %>% ungroup() %>% 
  arrange(n_chkl) %>% #sort by the number of checklists
  mutate(stateProvince = factor(stateProvince, levels = stateProvince)) %>% #coerce factor to the sorted levels
  ggplot() + 
    geom_bar(aes(y = stateProvince, x = n_chkl, fill = countryCode), stat = "identity") +
    theme_minimal() + labs(title = "Complete checklists by State/Province", fill = "Country", x = "# of checklists", y = "State/Province")

## remove places with too few observations to get a reasonable estimate
### (lets say <500 checklists for now)
obs_data_bycity <- df_chkl %>% group_by(stateProvince) %>% filter(complete_chkl == T) %>% 
  summarize(n_chkl = n_distinct(eventID)) %>% filter(n_chkl >= 500) %>% pull(stateProvince)

## create df with just complete checklists that have geographic coordinates and coerce to sf object
df_sf <- df_chkl %>% filter(stateProvince %in% obs_data_bycity) %>% 
  filter(!(is.na(decimalLatitude))) %>% 
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
glimpse(df_sf)

## 3. Identify urban areas of interest with the census-shapefile coordinating function ####
get_populationcentres <- function (shapefile, censusdat, crs, minpopsize) {
  popc <- st_read(paste0("./raw_data/", shapefile))
  # relational database csv with attributes for each polygon
  popctr <- read_csv(paste0("./raw_data/", censusdat), col_types = "dc") %>% as_tibble() 
  popctr <- popctr %>% select(DGUID = POPCTRRAdguid,
                              PCNAME = POPCTRRAname, 
                              pop2021 = POPCTRRApop_2021, 
                              urbarea = POPCTRRAarea, 
                              XPRuid = XPRuid) 
  ## Join polygons with attribute data, and filter to 'large' cities 
  map_proj <- st_crs(crs) #set crs 
  urb <- popc %>% left_join(popctr, by = "DGUID") %>% filter(pop2021 >= minpopsize) %>% st_transform(crs = map_proj)
}

#get urban areas with >50000 people and buffer them by 5km
urb_popcentres <- get_populationcentres("lpc_000b21a_e.shp", "POPCTR.csv", "EPSG:4326", 50000)
canurb_buff <- st_buffer(urb_popcentres, 5000)

## Join the checklist dataframe with the urban buffer dataframe based on points within polygons
urb_chkls <- st_join(df_sf, canurb_buff, join = st_within) %>% 
             select(!c(PCNAME.y)) %>% 
             filter(!(recordedBy == "lymanmuseum")) ###REMOVE THESE MUSEUM OBSERVATIONS, THEY ARE NOT COMPLETE CHECKLISTS 
glimpse(urb_chkls)

## 4. Visualize dataframe figures ####
## How many unique checklists per city in Canada are there?
urb_chkls %>% group_by(PCNAME.x, stateProvince) %>% filter(countryCode == "CA" & !is.na(PCNAME.x)) %>% 
  summarize(chkl_count = n_distinct(eventID)) %>% ungroup() %>% 
  filter(!(PCNAME.x == "Ottawa - Gatineau" & stateProvince == "Quebec")) %>% ## OTTAWA GATINEAU ISSUE with cross-jurisdiction
  arrange(chkl_count) %>% 
  mutate(cityname = factor(PCNAME.x, levels = PCNAME.x)) %>% 
  ggplot() + 
    geom_bar(aes(y = cityname, x = chkl_count, fill = stateProvince), stat = "identity") +
    geom_vline(xintercept = 100) + 
    labs(title = "Complete checklists by Canadian cities", fill = "Province", x = "# of checklists", y = "City") +
    theme_minimal()
    
## How many unique checklists per YEAR in canadian cities?
#### ALL YEARS
urb_chkls %>% filter(countryCode == "CA" & !is.na(PCNAME.x)) %>% 
  mutate(year = year(eventDate)) %>%
  group_by(year) %>% 
  summarize(chkl_count = n_distinct(eventID)) %>% 
  ggplot() + 
    geom_bar(aes(y = chkl_count, x = year), stat = "identity") +
    labs(title = "Complete checklists in Canadian cities by ALL YEARS", x = "Year", y = "# of checklists") + 
    theme_minimal()

#### JUST AFTER 2000
urb_chkls %>% filter(countryCode == "CA" & !is.na(PCNAME.x)) %>% 
  mutate(year = year(eventDate)) %>% 
  filter(year >= 2000) %>% 
  group_by(year) %>% 
  summarize(chkl_count = n_distinct(eventID)) %>% 
  ggplot() + 
  geom_bar(aes(y = chkl_count, x = year), stat = "identity") +
  labs(title = "Complete checklists in Canadian cities submitted after 2000", x = "Year", y = "# of checklists") + 
  theme_minimal()

### What are all the historical checklists and where are they from?
urb_chkls %>% filter(countryCode == "CA" & !is.na(PCNAME.x)) %>% group_by(stateProvince) %>% 
  mutate(year = year(eventDate)) %>% filter(year <= 2000) %>% 
  summarize(chkl_count = n_distinct(eventID))

urb_chkls %>% group_by(PCNAME.x, stateProvince) %>% filter(countryCode == "CA" & !is.na(PCNAME.x)) %>% 
  filter(!(recordedBy == "lymanmuseum")) %>% ###REMOVE THESE MUSEUM OBSERVATIONS, THEY ARE NOT COMPLETE CHECKLISTS 
  mutate(year = year(eventDate)) %>% filter(year >= 2000) %>% 
  summarize(chkl_count = n_distinct(eventID)) %>% ungroup() %>% 
  filter(!(PCNAME.x == "Ottawa - Gatineau" & stateProvince == "Quebec")) %>% ## OTTAWA GATINEAU ISSUE with cross-jurisdiction
  arrange(chkl_count) %>% 
  mutate(cityname = factor(PCNAME.x, levels = PCNAME.x)) %>% 
  ggplot() + 
  geom_bar(aes(y = cityname, x = chkl_count, fill = stateProvince), stat = "identity") +
  geom_vline(xintercept = 100) + 
  theme_minimal() + labs(title = "Complete checklists by Canadian cities", fill = "Province", x = "# of checklists", y = "City")

## 5. Save to file ####
can_urb_clean <- urb_chkls %>% 
  filter(countryCode == "CA" & !is.na(PCNAME.x)) %>% 
  filter(!(recordedBy == "lymanmuseum")) %>% ###REMOVE THESE MUSEUM OBSERVATIONS, THEY ARE NOT COMPLETE CHECKLISTS 
  mutate(year = year(eventDate)) %>% filter(year >= 2000) 

glimpse(can_urb_clean)
can_urb_clean %>% summarize(chkl_count = n_distinct(eventID))

write_csv(can_urb_clean, "./output/02-ebfly_cancities_chkl.csv")