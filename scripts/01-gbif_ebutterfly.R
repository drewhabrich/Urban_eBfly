## HEADER---------------------------
## Script name: gbif_ebutterfly
##
## Purpose of script: download and preprocess ebutterfly data from gbif api
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
# using ebutterfly data up to November 2023

## 1. packages to load ####
library(tidyverse)
library(rgbif)
library(terra)

### List of fields to search occurrence data from
### NOTE: gbif login info stored in renvironment
rgbif::occ_fields
### 1.1 CHECK WHAT DATASETS HAVE BEEN DOWNLOADED ALREADY ####
dl_list <- occ_download_list()[[2]]  
glimpse(dl_list)
### link: https://www.gbif.org/dataset/cf3bdc30-370c-48d3-8fff-b587a39d72d6
### ebutterfly dataset UUID: cf3bdc30-370c-48d3-8fff-b587a39d72d6

### occurrence counts for complete dataset (to date)
occ_count(datasetKey = "cf3bdc30-370c-48d3-8fff-b587a39d72d6")

## 2. dataset download prep ####
### NOTE: There are 3 formats that are downloadable (DWCA, SIMPLE_CSV, SPECIES_LIST)
## define predicates to access specific datasets and occurrences from GBIF
ebfly_dwc <-
  occ_download_prep(pred_and(
    pred("DATASET_KEY", "cf3bdc30-370c-48d3-8fff-b587a39d72d6"),
    pred_in("country", c("CA", "US"))
  ), format = "DWCA")
ebfly_csv <-
  occ_download_prep(pred_and(
    pred("DATASET_KEY", "cf3bdc30-370c-48d3-8fff-b587a39d72d6"),
    pred_in("country", c("CA", "US"))
  ), format = "SIMPLE_CSV")
ebfly_spl <-
  occ_download_prep(pred_and(
    pred("DATASET_KEY", "cf3bdc30-370c-48d3-8fff-b587a39d72d6"),
    pred_in("country", c("CA", "US"))
  ), format = "SPECIES_LIST")

ebfly_list <- list(ebfly_dwc, ebfly_csv, ebfly_spl)
### 2.1 occ_download_queue(.list = ebfly_list, status_ping = 10) ####
occ_download_meta(dl_list[[2,1]])

### 2.2 Import gbif data to R from the downloaded dataset; [[row, col]], NOTE THIS WILL DEPEND ON YOUR GBIF ACCOUNT ####
# download to local working directory first as .zip
eb_spl <- occ_download_get(dl_list[[2,1]], path = "./raw_data/", overwrite = F) #species list, 0053910
eb_csv <- occ_download_get(dl_list[[3,1]], path = "./raw_data/", overwrite = F) #simple csv, 0053909
eb_dwc <- occ_download_get(dl_list[[4,1]], path = "./raw_data/", overwrite = F) #darwinian core archive, 0053908
# import into R as a dataframe from .zip
df_spl <- occ_download_import(eb_spl)
df_csv <- occ_download_import(eb_csv)
df_dwc <- occ_download_import(key = dl_list[[4,1]], path = "./raw_data/")

## 3. EDA of dataframes (I think only the dwc has checklist data in RAW form) ####
glimpse(df_dwc)

## create a vector of empty columns and 1-entry columns
unique_entries <- df_dwc %>%
  summarise(across(where(~n_distinct(.) == 1), ~unique(.))) %>% 
  gather(key = "column_name", value = "unique_entry") %>% pull(column_name)

## bind both strings together into 1 and remove from the dataframe
df_tidy <- df_dwc %>% select(!all_of(unique_entries))

## remove redundant columns
glimpse(df_tidy)
df_tidy %>% summarise(across(everything(), n_distinct)) %>% 
  gather(key = "column_name", value = "n_distinct") 

df_min <- df_tidy %>% select(c("eventID", "eventDate", "samplingProtocol", "sampleSizeValue", "sampleSizeUnit",
                               "samplingEffort", "countryCode", "stateProvince", "recordedBy", "decimalLatitude", "decimalLongitude",
                               "coordinateUncertaintyInMeters","individualCount", "family","genus","species","vernacularName","iucnRedListCategory"))

## groupings
### How many unique checklists by country
df_min %>% group_by(countryCode) %>% n_distinct(.$eventID)
### How many COMPLETE checklists by country (after removing obviously incomplete)
df_min %>% filter(samplingProtocol != "Incidental Observation(s)") %>% 
  group_by(countryCode) %>%
  summarise(distinct_entries = n_distinct(eventID))

### filter to just COMPLETE checklists
df_c <- df_min %>% filter(!(samplingProtocol %in% c("Incidental Observation(s)","Historical",""))) %>%
                   mutate(complete_chkl = str_detect(samplingEffort, "Complete checklist"))
df_compl <- df_c %>% filter(complete_chkl == TRUE)

## Quick plot of checklist locations
df_compl %>% ggplot() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude)) +
  coord_map()

##check the sampling protocols
glimpse(df_compl)
df_c %>% filter(complete_chkl == TRUE) %>% group_by(samplingProtocol) %>% summarize(count = n())
df_c %>% filter(complete_chkl == TRUE) %>% group_by(countryCode) %>% summarize(n_chkl = n_distinct(eventID))

##save clean dataframe to csv for later
write_csv(df_compl, "./output/01-ebutterfly_completechecklist.csv")
