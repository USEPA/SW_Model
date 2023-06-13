# Get cloud cover data
# Author: Natalie Reynolds
# Date: 3.6.23

# Set up ----

# Clear workspace
rm(list = ls(all = T))

# Libraries
library(tidyverse)
library(lubridate)
# library(terra)
# library(tidyterra)
library(arrow)

# temp_dir <- paste0('./temp_files')
# temp_create <- ifelse(!dir.exists(temp_dir),
#                       dir.create(file.path(temp_dir),recursive = T),
#                       F)
# terraOptions(tempdir = temp_dir)

# Read in data ----
# Scene metadata
all_scenes <- read_feather('data/landsat_ard_all_scenes_2016_2022.feather')

# extracted_data <- read_feather('data/ard_points_nla_nwis.feather') 
ard_points_nla_nwis <- read_csv('data/ard_points_nla_nwis.csv')

# conus_lakes <- vect("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp")

all_scenes %>%
  dplyr::select(`Tile Identifier`, `Acquisition Date`, `Cloud Cover`, `Cloud Shadow`, `Snow Ice`, `Fill (No Data)`) %>% 
  janitor::clean_names() %>% rename(date = acquisition_date) %>%
  mutate(image_match = str_extract(tile_identifier,'[:digit:]{6}_[:digit:]{8}')) %>% dplyr::select(-tile_identifier) %>%
  inner_join(extracted_data) %>% distinct() %>% filter(!is.na(extracted_temp)) -> export_data

write_csv(export_data, file = 'ard_insitu_matchup_metadata.csv')
