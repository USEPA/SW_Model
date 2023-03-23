#Create ARD data frame for input into RF model

#Load libraries and set seed
library(tidyverse)
library(sf)
library(lubridate)
library(arrow)
set.seed(42)


#Load in lake shapefiles, lake morpho data, PRISM, ice presence, and ARD water temp data
lakes <- st_read("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp") %>% mutate(COMID = as.numeric(COMID))

lakes_morpho <- read_feather("data/conus_lake_morpho.feather") %>% mutate(COMID = as.numeric(COMID))

#Read in elevation data and manually add elevation for lake 13054044
elev <- read_feather('data/Elevation.feather') %>% select(COMID, ElevWs)
elev %>% add_row(COMID = 13054044, ElevWs = 188) -> elev

#Only include elevation for lakes that we're interested in
elev_conus <- elev[elev$COMID %in% lakes$COMID, ] 

weeks <- read_feather("data/week_assignments.feather")

#Read in all prism data, this includes both current day air temp and 30 day previous air
#temp mean
all_prism_data <- read_feather("data/30_day_atemp.feather") %>% filter(!is.na(mean_30day)) %>%
  filter(date > '2016-04-30') %>% mutate(COMID = as.numeric(COMID))

#Cloud and ice presence masks - rename masked in cloud to cloud_pres due to both mask csvs using
#the same masked name
ice <- read_feather("data/weekly_ice_tibble.feather")
cloud <- read_feather("data/cloud_mask_tibble.feather") %>% rename(cloud_pres = masked)

#Read in ARD surface temperature data, filter out NAs, and filter the starting point to match other datasets
# daily_water_temp <- read_feather("data/conus_daily_wtemp.feather") %>% #remove rows with no ARD water temp
#   filter(!is.na(temp_k)) %>% filter(date > "2016-04-30") 

daily_water_temp <- read_feather("data/conus_daily_wtemp_noclouds.feather") %>% #remove rows with no ARD water temp
  filter(!is.na(temp_k)) %>% filter(date > "2016-04-30") %>% mutate(COMID = as.numeric(COMID))

#Merge together lakes with lakes_morpho
lakes %>% 
  mutate(centroid = st_centroid(geometry),
         LONG = st_coordinates(centroid)[,1],
         LAT = st_coordinates(centroid)[,2]) %>%
  dplyr::select(COMID,LONG,LAT) %>% mutate(COMID = as.numeric(COMID))-> lakes_loc

lakes_data <- lakes_morpho %>% left_join(lakes_loc) %>%
  left_join(elev_conus)

#Convert lake shoreline from m to km and lake surface area from sq m to sq km
lakes_data %>% mutate(lake_shoreline = lake_shoreline/1000,
                      lake_sa = lake_sa/1000000) -> lakes_data

#Merge all data sets together and convert temperature to C
training <-  daily_water_temp %>% left_join(lakes_data, by = "COMID") %>%
  inner_join(all_prism_data)  %>% 
  inner_join(ice, by = c("COMID", "week", "year")) %>%
  inner_join(cloud, by = c("COMID", "date")) %>%
  mutate(day_of_year = yday(date)) %>% 
  #Filter out clouds, low temps, and clouds
  filter(masked == F & temp_k > 273 & cloud_pres == F) %>%
  select(COMID, date, temp_k, lake_shoreline, lake_sa, LONG, LAT, ElevWs, daily_atemp, mean_30day, masked,
         cloud_pres, day_of_year) %>%
  rename(TEMPERATURE = temp_k) %>% #Make same variable name as in situ temperature
  mutate(TEMPERATURE = TEMPERATURE - 273.15) #Put temperature in C

# write_csv(training, 'data/ard_training.csv')
write_feather(training, 'data/ard_training_no_clouds.feather', compression = 'zstd', compression_level = 22)
