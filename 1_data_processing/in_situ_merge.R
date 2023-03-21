#Combine all in-situ data (NLA and NWIS)
#Create one csv output to be the input into water_temp_insitu

#Written by Hannah Ferriby
#Updated: 2/16/2023

library(tidyverse)
library(sf)
library(zoo)
library(lubridate)
library(arrow)

#Load in lakes and lake morpho
lakes <- st_read("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp") %>% 
  mutate(COMID = as.numeric(COMID))

lakes_morpho <- read_feather("data/conus_lake_morpho.feather") 

#Read in elevation data and manually add elevation for lake 13054044
elev <- read_feather('data/Elevation.feather') %>% select(COMID, ElevWs)
elev %>% add_row(COMID = 13054044, ElevWs = 188) -> elev

#Only include elevation for lakes that we're interested in
elev_conus <- elev[elev$COMID %in% lakes$COMID, ]

weeks <- read_feather("data/week_assignments.feather") 

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



#Read in all prism data, this includes both current day air temp and 30 day previous air
#temp mean
all_prism <- read_feather("data/30_day_atemp.feather") %>% mutate(COMID = as.numeric(COMID))


nla_07 <- read_feather("data/NLA/NLA_2007_sample_dates_locations.feather") %>% 
  mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y")) %>% select(COMID, DATE_COL, TEMPERATURE, DEPTH) %>%
  rename(date = DATE_COL) %>%
  mutate(source = "NLA")

nla_12 <- read_feather("data/NLA/NLA_2012_sample_dates_locations.feather") %>%
  mutate(DATE_COL = as.Date(DATE_COL, "%d-%b-%y")) %>% select(COMID, DATE_COL, TEMPERATURE, DEPTH) %>%
  rename(date = DATE_COL) %>%
  mutate(source = "NLA")

nla_17 <- read_feather("data/NLA/NLA_2017_sample_dates_locations.feather") %>%
  mutate(DATE_COL = as.Date(DATE_COL, "%d-%b-%y")) %>% select(COMID, DATE_COL, TEMPERATURE, DEPTH) %>%
  rename(date = DATE_COL) %>%
  mutate(source = "NLA")

nwis <- read_feather("data/WQP/NWIS_sample_dates_locations.feather") %>% select(COMID, StartDate, Temp, Depth) %>%
  rename(date = StartDate, TEMPERATURE = Temp, DEPTH = Depth) %>% mutate(date = as.Date(date, "%Y-%m-%d"),
                                                                         source = "NWIS") %>%
  filter(TEMPERATURE < 45)

in_situ <- rbind(nla_07, nla_12, nla_17, nwis)

all_data <- in_situ %>% left_join(lakes_data, by = "COMID") %>%
  left_join(all_prism, by = c("COMID", "date"))  %>% 
  mutate(day_of_year = yday(date)) %>% na.omit()

#Split into training, validation, and 2022 prediction
set.seed(42)

#This data set will be used in both the ARD and insitu RF models for prediction
prediction <- all_prism %>% filter(date > '2021-12-31') %>%
  left_join(lakes_data) %>%
  select(COMID, date, daily_atemp, mean_30day, lake_sa, lake_shoreline, LAT, LONG, ElevWs) %>%
  mutate(day_of_year = yday(date))

write_feather(prediction, "data/prediction_2022.feather", compression = 'zstd', compression_level = 22)

#Limit the number of samples per lake to 250 max
comid_counts <- all_data %>% count(COMID)
summary(comid_counts)

lt_250 <- all_data %>% filter(!COMID %in% comid_counts$COMID[which(comid_counts$n > 250)])

in_situ_lt250 <- all_data %>% filter(COMID %in% comid_counts$COMID[which(comid_counts$n > 250)]) %>%
  group_by(COMID) %>%
  sample_n(250) %>%
  ungroup() %>% rbind(lt_250)

#Check that the limit worked
comid_counts_post <- in_situ_lt250 %>% count(COMID)
summary(comid_counts_post)

#Split data into 80/20
sample_data <- sample(c(TRUE, FALSE), nrow(in_situ_lt250), replace=TRUE, prob=c(0.8,0.2))

in_situ_lt250 %>% mutate(row = row_number(), 
                   subset = ifelse(row %in% which(sample_data == T), 'Training', 'Validation')) %>%
  select(COMID, date, TEMPERATURE, lake_sa, lake_shoreline, LONG, LAT, ElevWs, daily_atemp,
         mean_30day, day_of_year, subset) -> in_situ_split

#Export for use in RF model
write_feather(in_situ_split, "data/all_insitu_2007_2022.feather", compression = 'zstd', compression_level = 22)



