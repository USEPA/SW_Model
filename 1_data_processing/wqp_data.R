#WQP NWIS Water temp data processing
#Combine the two datasets and identify which stations fall within an OLCI resolvable lake
#Inputs -- Download resultphyschem feather and station feather from WQP, lakes shapefile 
#Output -- feather of relevant water temp samples with location and lake information


#Written by Hannah Ferriby
#Date updated: 1/25/2023

library(tidyverse)
library(sf)
library(lubridate)
library(ggplot2)
library(arrow)

#Load in temperature sample information 
physChem <- read_feather("data/WQP/resultphyschem.feather")
#Select only relevant columns and rename them something better, convert depth to meters, select depth less than or equal 2m
temp <- physChem %>% select(MonitoringLocationIdentifier, OrganizationIdentifier, ActivityMediaSubdivisionName, 
                            ActivityStartDate, `ActivityStartTime/Time`, `ActivityStartTime/TimeZoneCode`,
                            `ActivityDepthHeightMeasure/MeasureUnitCode`, `ActivityDepthHeightMeasure/MeasureValue`,
                            ResultMeasureValue, `ResultMeasure/MeasureUnitCode`, ResultValueTypeName, ResultStatusIdentifier) %>%
  rename(ID = MonitoringLocationIdentifier, StateID = OrganizationIdentifier, StartDate = ActivityStartDate, StartTime = `ActivityStartTime/Time`,
         TimeZone = `ActivityStartTime/TimeZoneCode`, Depth = `ActivityDepthHeightMeasure/MeasureValue`,
         DepthUnit = `ActivityDepthHeightMeasure/MeasureUnitCode`, Temp = ResultMeasureValue, TempUnit = `ResultMeasure/MeasureUnitCode`) %>%
  mutate(Depth = ifelse(DepthUnit == "feet", Depth/3.281, Depth)) %>% filter(!is.na(DepthUnit)) %>% filter(Depth <= 2.0)

#Load in sample location information
station <- read_feather("data/WQP/station.feather")
#Select relevant columns, remove non-CONUS samples, rename columns somsething better
station_conus <- station %>% filter(OrganizationIdentifier != "USGS-AK") %>%
    filter(OrganizationIdentifier != "USGS-HI") %>% filter(OrganizationIdentifier != "USGS-PR") %>% 
  select(OrganizationIdentifier, MonitoringLocationIdentifier, MonitoringLocationName,
         MonitoringLocationTypeName, LatitudeMeasure,
         LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName,StateCode) %>%
  rename(ID = MonitoringLocationIdentifier, StateID = OrganizationIdentifier, Lat = LatitudeMeasure, Long = LongitudeMeasure, horzCordSys = HorizontalCoordinateReferenceSystemDatumName,
         state = StateCode)

#Join water temp & station information by their IDs
all_temp_data <- temp %>% left_join(station_conus, by = c('ID', 'StateID'))


#Load OLCI resolvable lakes
lakes <- st_read("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp")

#Convert the Lat/Long columns to their own data frame
loc_df <- data.frame(Long = station_conus$Long, Lat = station_conus$Lat) %>% na.omit()

#Convert data frame to geometry with same CRS as the OLCI lakes
loc_2 <- st_as_sf(loc_df, coords = c("Long", "Lat"), remove = F) %>% st_set_crs(4617) %>% #NAD83 EPSG
  st_transform(st_crs(lakes))

#Plot stations and lakes
ggplot() +
  theme_minimal() +
  geom_sf(data = lakes, fill = 'red',lwd = .25)+
  geom_sf(data = loc_2)

#Identify which stations fall into which lakes and vice versa
lakes_select <- lengths(st_intersects(lakes, loc_2))
station_select <- lengths(st_intersects(loc_2, lakes))

lakes_w_station <- lakes[which(lakes_select>0),]
stations_in_lakes <- station_conus[which(station_select>0),]

#Select data that was sampled in an OLCI lake
select_temp_data <- all_temp_data %>% filter(ID %in% stations_in_lakes$ID) %>% mutate(day = yday(StartDate)) %>%
  select(ID, StartDate, StartTime, TimeZone, Depth, Temp, Lat, Long)
final_loc <- st_as_sf(select_temp_data, coords = c("Long", "Lat"), remove = F) %>% st_set_crs(4617) %>% #NAD83 EPSG
  st_transform(st_crs(lakes))

#Plot final stations and lakes
ggplot() +
  theme_minimal() +
  geom_sf(data = lakes, fill = 'red',lwd = .25)+
  geom_sf(data = final_loc)

#Add lake COMID and shape information to sample information
temp_data_w_comid <- st_intersection(final_loc, lakes)

#Export final temp data with lake COMID as feather
write_feather(temp_data_w_comid, 'data/WQP/NWIS_sample_dates_locations.feather', compression = 'zstd', compression_level = 22)
