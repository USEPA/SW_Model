library(tidyverse)
library(sf)
library(lubridate)
library(ggplot2)
library(arrow)

nla_2012 <- st_read("data/NLA/nla_2012_lakes.shp") %>% select(NLA12_ID, NLA12_DSN, NLA07_ID, X_ALBERS, Y_ALBERS, COMID, NLA_NAME)

nla_2007 <- st_read("data/NLA/nla_2007_lakes.shp") %>% rename(NLA07_ID = SITEID) #%>% st_transform(albers)

nla_2017 <- st_read('data/NLA/NLA_2017_Lake_Polygon.shp')

lakes <- st_read("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp")
lake_comid <- lakes %>% select(COMID)


#Find the unique 2012 lakes
# dupl_2012 <- which(is.na(nla_2012$NLA07_ID))
# number_of_lakes <- length(c(nla_2007$NLA07_ID, nla_2012$NLA12_ID[dupl_2012]))
# number_of_lakes
# number_of_repeated_in_2012 <- length(which(!is.na(nla_2012$NLA07_ID)))
# number_of_repeated_in_2012

profile_2007 <- read_feather("data/NLA/nla2007_profile_20091008.feather") %>% filter(SITE_ID %in% nla_2007$NLA07_ID) %>%
  select(SITE_ID, YEAR, DATE_PROFILE, DEPTH, TEMP_FIELD)
sample_2007 <- read_feather("data/NLA/nla2007_sampledlakeinformation_20091113.feather") %>% filter(SITE_ID %in% nla_2007$NLA07_ID) %>%
  select(SITE_ID, VISIT_NO, SAMPLED, DATE_COL, REPEAT, ALBERS_X, ALBERS_Y, LON_DD, LAT_DD, LAKENAME, COM_ID)

profile_2012 <- read_feather("data/NLA/nla2012_wide_profile_08232016.feather") %>% filter(SITE_ID %in% nla_2012$NLA12_ID) %>%
  select(SITE_ID, DATE_COL, DEPTH, TEMPERATURE)
sample_2012 <- read_feather("data/NLA/nla2012_wide_siteinfo_08232016.feather") %>% filter(SITE_ID %in% nla_2012$NLA12_ID) %>%
  select(SITE_ID, VISIT_NO, COMID2007, COMID2012, LON_DD83, LAT_DD83, EVAL_NAME, SITEID_07)

profile_2017 <- read_feather('data/NLA/nla_2017_profile-data.feather') %>% filter(SITE_ID %in% nla_2017$SITE_ID) %>%
  select(SITE_ID, DATE_COL, VISIT_NO, DEPTH, TEMPERATURE)
sample_2017 <- read_feather('data/NLA/nla_2017_site_information-data.feather') %>% filter(SITE_ID %in% nla_2017$SITE_ID) %>%
  select(SITE_ID, VISIT_NO, COMID, LAT_DD83, LON_DD83, XCOORD, YCOORD)

all_2007 <- profile_2007 %>% left_join(sample_2007, by = "SITE_ID") %>% filter(DEPTH <= 2.0)
all_2012 <- profile_2012 %>% left_join(sample_2012, by = "SITE_ID") %>% filter(DEPTH <= 2.0)
all_2017 <- profile_2017 %>% left_join(sample_2017, by = "SITE_ID") %>% filter(DEPTH <= 2.0)

sample_dates <- as.Date(c(all_2007$DATE_COL, all_2012$DATE_COL, all_2017$DATE_COL), "%Y/%m/%d")

all_nla_data <- all_2007 %>% full_join(all_2012, by = "SITE_ID") %>% full_join(all_2017) %>%
  mutate(DATE_COL = sample_dates, day = yday(DATE_COL), COMID = na.omit(c(COM_ID, COMID2012, COMID))) %>% filter(COMID %in% lakes$COMID)

for_2007_feather <- all_2007 %>% select(COM_ID, DATE_COL, DEPTH, TEMP_FIELD, ALBERS_X, ALBERS_Y, LON_DD, LAT_DD) %>% 
  rename(COMID = COM_ID, 
         TEMPERATURE = TEMP_FIELD,
         LON_DD83 = LON_DD,
         LAT_DD83 = LAT_DD) %>% filter(COMID %in% lakes$COMID)
for_2012_feather <- all_2012 %>% select(COMID2012, DATE_COL, TEMPERATURE, DEPTH, LON_DD83, LAT_DD83) %>% 
  rename(COMID = COMID2012) %>% filter(COMID %in% lakes$COMID)
for_2017_feather <- all_2017 %>% select(COMID, DATE_COL, TEMPERATURE, DEPTH, XCOORD, YCOORD, LON_DD83, LAT_DD83) %>%
  filter(COMID %in% lakes$COMID)

write_feather(for_2007_feather, 'data/NLA/NLA_2007_sample_dates_locations.feather', compression = 'zstd', compression_level = 22)
write_feather(for_2012_feather, 'data/NLA/NLA_2012_sample_dates_locations.feather', compression = 'zstd', compression_level = 22)
write_feather(for_2017_feather, 'data/NLA/NLA_2017_sample_dates_locations.feather', compression = 'zstd', compression_level = 22)

#Histogram of days samples were taken
hist(all_nla_data$day)


#Find which OLCI lakes are in the 2007 data
nla_lakes_2007 <- all_2007 %>% filter(COM_ID %in% lakes$COMID) 
length(unique(nla_lakes_2007$COM_ID))

nla_lakes_2012 <- all_2012 %>% filter(COMID2012 %in% lakes$COMID) 
length(unique(nla_lakes_2012$COMID2012))

nla_lakes_2007in2012 <- all_2012 %>% filter(COMID2007 %in% lakes$COMID) 
length(unique(nla_lakes_2007in2012$COMID2007))

nla_lakes_2017 <- all_2017 %>% filter(COMID %in% lakes$COMID)
length(unique(nla_lakes_2017$COMID))

all_comids <- unique(c(nla_lakes_2007$COM_ID, nla_lakes_2012$COMID2012, nla_lakes_2017$COMID))
length(all_comids)

