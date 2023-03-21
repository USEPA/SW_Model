library(readr)
library(arrow)
library(tidyverse)

elev <- read_csv('data/Elevation.csv')
write_feather(elev, 'data/Elevation.feather', compression = 'zstd', compression_level = 22)

cloud <- read_csv('data/cloud_mask_tibble.csv')
write_feather(cloud, 'data/cloud_mask_tibble.feather', compression = 'zstd', compression_level = 22)

wtemp <- read_csv('data/conus_daily_wtemp.csv')
write_feather(wtemp, 'data/conus_daily_wtemp.feather', compression = 'zstd', compression_level = 22)

morph <- read_csv('data/conus_lake_morpho.csv')
write_feather(morph, 'data/conus_lake_morpho.feather', compression = 'zstd', compression_level = 22)

pred <- read_csv('data/prediction_2022.csv')
write_feather(pred, 'data/prediction_2022.feather', compression = 'zstd', compression_level = 22)

week <- read_csv('data/week_assignments.csv')
write_feather(week, 'data/week_assignments.feather', compression = 'zstd', compression_level = 22)

ice <- read_csv('data/weekly_ice_tibble.csv')
write_feather(ice, 'data/weekly_ice_tibble.feather', compression = 'zstd', compression_level = 22)

nla07 <- read_csv('data/NLA/NLA_2007_sample_dates_locations.csv')
write_feather(nla07, 'data/NLA/NLA_2007_sample_dates_locations.feather', compression = 'zstd', compression_level = 22)

nla12 <- read_csv('data/NLA/NLA_2012_sample_dates_locations.csv')
write_feather(nla12, 'data/NLA/NLA_2012_sample_dates_locations.feather', compression = 'zstd', compression_level = 22)

location <- 'data/'
all_csv_files <- list.files(location) %>% str_subset(pattern = ".csv")

for(i in 1:length(all_csv_files)) {
  csv <- read_csv(paste0(location, all_csv_files[i]))
  write_feather(csv, paste0(location, str_sub(all_csv_files[i], end = -5), '.feather'), compression = 'zstd', compression_level = 22)
  
}
