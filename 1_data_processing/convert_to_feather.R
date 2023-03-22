library(readr)
library(arrow)
library(tidyverse)

#Example of slow way
# elev <- read_csv('data/Elevation.csv')
# write_feather(elev, 'data/Elevation.feather', compression = 'zstd', compression_level = 22)

#Bulk way
location <- 'atmos_outputs/'
all_csv_files <- list.files(location) %>% str_subset(pattern = ".csv")

for(i in 1:length(all_csv_files)) {
  csv <- read_csv(paste0(location, all_csv_files[i]))
  write_feather(csv, paste0(location, str_sub(all_csv_files[i], end = -5), '.feather'), compression = 'zstd', compression_level = 22)
  
}
