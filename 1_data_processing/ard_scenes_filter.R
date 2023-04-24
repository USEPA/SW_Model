#Filter ARD scenes list for cloud free images
library(tidyverse)
library(arrow)

ard_scenes <- read_feather('data/landsat_ard_all_scenes_2016_2022.feather')

ard_scenes_filter <- ard_scenes %>% select(`Tile Identifier`,
                                            `Acquisition Date`,
                                           `Tile Grid Horizontal`,
                                           `Tile Grid Vertical`,
                                           `Cloud Cover`,
                                           `Cloud Shadow`,
                                           `Snow Ice`) %>%
  rename(tile_name = `Tile Identifier`,
         date = `Acquisition Date`,
         grid_h = `Tile Grid Horizontal`,
         grid_v = `Tile Grid Vertical`,
         cloud = `Cloud Cover`,
         cloud_shadow = `Cloud Shadow`,
         snow_ice = `Snow Ice`) %>%
  filter(cloud == 0) %>% filter(cloud_shadow == 0)

write_feather(ard_scenes_filter, 'data/landsat_ard_scenes_no_clouds.feather', compression = 'zstd', compression_level = 22)
