# Extract ARD/NWIS/NLA match-ups
# Author: Natalie Reynolds
# Date: 12-7-22

# To restart R: CTRL + SHIFT + fn + F10

# Set up
rm(list = ls(all = T))

# Libraries
library(tidyverse)
library(lubridate)
library(terra)
library(tidyterra)

temp_dir <- paste0('./temp_files')
temp_create <- ifelse(!dir.exists(temp_dir),
                      dir.create(file.path(temp_dir),recursive = T),
                      F)
terraOptions(tempdir = temp_dir)

# Read in data
conus_lakes <- vect("data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp")

## In situ data
nwis <- read_csv('data/NWIS_sample_dates_locations.csv') %>% 
  vect(geom = c('Long','Lat'),crs = 'epsg:4617') %>% project(crs(conus_lakes)) %>% mutate(tile_identifier = NA)
  
nla <- read_csv('data/NLA_sample_dates_locations.csv') %>% 
  vect(geom = c('LON_DD83','LAT_DD83'),crs = 'epsg:4617') %>% project(crs(conus_lakes)) %>% mutate(tile_identifier = NA)

## Tiles
ard_tiles <- vect('data/CONUS_C2_ARD_grid/conus_c2_ard_grid.shp') %>% project(crs(conus_lakes)) %>% 
  mutate(tile_identifier = str_c(ifelse(nchar(h) == 1, str_c('00',h),str_c('0',h)),
                                 ifelse(nchar(v) == 1, str_c('00',v),str_c('0',v))))

nwis_relate <- relate(nwis,ard_tiles,'within')

nla_relate <- relate(nla,ard_tiles,'within')

pb = txtProgressBar(min = 0,
                    max = nrow(nwis) + nrow(nla),
                    initial = 0,
                    style = 3)

for(i in 1:nrow(nwis)){
  nwis$tile_identifier[i] <- ard_tiles$tile_identifier[which(nwis_relate[i,] == T)]
  setTxtProgressBar(pb,i)
}

for(i in 1:nrow(nla)){
  nla$tile_identifier[i] <- ard_tiles$tile_identifier[which(nla_relate[i,] == T)]
  setTxtProgressBar(pb,nrow(nwis)+i)
}

## Correct dates for NWIS and NLA and create matching regex for ARD files: TTTTTT_YYYYMMDD
nwis$date <- read_csv('data/NWIS_sample_dates_locations.csv')$StartDate
nla$date <- dmy(nla$DATE_COL)

## Combine NWIS and NLA
nwis %>% dplyr::select(date,Depth,Temp,tile_identifier) %>% 
  rename(depth = Depth, insitu_temp = Temp) %>%
  mutate(data_set = 'NWIS') %>%
  rbind(nla %>% dplyr::select(date,DEPTH,TEMPERATURE,tile_identifier) %>%
          rename(depth = DEPTH, insitu_temp = TEMPERATURE) %>% mutate(data_set = 'NLA')) %>%
  mutate(image_match = str_c(tile_identifier,'_',str_replace_all(date,'-',''))) -> joined_data

## ARD file names
ard_info <- tibble(
  fn = list.files(path = 'data/conus_ard_data', full.names = T),
  image_match = str_extract(fn,'[:digit:]{6}_[:digit:]{8}')
) %>% filter(image_match %in% joined_data$image_match) %>% arrange(fn)

image_matches <- unique(ard_info$image_match)

## Filter joined data to points with same-date ARD images and create columns for extracted data
joined_data %>% filter(image_match %in% image_matches) %>% 
  mutate(extracted_temp = NA,
         extracted_qa = NA) -> joined_data

# Read in files and extract ----

pb = txtProgressBar(min = 0,
                    max = length(image_matches),
                    initial = 0,
                    style = 3)

for(i in 1:length(image_matches)){
  
  files <- filter(ard_info,image_match == image_matches[i])$fn
  
  points <- filter(joined_data, image_match == image_matches[i])
  
  rast(files) %>% project(crs(conus_lakes)) %>% extract(points) -> extracted
  
  joined_data[which(joined_data$image_match == image_matches[i])]$extracted_temp <- extracted[,3]
  
  joined_data[which(joined_data$image_match == image_matches[i])]$extracted_qa <- extracted[,2]
  
  setTxtProgressBar(pb,i)
  
}

# write_rds(wrap(joined_data),file = 'joined_data.RDS')

# Process extracted data ----

L7.clouds <- c(1, 5440, 442, 5504, 5506, 5696, 5760, 5896, 7440, 7568, 7696, 7824, 7960, 8088, 13664)
L8.clouds <- c(1, 21824, 21826, 21888, 21890, 22080, 22144, 22280, 23888, 23952, 24088, 
               24216, 24344, 24472, 30048, 54596, 54852, 55052, 56856, 56984, 57240)
clouds <- c(L7.clouds, L8.clouds)

joined_data %>% 
  filter(!is.na(extracted_qa), !is.na(extracted_temp)) %>%
  project('epsg:4617') %>%
  mutate(
    # ard_temp = ifelse(extracted_qa %in% clouds, NA, (0.00341802*extracted_temp + 149.0 - 273.15)),
    lat = as_tibble(geom(joined_data))$y,
    long = as_tibble(geom(joined_data))$x
    ) %>%
  # filter(!is.na(ard_temp)) %>%
  as.data.frame() %>% as_tibble() -> joined_data

write_csv(joined_data, 'data/ard_points_nla_nwis.csv')

##

# tibble(
#   scene_identifiers = c(nwis$image_match,nla$image_match)
# ) %>% write.table(file = str_c('ard_points_sample_scenes.txt'), sep = '',row.names = F,quote = F)


# Delete terra temp files
unlink(temp_dir, recursive = TRUE, force = TRUE) 
