#Takes in surface temperature and QA_Pixel images for Landsat ARD
#Uses the QA_Pixel image to mask out clouds in the surface temperature images
#Outputs a csv file with date, mean lake temperature (scaled and not scaled), and corresponding satellite
#for each COMID lake
#Go year by year due to computing limits

#Created by: Hannah Ferriby & Natalie Von Tress
#Date created: May 25, 2022
#Last Updated: November 4, 2022


# Clear work space
rm(list = ls())
ptm <- proc.time()
#Load libraries and set WD ----
#Load libraries and set WD ----
library(tidyverse)
library(lubridate)
library(terra)
library(sf)
library(exactextractr)
library(future)
library(future.apply)
library(arrow)

#Working directory location of all external data and contains a folder with QA and ST images
wd <- '/work/HAB4CAST/data_processing/'
setwd(wd)
year <- 2016
#List all files in working directory
allFiles <- list.files(path = 'data/conus_ard_data', full.names = FALSE)

#Load in external data ---- 
#Load Florida Lakes shapefile, ARD Tiles from USGS, and week number assignments (made by Natalie)
#https://www.usgs.gov/media/files/landsat-collection-2-us-ard-tile-grid-shapefile-conus
conus_lakes <- st_cast(st_read('data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp'), "MULTIPOLYGON")
ard_tiles <- st_read('data/CONUS_C2_ARD_grid/conus_c2_ard_grid.shp') %>% st_transform(ard_tiles, crs = st_crs(conus_lakes))
week_assignments <- read_csv('data/week_assignments.csv')

#Create buffer to remove boundary pixels - 300 m pixel step in
conus_lakes_300m_stepin <- st_buffer(conus_lakes, (-300))

## Filter tiles to remove tiles without any lakes----
ard_tiles %>% mutate(tile_identifier = str_c(ifelse(nchar(h) == 1, str_c('00',h),str_c('0',h)),
                                             ifelse(nchar(v) == 1, str_c('00',v),str_c('0',v))),
                     contains_lakes = NA) -> ard_tiles

contains_lake <- function(ard_tiles, conus_lakes){
  ard_tiles$contains_lakes <- lengths(st_intersects(ard_tiles,conus_lakes))
  return(ard_tiles)
}

ard_tiles <- contains_lake(ard_tiles, conus_lakes)


# Keep only file names that have a lake in the tile
allFiles[which(str_sub(allFiles,start = 9, end = 14) %in% pull(filter(ard_tiles,contains_lakes > 0),tile_identifier))] -> allFiles

#Separate the files into QA, B10, or B6
QAfileNames <- allFiles %>% str_subset(pattern = 'QA_PIXEL')
STB10fileNames <- allFiles %>% str_subset(pattern = 'B10')
STB6fileNames <- allFiles %>% str_subset(pattern = 'B6') # for later
#Combine B10 and B6 for all water temperature
STfileNames <- c(STB6fileNames, STB10fileNames)


#Set cloud mask values ----
#Lists of values we want to mask. Values from Tables 2-7 and 2-19 in the Landsat ARD documentation
#https://www.usgs.gov/media/files/landsat-collection-2-us-analysis-ready-data-dfcb
L7.clouds <- c(1, 5440, 442, 5504, 5506, 5696, 5760, 5896, 7440, 7568, 7696, 7824, 7960, 8088, 13664)
L8.clouds <- c(1, 21824, 21826, 21888, 21890, 22080, 22144, 22280, 23888, 23952, 24088, 
               24216, 24344, 24472, 30048, 54596, 54852, 55052, 56856, 56984, 57240)
clouds <- c(L7.clouds, L8.clouds)


#Set up for while loop ----
#Pull the image date from the image name
pull_ard_date <- function(fn){
  str_extract(fn,'[:digit:]{8}') %>% ymd() %>% return()
}

#Pull the first 35 characters from the image name for indexing
regex_detect <- str_c('^[:graph:]{15}',year)

#List all files for a given year
shortName <- unique(str_sub(allFiles, 1, 35))
shortName <- str_subset(shortName,regex_detect)

# Reproject vector to raster crs
ST_crs <- crs(rast(str_c('data/conus_ard_data/',STfileNames[1])))
ard_tiles <- st_transform(ard_tiles, ST_crs)
conus_lakes_300m_stepin <- st_transform(conus_lakes_300m_stepin, ST_crs)

#Function to load both surface temp and QA images, mask ST using QA,
#extract the ST values of the lakes that intersect with the image,
#and output a water temperature value in K

make_table <- function(shortName) {
  #Pull the ST and QA filenames that correspond with the shortName
  ST.name <- STfileNames %>% str_subset(pattern = shortName)
  QA.name <- QAfileNames %>% str_subset(pattern = shortName)
  
  #Extract the ARD tile number from the short name
  tileName <- str_extract(QA.name, '[:digit:]{6}')
  tile <- filter(ard_tiles, tile_identifier == tileName)
  
  # use.tile <- st_intersects(tile, conus_lakes, sparse = FALSE)
  #Determine what resolvable lakes (if any) intersect with the ARD tile
  
  lakes_in_tile <- st_intersection(tile,conus_lakes_300m_stepin)
  #Added line due to error in exact_extract with geometry types
  lakes_in_tile <- st_cast(lakes_in_tile, "MULTIPOLYGON") 
  
  #Load relevant ST and QA images
  ST.image <- rast(str_c('data/conus_ard_data/',ST.name))
  QA.image <- rast(str_c('data/conus_ard_data/',QA.name))

  #Mask the ST image using certain pixel values from the QA image
  st_masked <- mask(
    ST.image,
    QA.image,
    maskvalues = clouds
  ) 

  
  #Extract the ST values for the lakes to get the average daily temperature
  exact_extract(
    st_masked,
    lakes_in_tile,
    fun = 'mean'
  ) ->  extracted_avg_daily_temp
  
  #Add data to summary table and convert the scaled K to normal K
  return(tibble(
    COMID = pull(lakes_in_tile,COMID),
    date = pull_ard_date(ST.image),
    Tile = tileName, 
    satellite = as.numeric(str_extract(ST.name,'[:digit:]{2}')),
    temp_scaled = extracted_avg_daily_temp,
    temp_k = (0.00341802*temp_scaled + 149.0)
  )
 )
}

#Call the function and make the output one large tibble using the bind_rows
plan(multisession, workers = parallel::detectCores() - 1)
st_summary_table <- future_lapply(shortName, make_table) %>% bind_rows()
plan(sequential)

#Calculate lake area percent ----
# make one tibble w percent of lakes in each tile
tile_lake_intersect <- as_tibble(st_intersection(ard_tiles, conus_lakes_300m_stepin)) %>% 
  dplyr::select(h,v,COMID,geometry) %>%
  full_join(dplyr::select(conus_lakes_300m_stepin,COMID,geometry) %>% rename(geometry_full = geometry)) %>% # Join w original geometry
  mutate(fraction_in_tile = as.numeric(st_area(geometry))/as.numeric(st_area(geometry_full))) %>%
  dplyr::select(-geometry,-geometry_full)

#Combine all data into one tibble ----
# Separate out the tile number (hhhvvv)
st_summary_table %>%
  # Parse out tile parameters
  mutate(h = as.numeric(str_extract(Tile,'^[:digit:]{3}')),
         v = as.numeric(str_extract(Tile,'[:digit:]{3}$'))) %>%
  # Join w lake percents
  left_join(tile_lake_intersect) %>% 
  # Handle lakes that were split across two tiles by calculating weighted mean
  group_by(date,COMID,satellite) %>%
  summarize(t = sum(fraction_in_tile*temp_k)) %>%
  # Handle lakes that had overlapping satellite observations
  ungroup() %>% group_by(date,COMID) %>%
  summarize(t = mean(t,na.rm = T)) %>%
  # Add in week assignments and compute average weekly value
  ungroup() %>%
  left_join(week_assignments) %>%
  group_by(COMID,year,week) %>%
  summarise(COMID = COMID,
            year = year,
            week = week,
            date = date,
            temp_k = t) %>% 
  # Drop repeated rows
  distinct() -> mean_wtemp

#Export final weekly mean temp ----
write_feather(mean_wtemp, paste(wd, '/conus_daily_wtemp_16.feather', sep=''), compression = 'zstd', compression_level = 22) # Set save location
proc.time() - ptm