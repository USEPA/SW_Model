# This script is for downloading PRISM data (hopefully :) )
# Author: Natalie Von Tress
# Date Created: 05/10/2022

# Reference:
# https://cran.r-project.org/web/packages/prism/vignettes/prism.html#download-30-year-normal-data

# We want:
## Daily PRISM data 
## from May 2016-May 2019
## Variables: Air temperature (tmean; ATEMP) and preciptiation (ppt; PRECIP)

# Note: Files cannot be directly uploaded to the network drive; you must download locally, then copy the files over to the network

# Set up workspace ----

# Clear environment
rm(list = ls(all = T))

# Load packages
library(tidyverse)
library(raster)
library(prism)

# Download air temperature data ----
# 2020-2022 data
prism_set_dl_dir('data/PRISM/atemp')

get_prism_dailys(
  type = 'tmean',
  minDate = '2022-08-01',
  maxDate = '2022-12-31',
  keepZip = F
)

# Convert to raster files
atemp_stack <- pd_stack(prism_archive_ls())
#writeRaster(atemp_stack,filename = list.files('/work/HAB4CAST/data_processing/data/prism_atemp_data',full.names = T),format = 'raster',bylayer = T)


