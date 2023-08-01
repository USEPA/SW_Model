library(sf)
library(geoarrow)
library(lakemorpho)
library(dplyr)
library(lwgeom)
library(elevatr)
#nhd_gdb <- "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
#nhd_waterbody <- st_read(nhd_gdb, "NHDWaterbody", options = "METHOD=ONLY_CCW")
#write_geoparquet(nhd_waterbody, "nhd_waterbody.parquet")
lakes <- st_read('data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp')
aea <- st_crs(lakes)
nhd_waterbody <- read_geoparquet_sf("nhd_waterbody.parquet")
nhd_waterbody <- st_make_valid(nhd_waterbody)
nhd_waterbody <- filter(nhd_waterbody, st_is_valid(nhd_waterbody))
nhd_waterbody <- st_transform(nhd_waterbody, aea)
nhd_waterbody2 <- nhd_waterbody %>%
  mutate(lake_sa = units::set_units(st_area(.), "km2"),
         lake_shoreline = units::set_units(st_perimeter(.), "km"),
         long_dd = data.frame(st_coordinates(st_centroid(.)))$X,
         lat_dd = data.frame(st_coordinates(st_centroid(.)))$X) %>%
  select(COMID, lake_sa, lake_shoreline, long_dd, lat_dd)

  
