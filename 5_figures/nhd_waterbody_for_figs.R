library(sf)
library(geoarrow)
library(lakemorpho)
library(dplyr)
library(lwgeom)
library(elevatr)
library(arrow)
#nhd_gdb <- "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
#nhd_waterbody <- st_read(nhd_gdb, "NHDWaterbody", options = "METHOD=ONLY_CCW")
#write_geoparquet(nhd_waterbody, "nhd_waterbody.parquet")
lakes <- st_read('data/OLCI_resolvable_lakes_2022_09_08/OLCI_resolvable_lakes_2022_09_08.shp')
aea <- st_crs(lakes)
nhd_waterbody <- read_geoparquet_sf("nhd_waterbody.parquet")
nhd_waterbody <- st_make_valid(nhd_waterbody)
nhd_waterbody <- filter(nhd_waterbody, st_is_valid(nhd_waterbody)) 
nhd_waterbody <- mutate(nhd_waterbody,
                        long_dd = data.frame(st_coordinates(st_centroid(nhd_waterbody)))$X,
                        lat_dd = data.frame(st_coordinates(st_centroid(nhd_waterbody)))$Y)
nhd_waterbody <- st_transform(nhd_waterbody, aea)
nhd_waterbody2 <- nhd_waterbody %>%
  mutate(lake_sa = units::set_units(st_area(.), "km2"),
         lake_shoreline = units::set_units(st_perimeter(.), "km"),
         long_aea = data.frame(st_coordinates(st_centroid(nhd_waterbody)))$X,
         lat_aea = data.frame(st_coordinates(st_centroid(nhd_waterbody)))$Y) %>%
  select(COMID, lake_sa, lake_shoreline, long_dd, lat_dd, long_aea, lat_aea)

nhd_wb_pts <- nhd_waterbody2
st_geometry(nhd_wb_pts) <- NULL
nhd_wb_pts <- st_as_sf(nhd_wb_pts, coords = c("long_aea", "lat_aea"), crs = st_crs(lakes))
# Get Elevation Next
#x4 <- get_elev_point(nhd_wb_pts, src = "aws", z = 4)
x11 <- get_elev_point(nhd_wb_pts, src = "aws", z = 10, override_size_check = TRUE)
#x10 <- st_transform(x10, crs = st_crs(nhd_waterbody2))

nhd_waterbody2 <- st_join(nhd_waterbody2, x11)
nhd_predictors <- nhd_waterbody2
st_geometry(nhd_predictors) <- NULL
nhd_predictors
write_feather(nhd_predictors, sink = "data/nhd_predictors.feather", compression = "zstd")
