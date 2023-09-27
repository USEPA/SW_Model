library(geoarrow)
library(sf)
# might need to download data
# https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z
nhd_gdb <- "nhdplus/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
nhd_waterbody <- st_read(nhd_gdb, "NHDWaterbody", options = "METHOD=ONLY_CCW")
write_geoparquet(nhd_waterbody, "nhd_waterbody.parquet", compression = "zstd", compression_level = 21)
