library(sf)

# read in 2015 street tree data
ftp_points <- st_read("/Users/dlm356/dlm356_files/nyc_tree_mortality/geospatial_data/2015_Street_Tree_Census_canopyChange.gpkg")

# Read MapPLUTO
pluto <- st_read("/Volumes/NYC_geo/vectors/nyc_mappluto_21v3_arc_fgdb/MapPLUTO_21v3.gdb", layer = "MapPLUTO_21v3_clipped")

# test with a few points
st_crs(ftp_points) == st_crs(pluto) # crs matches

pluto_nf_index <- st_nearest_feature(ftp_points, pluto)

# get distances - takes a couple minutes
dist_tree_pluto <- st_distance(ftp_points, pluto[pluto_nf_index,], by_element = TRUE)

hist(dist_tree_pluto)
