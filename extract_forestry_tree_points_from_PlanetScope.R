# Based on: planet_daily_stack_nyc_cal_tree_extract_highsunonly_treepoints_tncfinal_4b_v2.R
# Extracting and calculating mean summer JJA NDVI for all trees in the forestry tree points database
# Using new street tree database file
# '/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20260106/geo_export_aef4cd2e-cc1b-40b1-a609-5ed456741201.shp'
# output_folder <- '/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract_forestry_tree_points'

# Get Planetscope images for JJA

# ----
library(terra)
library(lubridate)
library(sf)
library(reshape2)
#library(dplyr)
library(future)
library(future.apply)

test_rast <- rast("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal/nyc_planet_composite_4band_20220710_nyccal20220710ref.tif")
# use this for reprojecting
#plotRGB(test_rast, r = 4, g = 3, b = 2, stretch = "lin")

tree_point_path_full <- '/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20260106/geo_export_aef4cd2e-cc1b-40b1-a609-5ed456741201.shp'
nyc_ftp_points <- st_read(tree_point_path_full)
nyc_ftp_points$Object_ID <- seq(1,nrow(nyc_ftp_points)) # so there is a unique identifier for each tree # this is OK but in future use # fid_column_name = "OBJECTID"
nyc_ftp_points_reproj <- st_transform(nyc_ftp_points, crs(test_rast)) # this takes about a minute fyi

top_output_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract_forestry_tree_points" # no trailing "/" 

# HERE WE GO
setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal") # source directory
tif_file_list <- list.files(pattern = glob2rx("nyc_planet_composite_4band*nyccal20220710ref*"))
# this has a hard coded substring range, make sure to check this
tif_file_list_datestrings <- strsplit(tif_file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = -26, end = -19)
tif_file_list_dates <- ymd(tif_file_list_datestrings)

sel_tif_date_range <- interval(ymd("20160101"), ymd("20251231")) # expanded date range to capture everything
sel_tif_inds <- which(tif_file_list_dates %within% sel_tif_date_range)
sel_tif_images <- tif_file_list[sel_tif_inds]
sel_tif_images_datestrings <- tif_file_list_datestrings[sel_tif_inds]
sel_tif_images_dates <- tif_file_list_dates[sel_tif_inds]

# need to change so it's not Object_ID, another object id in the forestry tree points

# This is what we actually use, modified!
extractTreesPointByObjectIDRange <- function(i, sel_tif_images, tree_point_sf, set_num){
  sel_tif_rast <- rast(sel_tif_images[i]) # sel_tif_images[i] # could also check if image is within borough
  extracted_trees <- terra::extract(sel_tif_rast, tree_point_sf) # need to use terra::extract for points
  colnames(extracted_trees)[1] <- "objectid" # the original IDs for terra::extract() are just the row numbers
  extracted_trees$objectid <- tree_point_sf$objectid # so these need to get appended. This works fine when it's doing everything at once, but not with unique IDs
  extracted_trees2 <- extracted_trees[which(complete.cases(extracted_trees)),]
  write.csv(extracted_trees2, paste0(top_output_dir, "/tree_outputs_point/nyc_trees_forestry_tree_points_20260106_objset", as.character(set_num), "_", as.character(sel_tif_images_datestrings[i]), "_nyccal20220710ref_point.csv"), row.names = FALSE)
  # clear files
  rm(sel_tif_rast)
  rm(extracted_trees)
  rm(extracted_trees2)
  gc()
  tmpFiles(remove=TRUE)
}

# Need to start a multisession here
plan(multisession, workers = 8) # can increase this again, seems ok for mean calculation when full set is cut up
# doing 8 workers instead of 10 to not push the memory so hard

# To do by tree object_id index range, loop over this and subset the nyc_ftp_polys_reproj each time

start_time <- Sys.time()
# i is the set number, nyc_ftp_polys_reproj_sub is the subset of the larger polygon file set
# Set up ranges for rows (of trees!) to loop over
rowstart <- c(1, 200001, 400001, 600001, 800001, 1000001)
rowend <- c(200000, 400000, 600000, 800000, nrow(nyc_ftp_points_reproj))
#i <- 1 # for loop
for (i in 1:length(rowstart)){
  #for (i in 1:3){
  nyc_ftp_points_reproj_sub <- nyc_ftp_points_reproj[rowstart[i]:rowend[i],]
  future_lapply(1:length(sel_tif_images), FUN = extractTreesPointByObjectIDRange, sel_tif_images, nyc_ftp_points_reproj_sub, i, future.seed = TRUE)
}
stop_time <- Sys.time()
stop_time - start_time
