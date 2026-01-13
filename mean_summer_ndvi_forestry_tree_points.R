# Get mean summer JJA NDVI for each year for each tree as a first cut to identify which trees may have been removed

library(tidyverse)
library(purrr)
library(data.table)
library(sf)

calcNormDif <- function(b1, b2){
  return((b1-b2)/(b1+b2))
}

setwd('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract_forestry_tree_points/tree_outputs_point')

file_list <- list.files()
#names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 19, end = 27) %>% ymd()
names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 49, end = 56) %>% ymd()

# Need to loop over years
for (yr in 2017:2024){
  #yr <- 2017
  print(yr)
  file_list_sub <- file_list[which(month(names(file_list)) %in% 6:8 & year(names(file_list)) == yr)]
  tree_df_sub <- purrr::map_df(file_list_sub, fread, .id = 'date') 
  
  mean_red <- aggregate(tree_df_sub$red, by = list(tree_df_sub$objectid), FUN = 'mean', na.rm = T)
  colnames(mean_red) <- c('objectid', 'refl')
  mean_nir <- aggregate(tree_df_sub$nir, by = list(tree_df_sub$objectid), FUN = 'mean', na.rm = T)
  colnames(mean_nir) <- c('objectid', 'refl')
  
  mean_ndvi <- mean_nir
  mean_ndvi$refl <- calcNormDif(mean_nir$refl, mean_red$refl)
  
  colnames(mean_ndvi)[2] <- paste0('ndvi_', yr)
  
  if (yr == 2017) {
    mean_ndvi_all <- mean_ndvi
  } else if (yr > 2017){
    mean_ndvi_all <- merge(mean_ndvi_all, mean_ndvi)
  }
}

mean_ndvi_all[,2:9] <- round(mean_ndvi_all[,2:9], 4)

setwd('/Volumes/NYC_geo/tree_mortality')
write.csv(mean_ndvi_all, "mean_summer_ndvi_forestry_tree_points_20260106.csv", row.names = FALSE)


#####
setwd('/Volumes/NYC_geo/tree_mortality')
mean_ndvi_all <- fread("mean_summer_ndvi_forestry_tree_points_20260106.csv")

mean_ndvi_all_long <- mean_ndvi_all %>% pivot_longer(cols = 2:9)
colnames(mean_ndvi_all_long) <- c('objectid', 'Year', 'NDVI')
mean_ndvi_all_long$Year <- factor(mean_ndvi_all_long$Year, labels = 2017:2024)
ggplot(mean_ndvi_all_long) +
  geom_density(aes(x = NDVI, color = Year), linewidth = 1) +
  theme_bw()

###----

# Can join back to forestry tree points (downloaded 20260106)

ftp_points <- st_read('/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20260106/geo_export_aef4cd2e-cc1b-40b1-a609-5ed456741201.shp')

front <- sapply(strsplit(ftp_points$genusspeci, " - "), '[', 1)
species <- sapply(strsplit(front, " '"), '[', 1)
species2 <- sapply(strsplit(species, " var. inermis"), '[', 1)
genus <- sapply(strsplit(species, " "), '[', 1)

ftp_points$genus <- genus
species2[which(species2 == "Platanus x acerfolia")] <- "Platanus x acerifolia" # fix typo in original dataset
ftp_points$species <- species2

# for filtering, if needed
gen_list <- c("Platanus", "Gleditsia", "Pyrus", "Quercus", "Acer", "Tilia",
              "Prunus", "Zelkova", "Ginkgo", "Styphnolobium", "Ulmus",
              "Liquidambar", "Malus", "Robinia", "Liriodendron", "Betula", "Ailanthus", "Fraxinus")

# join
ftp_points_ndvi <- full_join(ftp_points, mean_ndvi_all)
ftp_points_ndvi_long <- ftp_points_ndvi %>% pivot_longer(cols = 16:23)
colnames(ftp_points_ndvi_long)[17:18] <- c('Year', 'NDVI')
ftp_points_ndvi_long$Year <- factor(ftp_points_ndvi_long$Year, labels = 2017:2024)

ftp_points_ndvi_long %>% filter(genus == "Fraxinus", tpstructur == "Retired") %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI))

ftp_points_ndvi_long %>% filter(genus == "Acer") %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI))

#####
# # Old stuff below here
# # Distributions for mean summer NDVI are different between Dove (2017-2020) and SuperDove (2021-2024)
# # Not too different at the low end of the range, so likely can use constant threshold, just need to decide which value to use
# 
# #
# # Check for different NDVI thresholds and review literature
# # 0.25, Fang et al. 2020 (1.2 m, WorldView-3)
# # 0.3, Thapa et al. 2024 (3 m, PlanetScope)
# # 0.6 (and 0.5), Alonzo et al. 2014 (3.7 m, AVIRIS)
# # Does not list a value: Alonzo et al. 2023; Katz et al. 2020
# #
# 
# # filter just for 2024
# ndvi_min <- 0.3 # just picked this number for now, Thapa
# mean_ndvi_all_live <- mean_ndvi_all %>% filter(ndvi_2024 > ndvi_min)
# mean_ndvi_all_live_long <- mean_ndvi_all_live %>% pivot_longer(cols = 2:9)
# colnames(mean_ndvi_all_live_long) <- c('Object_ID', 'Year', 'NDVI')
# mean_ndvi_all_live_long$Year <- factor(mean_ndvi_all_live_long$Year, labels = 2017:2024)
# ggplot(mean_ndvi_all_live_long) +
#   geom_density(aes(x = NDVI, color = Year), linewidth = 1) +
#   #coord_cartesian(xlim = c(0.1, 0.3)) +
#   theme_bw()
# 
# # All years exceed the NDVI minimum
# ndvi_min <- 0.3
# mean_ndvi_all_live <- mean_ndvi_all %>% filter_at(vars(starts_with("ndvi")), all_vars(. > ndvi_min))
# 
# mean_ndvi_all_live_long <- mean_ndvi_all_live %>% pivot_longer(cols = 2:9)
# colnames(mean_ndvi_all_live_long) <- c('Object_ID', 'Year', 'NDVI')
# mean_ndvi_all_live_long$Year <- factor(mean_ndvi_all_live_long$Year, labels = 2017:2024)
# ggplot(mean_ndvi_all_live_long) +
#   geom_density(aes(x = NDVI, color = Year), linewidth = 1) +
#   #coord_cartesian(xlim = c(0.1, 0.3)) +
#   theme_bw()
