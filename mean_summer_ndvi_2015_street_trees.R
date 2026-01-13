# Get mean summer JJA NDVI for each year for each tree as a first cut to identify which trees may have been removed

library(tidyverse)
library(purrr)
library(data.table)
library(sf)

calcNormDif <- function(b1, b2){
  return((b1-b2)/(b1+b2))
}

#setwd('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract_forestry_tree_points/tree_outputs_point')
setwd('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract_2015_street_trees/tree_outputs_point')

file_list <- list.files()
#names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 19, end = 27) %>% ymd()
#names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 49, end = 56) %>% ymd()
names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 37, end = 44) %>% ymd()

# Need to loop over years
for (yr in 2017:2024){
  #yr <- 2017
  print(yr)
  file_list_sub <- file_list[which(month(names(file_list)) %in% 6:8 & year(names(file_list)) == yr)]
  tree_df_sub <- purrr::map_df(file_list_sub, fread, .id = 'date')

  #mean_red <- aggregate(tree_df_sub$red, by = list(tree_df_sub$objectid), FUN = 'mean', na.rm = T)
  #colnames(mean_red) <- c('objectid', 'refl')
  #mean_nir <- aggregate(tree_df_sub$nir, by = list(tree_df_sub$objectid), FUN = 'mean', na.rm = T)
  #colnames(mean_nir) <- c('objectid', 'refl')

  mean_red <- aggregate(tree_df_sub$red, by = list(tree_df_sub$tree_id), FUN = 'mean', na.rm = T)
  colnames(mean_red) <- c('tree_id', 'refl')
  mean_nir <- aggregate(tree_df_sub$nir, by = list(tree_df_sub$tree_id), FUN = 'mean', na.rm = T)
  colnames(mean_nir) <- c('tree_id', 'refl')

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
write.csv(mean_ndvi_all, "mean_summer_ndvi_2015_street_trees.csv", row.names = FALSE)


#####
setwd('/Volumes/NYC_geo/tree_mortality')
mean_ndvi_all <- fread("mean_summer_ndvi_2015_street_trees.csv")
mean_ndvi_all_long <- mean_ndvi_all %>% pivot_longer(cols = 2:9)
colnames(mean_ndvi_all_long) <- c('tree_id', 'Year', 'NDVI')
mean_ndvi_all_long$Year <- factor(mean_ndvi_all_long$Year, labels = 2017:2024)
ggplot(mean_ndvi_all_long) +
  geom_density(aes(x = NDVI, color = Year), linewidth = 1) +
  theme_bw()
ggplot(mean_ndvi_all_long) +
  geom_boxplot(aes(x = as.factor(Year), y = NDVI)) +
  theme_bw()
# 2024 is shifted more positive than other years, sampling issue?

###----

# Can join back to forestry tree points (downloaded 20260106)

#ftp_points <- st_read('/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20260106/geo_export_aef4cd2e-cc1b-40b1-a609-5ed456741201.shp')
ftp_points <- st_read('/Volumes/NYC_geo/tree_mortality/2015_Street_Tree_Census.gpkg')
genus <- sapply(strsplit(ftp_points$spc_latin, " "), '[', 1)
sp1 <- sapply(strsplit(ftp_points$spc_latin, " var. inermis"), '[', 1)
sp2 <- sapply(strsplit(sp1, " 'Crimson King'"), '[', 1)

ftp_points$genus <- genus
ftp_points$species <- sp2

# for filtering, if needed
gen_list <- c("Platanus", "Gleditsia", "Pyrus", "Quercus", "Acer", "Tilia",
              "Prunus", "Zelkova", "Ginkgo", "Styphnolobium", "Ulmus",
              "Liquidambar", "Malus", "Robinia", "Liriodendron", "Betula", "Ailanthus", "Fraxinus")

# join
ftp_points_ndvi <- full_join(ftp_points, mean_ndvi_all)
ftp_points_ndvi_long <- ftp_points_ndvi %>% pivot_longer(cols = 46:53)
ncol_ftp <- ncol(ftp_points_ndvi_long)
colnames(ftp_points_ndvi_long)[(ncol_ftp - 1):ncol_ftp] <- c('Year', 'NDVI')
ftp_points_ndvi_long$Year <- factor(ftp_points_ndvi_long$Year, labels = 2017:2024)

ftp_points_ndvi_long <- ftp_points_ndvi_long %>% filter(status == "Alive", health != "", Year %in% 2017:2021)

ftp_points_ndvi_long$health <- factor(ftp_points_ndvi_long$health, levels = c("Good", "Fair", "Poor"))

setwd("/Volumes/NYC_geo/tree_mortality/plots/")

gen <- "Zelkova"
ftp_points_ndvi_long %>% filter(genus == gen) %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI)) +
  labs(title = gen) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~health) +
  theme_bw()
ggsave(paste0(gen, "_ndvi_2017_2021.jpg"), width = 7, height = 5, units = "in")

gen <- "Fraxinus"
ftp_points_ndvi_long %>% filter(genus == gen) %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI)) +
  labs(title = gen) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~health) +
  theme_bw()
ggsave(paste0(gen, "_ndvi_2017_2021.jpg"), width = 7, height = 5, units = "in")

gen <- "Platanus"
ftp_points_ndvi_long %>% filter(genus == gen) %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI)) +
  labs(title = gen) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~health) +
  theme_bw()
ggsave(paste0(gen, "_ndvi_2017_2021.jpg"), width = 7, height = 5, units = "in")

gen <- "Quercus"
ftp_points_ndvi_long %>% filter(genus == gen) %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI)) +
  labs(title = gen) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~health) +
  theme_bw()
ggsave(paste0(gen, "_ndvi_2017_2021.jpg"), width = 7, height = 5, units = "in")

gen <- "Acer"
ftp_points_ndvi_long %>% filter(genus == gen) %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI)) +
  labs(title = gen) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~health) +
  theme_bw()
ggsave(paste0(gen, "_ndvi_2017_2021.jpg"), width = 7, height = 5, units = "in")

gen <- "Gleditsia"
ftp_points_ndvi_long %>% filter(genus == gen) %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI)) +
  labs(title = gen) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~health) +
  theme_bw()
ggsave(paste0(gen, "_ndvi_2017_2021.jpg"), width = 7, height = 5, units = "in")

#

ftp_points_ndvi_long %>% filter(species == "Acer platanoides") %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI)) + 
  facet_wrap(~health)

ftp_points_ndvi_long %>% #filter(genus == "Acer") %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = NDVI))


# Trying to figure out why 2024 has higher NDVI than other years, shifting in distribution? Something else?
file_list_test <- file_list[which(month(names(file_list)) %in% 6:8 & year(names(file_list)) %in% 2017:2024)]
doy <- yday(names(file_list_test))
yr <- year(names(file_list_test))

yr_doy <- cbind.data.frame(yr, doy)
ggplot(yr_doy) + 
  geom_point(aes(x = yr, y = doy))
# 2024 looks like it has fewer points in July than other years, more observations early and late in summer, not sure if this is the cause


# checking individual months
setwd('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract_2015_street_trees/tree_outputs_point')
for (yr in 2017:2024){
  #yr <- 2017
  print(yr)
  file_list_sub <- file_list[which(month(names(file_list)) %in% 8 & year(names(file_list)) == yr)] # change the month value, check 6, 7, 8, and then combinations
  tree_df_sub <- purrr::map_df(file_list_sub, fread, .id = 'date') 
  
  mean_red <- aggregate(tree_df_sub$red, by = list(tree_df_sub$tree_id), FUN = 'mean', na.rm = T)
  colnames(mean_red) <- c('tree_id', 'refl')
  mean_nir <- aggregate(tree_df_sub$nir, by = list(tree_df_sub$tree_id), FUN = 'mean', na.rm = T)
  colnames(mean_nir) <- c('tree_id', 'refl')
  
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
mean_ndvi_all_long <- mean_ndvi_all %>% pivot_longer(cols = 2:9)
colnames(mean_ndvi_all_long) <- c('tree_id', 'Year', 'NDVI')
mean_ndvi_all_long$Year <- factor(mean_ndvi_all_long$Year, labels = 2017:2024)

ggplot(mean_ndvi_all_long) +
  geom_density(aes(x = NDVI, color = Year), linewidth = 1) +
  theme_bw()
ggplot(mean_ndvi_all_long) +
  geom_boxplot(aes(x = as.factor(Year), y = NDVI)) +
  theme_bw()

# 2024 not uncharacteristically high in June
mean_ndvi_all_long$Month <- 6
mean_ndvi_all_long_6 <- mean_ndvi_all_long

# repeat above section, replacing month with 7
# 2024 is a little higher than other years in July
mean_ndvi_all_long$Month <- 7
mean_ndvi_all_long_7 <- mean_ndvi_all_long

# repeat above section, replacing month with 8
# 2024 is much higher than other years in August
mean_ndvi_all_long$Month <- 8
mean_ndvi_all_long_8 <- mean_ndvi_all_long

mean_ndvi_all_long_bymonth <- bind_rows(mean_ndvi_all_long_6, mean_ndvi_all_long_7, mean_ndvi_all_long_8)
ggplot(mean_ndvi_all_long_bymonth) +
  geom_boxplot(aes(x = as.factor(Month), y = NDVI, fill = as.factor(Year))) +
  theme_bw()
# Not sure why 2024 has higher NDVI than the other years, will have to circle back to it
# 2017 to 2021 should be fine to visualize though