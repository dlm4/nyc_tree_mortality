
library(tidyverse)
library(data.table)
library(terra)
library(sf)

# Load tree canopy change raster
#tcc_rast <- rast("/Users/dlm356/dlm356_files/nyc_tree_mortality/geospatial_data/treecanopychange_nyc_2017_2021_6in.tif")

# Load street tree dataset
#st_read("/Users/dlm356/dlm356_files/nyc_tree_mortality/geospatial_data/2015_Street_Tree_Census.gpkg")

# intersect and save this

# Ging Yan already did this
ftp_points <- st_read("/Users/dlm356/dlm356_files/nyc_tree_mortality/geospatial_data/2015_Street_Tree_Census_canopyChange.gpkg")

# Load mean ndvi by year from planetscope
setwd('/Volumes/NYC_geo/tree_mortality')
mean_ndvi_all <- fread("mean_summer_ndvi_2015_street_trees.csv")

# for filtering, if needed
gen_list <- c("Platanus", "Gleditsia", "Pyrus", "Quercus", "Acer", "Tilia",
              "Prunus", "Zelkova", "Ginkgo", "Styphnolobium", "Ulmus",
              "Liquidambar", "Malus", "Robinia", "Liriodendron", "Betula", "Ailanthus", "Fraxinus")

# join
ftp_points_ndvi <- full_join(ftp_points, mean_ndvi_all)

ftp_points_ndvi$ndvi_dif <- ftp_points_ndvi$ndvi_2021 - ftp_points_ndvi$ndvi_2017
ftp_points_ndvi$ndvi_dif_pct <- ftp_points_ndvi$ndvi_dif / ftp_points_ndvi$ndvi_2017 * 100
ftp_points_ndvi$ndvi_dif_pct[which(ftp_points_ndvi$ndvi_2017 < 0)] <- NA

ftp_points_ndvi %>% filter(genus %in% gen_list, canopy_change %in% 3) %>%
ggplot() +
  geom_boxplot(aes(y = ndvi_dif, fill = species), outlier.size = 0.1) +
  facet_wrap(~as.factor(canopy_change), nrow = 3)

ftp_points_ndvi %>% filter(genus %in% gen_list, canopy_change %in% 3) %>%
  ggplot() +
  geom_boxplot(aes(y = ndvi_dif_pct, fill = species), outlier.size = 0.1) +
  facet_wrap(~as.factor(canopy_change), nrow = 3) +
  ylim(-100, 100)


x <- ftp_points_ndvi %>% filter(canopy_change == 3) %>% select(ndvi_dif) %>% st_drop_geometry()
mean(x$ndvi_dif)
median(x$ndvi_dif)

#####
library(ranger)
library(caret)

ftp_points_ndvi_sub <- ftp_points_ndvi %>% select(genus, species, canopy_change, ndvi_2017, ndvi_2018, ndvi_2019, ndvi_2020, ndvi_2021) %>% st_drop_geometry() %>% drop_na()

train_idx <- sample(nrow(ftp_points_ndvi_sub), 0.8 * nrow(ftp_points_ndvi_sub))
ftp_points_ndvi_sub_train <- ftp_points_ndvi_sub[train_idx, ]
ftp_points_ndvi_sub_test <- ftp_points_ndvi_sub[-train_idx, ]

rf_test <- ranger(as.factor(canopy_change) ~ ., data = ftp_points_ndvi_sub_train)

pred_test <- predict(rf_test, data = ftp_points_ndvi_sub_test)
confusionMatrix(pred_test$predictions, as.factor(ftp_points_ndvi_sub_test$canopy_change))
# this didn't work

#####
# Take the difference between 2017 and each subsequent year
# with greater number of negative values, increasingly likely that tree was removed?
# or only difference with 2021?

ftp_points_ndvi <- ftp_points_ndvi %>% mutate(ndvi_dif_18 = ndvi_2018 - ndvi_2017,
                                              ndvi_dif_19 = ndvi_2019 - ndvi_2017,
                                              ndvi_dif_20 = ndvi_2020 - ndvi_2017,
                                              ndvi_dif_21 = ndvi_2021 - ndvi_2017,
                                              ndvi_dif_22 = ndvi_2022 - ndvi_2017)
