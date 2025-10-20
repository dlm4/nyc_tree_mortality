
library(tidyverse)
library(data.table)
library(sf)
library(purrr)

# mean summer NDVI
mean_ndvi <- fread("/Volumes/NYC_geo/tree_mortality/mean_summer_ndvi.csv")



# monthly NDVI (for subset of trees)
##

# practical classification
# can filter for known labeled trees (from street tree database source)
# Or that were predicted from our classification

cls <- st_read("/Volumes/NYC_geo/tree_classification/outputs_practical_v1_monthcomp/cls_poly_practical_v1_monthcomp.gpkg")

#genus <- "Acer"
#plot_color <- "forestgreen"

#genus <- "Fraxinus"
#plot_color <- "navy"

genus <- "Gleditsia"
plot_color <- "sienna"

ids_ref <- cls$Poly_ID[cls$Genus_Ref == genus]
ids_pred <- cls$Poly_ID[cls$Genus_Predicted == genus]

mean_ndvi_dif <- mean_ndvi %>% select(!ndvi_2017) %>% 
  mutate(ndvi_2019_dif = ndvi_2019 - ndvi_2018,
         ndvi_2020_dif = ndvi_2020 - ndvi_2018,
         ndvi_2021_dif = ndvi_2021 - ndvi_2018,
         ndvi_2022_dif = ndvi_2022 - ndvi_2018,
         ndvi_2023_dif = ndvi_2023 - ndvi_2018,
         ndvi_2024_dif = ndvi_2024 - ndvi_2018) %>%
  select(Object_ID, ndvi_2019_dif, ndvi_2020_dif, ndvi_2021_dif, ndvi_2022_dif, ndvi_2023_dif, ndvi_2024_dif)

#ids_ref <- cls$Poly_ID[cls$Genus_Ref == "Quercus"]

# mean_ndvi <- mean_ndvi %>% pivot_longer(cols = colnames(mean_ndvi)[2:9])
# colnames(mean_ndvi)[2] <- "Year"
# mean_ndvi$Year <- strsplit(mean_ndvi$Year, "_") %>% sapply('[[', 2) %>% as.numeric()
# mean_ndvi_ref <- mean_ndvi %>% filter(Object_ID %in% ids_ref)
# 
# ggplot(mean_ndvi_ref) +
#   geom_boxplot(aes(x = as.factor(Year), y = value))


mean_ndvi_dif <- mean_ndvi_dif %>% pivot_longer(cols = colnames(mean_ndvi_dif)[2:7])
colnames(mean_ndvi_dif)[2] <- "Year"
mean_ndvi_dif$Year <- strsplit(mean_ndvi_dif$Year, "_") %>% sapply('[[', 2) %>% as.numeric() %>% as.factor()

mean_ndvi_dif_ref <- mean_ndvi_dif %>% filter(Object_ID %in% ids_ref)

ggplot(mean_ndvi_dif_ref) +
  geom_line(aes(x = Year, y = value, group = Object_ID), alpha = 0.05, color = plot_color) +
  labs(y = "Change in NDVI from 2018", title = genus) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_bw()
