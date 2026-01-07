
library(tidyverse)
library(data.table)
library(sf)
library(purrr)
library(ggrepel)

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

genus <- "Fraxinus"
plot_color <- "navy"

#genus <- "Ginkgo"
#plot_color <- "navy"

ids_ref <- cls$Poly_ID[cls$Genus_Ref == genus]
ids_pred <- cls$Poly_ID[cls$Genus_Predicted == genus]
ids_merged <- cls$Poly_ID[cls$Genus_Merged == genus]

mean_ndvi_dif <- mean_ndvi %>% select(!ndvi_2017) %>% 
  mutate(ndvi_2018_dif = ndvi_2018 - ndvi_2018, # all zeros for visualization
         ndvi_2019_dif = ndvi_2019 - ndvi_2018,
         ndvi_2020_dif = ndvi_2020 - ndvi_2018,
         ndvi_2021_dif = ndvi_2021 - ndvi_2018,
         ndvi_2022_dif = ndvi_2022 - ndvi_2018,
         ndvi_2023_dif = ndvi_2023 - ndvi_2018,
         ndvi_2024_dif = ndvi_2024 - ndvi_2018) %>%
  select(Object_ID, ndvi_2018_dif, ndvi_2019_dif, ndvi_2020_dif, ndvi_2021_dif, ndvi_2022_dif, ndvi_2023_dif, ndvi_2024_dif)

#ids_ref <- cls$Poly_ID[cls$Genus_Ref == "Quercus"]

# mean_ndvi <- mean_ndvi %>% pivot_longer(cols = colnames(mean_ndvi)[2:9])
# colnames(mean_ndvi)[2] <- "Year"
# mean_ndvi$Year <- strsplit(mean_ndvi$Year, "_") %>% sapply('[[', 2) %>% as.numeric()
# mean_ndvi_ref <- mean_ndvi %>% filter(Object_ID %in% ids_ref)
# 
# ggplot(mean_ndvi_ref) +
#   geom_boxplot(aes(x = as.factor(Year), y = value))


mean_ndvi_dif <- mean_ndvi_dif %>% pivot_longer(cols = colnames(mean_ndvi_dif)[2:8]) # hard coded range of columns!
colnames(mean_ndvi_dif)[2] <- "Year"
mean_ndvi_dif$Year <- strsplit(mean_ndvi_dif$Year, "_") %>% sapply('[[', 2) %>% as.numeric() %>% as.factor()

mean_ndvi_dif_ref <- mean_ndvi_dif %>% filter(Object_ID %in% ids_ref)

ggplot(mean_ndvi_dif_ref) +
  geom_line(aes(x = Year, y = value, group = Object_ID), alpha = 0.05, color = plot_color) +
  labs(y = "Change in NDVI from 2018", title = genus) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_bw()


#
cls_genus <- cls %>% filter(Poly_ID %in% ids_ref)
cls_genus <- st_centroid(cls_genus)

mean_ndvi_dif_genus_2024 <- mean_ndvi_dif %>% filter(Object_ID %in% ids_ref, Year == 2024)

cls_genus_dif <- merge(cls_genus, mean_ndvi_dif_genus_2024, by.x = "Poly_ID", by.y = "Object_ID")

boros <- st_read("/Volumes/NYC_geo/vectors/Borough Boundaries/geo_export_da133389-a6c6-45c3-a980-14295f0e4c2f.shp")
boros <- st_transform(boros, st_crs(cls))

ggplot(cls_genus_dif) +
  geom_sf(data = boros) +
  geom_sf(aes(color = value), shape = 1, alpha = 0.5) +
  scale_color_gradient2(low = "sienna", mid = "gray80", high = "forestgreen", midpoint = 0) +
  theme_bw()

cls_genus_dif$value_ranges <- cut(cls_genus_dif$value, breaks = c(-1, -0.2, -0.02, 0.02, 0.2, 1))

ggplot(cls_genus_dif) +
  geom_sf(data = boros) +
  geom_sf(aes(fill = value_ranges), color = "gray20", shape = 21, alpha = 0.8, size = 2) +
  scale_fill_manual(values = c('#e66101','#fdb863','#f7f7f7','#80cdc1','#018571')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = "NDVI change from 2018", title = "Fraxinus (reference)") +
  theme_bw()

cls_genus_dif$value_ranges <- cut(cls_genus_dif$value, breaks = c(-1, -0.2, 1))

ggplot(cls_genus_dif) +
  geom_sf(data = boros) +
  geom_sf(aes(fill = value_ranges), color = "gray20", shape = 21, alpha = 0.8, size = 2) +
  scale_fill_manual(values = c('#e66101','#f7f7f7')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  #labs(fill = "NDVI change from 2018", title = "Fraxinus (reference)") +
  theme_bw()

cls_genus_dif %>% filter(value <= -0.2) %>%
ggplot() +
  geom_sf(data = boros) +
  geom_sf(fill = '#e66101', color = "gray20", shape = 21, alpha = 0.8, size = 2) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Fraxinus (reference)") +
  theme_bw()

# Now can map it with the predicted (mapped) Fraxinus
cls_genus <- cls %>% filter(Poly_ID %in% ids_pred)
cls_genus <- st_centroid(cls_genus)

mean_ndvi_dif_genus_2024 <- mean_ndvi_dif %>% filter(Object_ID %in% ids_pred, Year == 2024)

cls_genus_dif <- merge(cls_genus, mean_ndvi_dif_genus_2024, by.x = "Poly_ID", by.y = "Object_ID")

cls_genus_dif %>% filter(value <= -0.2) %>%
  ggplot() +
  geom_sf(data = boros) +
  geom_sf(fill = '#e66101', color = "gray20", shape = 21, alpha = 0.8, size = 2) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Fraxinus (mapped)") +
  theme_bw()

# Now can map it with the merged (mapped+reference) Fraxinus
cls_genus <- cls %>% filter(Poly_ID %in% ids_merged)
cls_genus <- st_centroid(cls_genus)

mean_ndvi_dif_genus_2024 <- mean_ndvi_dif %>% filter(Object_ID %in% ids_merged, Year == 2024)

cls_genus_dif <- merge(cls_genus, mean_ndvi_dif_genus_2024, by.x = "Poly_ID", by.y = "Object_ID")

cls_genus_dif %>% filter(value <= -0.2) %>%
  ggplot() +
  geom_sf(data = boros) +
  geom_sf(fill = '#e66101', color = "gray20", shape = 21, alpha = 0.8, size = 2) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Fraxinus (mapped+ref)") +
  theme_bw()

cls_genus_dif_ll <- cls_genus_dif %>% filter(value <= -0.2) %>% st_transform("EPSG:4326")


#####

#id <- 183535
id <- 1234465

mean_ndvi_dif_ref %>% filter(Object_ID == id) %>%
ggplot() +
  geom_line(aes(x = Year, y = value, group = Object_ID), color = plot_color) +
  labs(y = "Change in NDVI from 2018", title = paste0("Tree ID: ", id)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_bw()


#####

mean_ndvi_dif_2024 <- mean_ndvi_dif %>% filter(Year == 2024)
cls_all_dif <- merge(cls, mean_ndvi_dif_2024, by.x = "Poly_ID", by.y = "Object_ID")

ref_all <- table(cls_all_dif$Genus_Ref)
ref_loss <- table(cls_all_dif$Genus_Ref[cls_all_dif$value <= -0.2])
df_join_dif <- full_join(as.data.frame(ref_all), as.data.frame(ref_loss), by = "Var1")
colnames(df_join_dif) <- c("Genus", "All", "Loss")
df_join_dif$Loss[which(is.na(df_join_dif$Loss))] <- 0
df_join_dif$Pct_Loss <- df_join_dif$Loss/df_join_dif$All*100

df_join_dif %>% filter(Genus != "Unknown") %>%
ggplot(aes(x = Pct_Loss, y = Loss)) +
  geom_point() +
  geom_text_repel(aes(label = Genus))+
  labs(x = "% Known lost tree stems", y = "Number of lost known tree stems", title = "Known tree stems with NDVI 2024-2018 <= -0.2 (ie 'Lost Stems')") +
  theme_bw()


pred_all <- table(cls_all_dif$Genus_Pred)
pred_loss <- table(cls_all_dif$Genus_Pred[cls_all_dif$value <= -0.2])
df_join_dif <- full_join(as.data.frame(pred_all), as.data.frame(pred_loss), by = "Var1")
colnames(df_join_dif) <- c("Genus", "All", "Loss")
df_join_dif$Loss[which(is.na(df_join_dif$Loss))] <- 0
df_join_dif$Pct_Loss <- df_join_dif$Loss/df_join_dif$All*100

df_join_dif %>% filter(Genus != "Unknown") %>%
  ggplot(aes(x = Pct_Loss, y = Loss)) +
  geom_point() +
  geom_text_repel(aes(label = Genus))+
  labs(x = "% Predicted lost tree stems", y = "Number of predicted lost tree stems", title = "Predicted tree stems with NDVI 2024-2018 <= -0.2 (ie 'Lost Stems')") +
  theme_bw()


table(cls$Species_Ref[cls$Genus_Ref == "Ulmus"])


######
# Counts
library(sf)

# Street tree database
tree_pts <- st_read("/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20240228/geo_export_3a66edb6-f0e4-44ec-9a82-e8b36349c231.shp")

front <- sapply(strsplit(tree_pts$genusspeci, " - "), '[', 1)
species <- sapply(strsplit(front, " '"), '[', 1)
species2 <- sapply(strsplit(species, " var. inermis"), '[', 1)
genus <- sapply(strsplit(species, " "), '[', 1)

tree_pts$genus <- genus
species2[which(species2 == "Platanus x acerfolia")] <- "Platanus x acerifolia" # fix typo in original dataset
tree_pts$species <- species2

tree_counts <- table(tree_pts$genus)
tree_counts_pct <- tree_counts / sum(tree_counts) * 100

gen_list <- c("Acer", "Ailanthus", "Betula", "Fraxinus", "Ginkgo", "Gleditsia", "Liquidambar", "Liriodendron", "Malus", "Platanus", "Prunus", "Pyrus", "Quercus", "Robinia", "Styphnolobium", "Tilia", "Ulmus", "Zelkova")

tree_counts_pct[which(names(tree_counts_pct) %in% gen_list)] %>% sum() # 82.4%

tree_dbh_sums <- aggregate(tree_pts$dbh, by = list(genus), FUN = sum, na.rm = TRUE)

dbh_total <- tree_dbh_sums$x %>% sum()
