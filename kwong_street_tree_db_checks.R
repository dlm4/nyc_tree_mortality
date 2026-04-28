library(data.table)
library(arrow)
library(tidyverse)

#r4list <- fread("/Volumes/NYC_geo/R4LIST.csv")
#write_parquet(r4list, "/Volumes/NYC_geo/R4LIST.parquet")

# read back in
r4list <- read_parquet("/Volumes/NYC_geo/R4LIST.parquet")
r4list_samp <- r4list[sample(1:nrow(r4list), 50000),]

# Check LON and LAT

ggplot(r4list_samp) +
  geom_point(aes(x = LON, y = LAT))
# Some outlier points in LON, LAT, should check or remove these

#LON > -67.02
#LAT > 24.56
bad_loc <- r4list %>% subset(LON > -67.02 | LAT < 24.56)
table(bad_loc$CITY)
#clarksville_tn.csv   fallschurch_va.csv       jackson_ms.csv  jerseycity_nj_P.csv     knoxville_tn.csv virginiabeach_va.csv 
#4475                    1                   75                  916                    1                    2

# Clarksville TN - all zeroes
# Jersey City, NJ - LON and LAT are flipped, need to switch columns
# Falls Church ?
# Jackson MS - zeroes
# Knoxville TN
# Virginia Beach VA - LON is OK, bad LAT


# Check DBH
ggplot(r4list_samp) +
  geom_histogram(aes(DBH))
# Some DBH are very large, unreasonable well over 100 cm?
# I would change the column name to DBH_cm

# Check Height
ggplot(r4list_samp) +
  geom_histogram(aes(HEIGHT))
# I would change the column name to HEIGHT_m

# Check species
sp_df <- as.data.frame(table(r4list$SPECIES))
# Betula series?
# Some genera don't have spp. listed after them, just the genus name. Fraxinus, Feijoa, Catalpa, ...?
# Ginkgo can be collapsed into Ginkgo biloba, there are no other Ginkgos

city_df <- as.data.frame(table(r4list$CITY))

# I would include a table in the article, perhaps supplemental, that lists every column in the dataset and describes specifically what it is, in order, to make it as accessible as possible. This is what people would refer to most imo