### load library--------
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(ggspatial)
library(showtext)

### read in polygons I have drawn in Google map
sitesdir <- "field-sites"
sites <- dir(sitesdir)

rdmp <- NULL

for (i in 1:length(sites)){
  plg <- st_read(paste0(sitesdir,"/",sites[i]))
  ctg <- str_sub(sites[i], start = 1, end = 2)
  tmp_rdmp <- as.data.frame(matrix(unlist(st_sample(plg, 1000)), ncol=2, byrow = T))
  tmp_rdmp$category <- ctg
  colnames(tmp_rdmp)[1:2] <- c("long","lat")
  rdmp <- bind_rows(rdmp, tmp_rdmp)
  
}

GR4 <- terra::rast("downloaded-datasets/sentinel-bands48/2022-07-22-00:00_2022-07-22-23:59_Sentinel-2_L2A_B04_(Raw).tiff")
GR8 <- terra::rast("downloaded-datasets/sentinel-bands48/2022-07-22-00:00_2022-07-22-23:59_Sentinel-2_L2A_B08_(Raw).tiff")
NG4 <- terra::rast("downloaded-datasets/sentinel-bands48/2022-01-18-00:00_2022-01-18-23:59_Sentinel-2_L2A_B04_(Raw).tiff")
NG8 <- terra::rast("downloaded-datasets/sentinel-bands48/2022-01-18-00:00_2022-01-18-23:59_Sentinel-2_L2A_B08_(Raw).tiff")

rdmp %<>% st_as_sf(coords = c(1,2)) %>% st_set_crs(4326) %>% st_transform(32647)

NDVI_GR <- (GR8-GR4)/(GR8+GR4)
NDVI_NG <- (NG8-NG4)/(NG8+NG4)

rdmp$NDVI_GR <- extract(NDVI_GR, rdmp)[,2]
rdmp$NDVI_NG <- extract(NDVI_NG, rdmp)[,2]

rdmp_long <- pivot_longer(rdmp, cols = starts_with("NDVI"), names_to = "Season", values_to = "Value")

ggplot(rdmp_long, aes(x = Season, y = Value, fill = category)) + geom_boxplot()

rdmp$abs_diff <- rdmp$NDVI_GR-rdmp$NDVI_NG
rdmp$rel_diff <- rdmp$abs_diff/rdmp$NDVI_NG
rdmp %>% ggplot()+ aes(x = category, y = abs_diff, fill = category) + geom_boxplot()
ggsave("figures/absolute_diff_NDVI.png",width = 6, height = 4, dpi = 600)

rdmp %>% ggplot()+ aes(x = category, y = rel_diff, fill = category) + geom_boxplot()
ggsave("figures/relative_diff_NDVI.png",width = 6, height = 4, dpi = 600)
