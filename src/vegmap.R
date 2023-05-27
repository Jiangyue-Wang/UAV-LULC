### load library--------
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(ggspatial)
library(showtext)

### read in data------
veg_china <- read_sf("downloaded-datasets/Vegetation-map/vegemap-layer.shp")
suli <- read_sf("downloaded-datasets/Suli/Suli.shp")
road <- read_sf("downloaded-datasets/tele_road/tele_road.shp") %>% st_transform(crs = crs(suli))

#check geometry in vegemap
st_is_valid(veg_china)
veg_china_valid <- st_make_valid(veg_china)
st_is_valid(veg_china_valid)

veg_suli <- st_intersection(veg_china_valid,suli)
road_suli <- st_intersection(road,suli)

ggplot() + layer_spatial(veg_suli,aes(fill=zbxz)) + labs(fill = "Landcover") + layer_spatial(road_suli) + theme_bw()
ggsave("maps/vegemap-landcover.pdf", width = 6, height = 4, dpi = 600)

ggplot() + layer_spatial(veg_suli,aes(fill=qx)) + labs(fill = "Vegetation") + layer_spatial(road_suli) + theme_bw()
ggsave("maps/vegemap-vegetation.pdf", width = 6, height = 4, dpi = 600)
