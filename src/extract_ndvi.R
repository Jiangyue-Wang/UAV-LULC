### load library----------
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(parallel)
library(ggridges)
### parameter setting----------
n_sample <- 10000
set.seed(23529)

### functions defining-------
normalizedDiff <- function(v1, v2){
  return(((v1-v2)/(v1+v2)))
}

NDVI_habitat <- function(Diff){
  habitat <- c()
  habitat[Diff<=0.1]<-"bare"
  habitat[Diff<=0.3&Diff>0.1]<-"low"
  habitat[Diff>0.3]<-"high"
  return(habitat)
}

### read in suli range--------
suli <- read_sf("downloaded-datasets/Suli/suli.shp")
sample_point <- sf::st_sample(suli, n_sample) %>% st_as_sf() %>% mutate(X = st_coordinates(x)[,1], Y = st_coordinates(x)[,2])

### read and mosaic satellite images, then extract monthly NDVI values
filelist <- readRDS("intermediate_rds/cloudfree-10year-composite_filelist.rds")
head(filelist)

for(j in 1:12){
  MONTH <- j
  for (i in 1:6){
    assign(paste0("img",i), rast(filelist$filedir[MONTH*6-6+i]))
  }
  MONTH_img <- mosaic(img1, img2, img3, img4, img5, img6)
  rm(img1, img2, img3, img4, img5, img6)
  gc()
  
  sample_point[,paste0("Month",str_pad(MONTH,width = 2, side = "left", pad = "0"))] <- normalizedDiff(terra::extract(MONTH_img$B8, sample_point[,c("X","Y")])$B8, terra::extract(MONTH_img$B4, sample_point[,c("X","Y")])$B4)}

# saveRDS(sample_point, "intermediate_rds/sample_point.rds")

summary(sample_point)

sample_point %>% pivot_longer(cols = starts_with("Month"), names_to = "Month", values_to = "NDVI") %>% ggplot() + aes(x = NDVI, y = Month, fill = after_stat(x)) + geom_density_ridges_gradient() + scale_fill_viridis_c() + theme_ridges()
ggsave("figures/Suli_NDVI-distribution-monthly.pdf",width = 6, height = 6, dpi = 600)

sample_point %>% mutate(PointID = 1:n_sample) %>% filter(PointID <= 10000) %>% pivot_longer(cols = starts_with("Month"), names_to = "Month", values_to = "NDVI") %>% mutate(Month = str_sub(Month, start  = 6, end = 7)) %>% ggplot() + aes(x = Month, y = NDVI, group = PointID) + geom_line(alpha = 0.01) + theme_bw()
ggsave("figures/Suli_NDVI-trend-monthly.pdf", width = 6, height = 4, dpi = 600)

sample_point %>% mutate(PointID = 1:n_sample, Diff = Month08-Month02) %>% ggplot() + aes(x = Diff) + geom_density() + theme_bw() + xlab("NDVI difference between August and Feburary")
ggsave("figures/Suli_NDVI-diff.pdf", width = 6, height = 4, dpi = 600)


### calculate Moisture index------

for(j in 1:12){
  MONTH <- j
  for (i in 1:6){
    assign(paste0("img",i), rast(filelist$filedir[MONTH*6-6+i]))
  }
  MONTH_img <- mosaic(img1, img2, img3, img4, img5, img6)
  rm(img1, img2, img3, img4, img5, img6)
  gc()
  
  sample_point[,paste0("MI",str_pad(MONTH,width = 2, side = "left", pad = "0"))] <- normalizedDiff(terra::extract(MONTH_img$B8A, sample_point[,c("X","Y")])$B8A, terra::extract(MONTH_img$B11, sample_point[,c("X","Y")])$B11)}

summary(sample_point)
saveRDS(sample_point,"intermediate_rds/sample_point.rds")

sample_point %>% pivot_longer(cols = starts_with("MI"), names_to = "Month", values_to = "Moisture") %>% ggplot() + aes(x = Moisture, y = Month, fill = after_stat(x)) + geom_density_ridges_gradient() + scale_fill_viridis_c() + theme_ridges()
ggsave("figures/Suli_MI-distribution-monthly.pdf",width = 6, height = 6, dpi = 600)


sample_point %>% mutate(Diff = Month08-Month02, HabitatClass = NDVI_habitat(Diff)) %>% filter(HabitatClass == "low") %>% pivot_longer(cols = starts_with("MI"), names_to = "Month", values_to = "Moisture") %>% ggplot() + aes(x = Moisture, y = Month, fill = after_stat(x)) + geom_density_ridges_gradient() + scale_fill_viridis_c() + theme_ridges()
# cannot see any difference in previous NDVI-defined habitat categories

# Let's see overall map based on NDVI-defined habitat categories
MONTH <- 2
for (i in 1:6){
  assign(paste0("img",i), rast(filelist$filedir[MONTH*6-6+i]))
}
assign(paste0("MONTH", MONTH, "_img"),mosaic(img1, img2, img3, img4, img5, img6))
rm(img1, img2, img3, img4, img5, img6)
gc()
# writeRaster(MONTH2_img, "maps/Feb_satellite.tif")

MONTH <- 8
for (i in 1:6){
  assign(paste0("img",i), rast(filelist$filedir[MONTH*6-6+i]))
}
assign(paste0("MONTH", MONTH, "_img"),mosaic(img1, img2, img3, img4, img5, img6))
rm(img1, img2, img3, img4, img5, img6)
gc()
# writeRaster(MONTH8_img, "maps/Aug_satellite.tif")

MONTH8_NDVI <- (MONTH8_img$B8-MONTH8_img$B4)/(MONTH8_img$B8+MONTH8_img$B4)
MONTH2_NDVI <- (MONTH2_img$B8-MONTH2_img$B4)/(MONTH2_img$B8+MONTH2_img$B4)

NDVI_diff <- MONTH8_NDVI-MONTH2_NDVI
plot(NDVI_diff)
values(NDVI_diff) <- as.numeric(factor(NDVI_habitat(values(NDVI_diff)), levels = c("bare", "low", "high")))
plot(NDVI_diff)
writeRaster(NDVI_diff, "maps/NDVI-defined_habitat.tif")
