### load library----------
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(parallel)

### parameter setting----------
n_sample <- 10000
set.seed(23529)

### functions defining-------
normalizedDiff <- function(v1, v2){
  return(((v1-v2)/(v1+v2)))
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



