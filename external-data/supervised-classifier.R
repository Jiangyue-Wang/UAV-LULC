# load library
library(tidyverse)
library(sf)
library(rpart)
library(dismo)
library(terra)
library(caret)
library(randomForest)
library(parallel)
library(doParallel)
library(MLmetrics)
library(nnet)
library(neuralnet)
#read data
field_data <- st_read("external-data/avail.pts.RSF_bands_data.shp")
head(field_data)
sampdata <- field_data %>% as.data.frame() %>% dplyr::select(-geometry)%>% dplyr::select(B1, B2, B3, B4, B5, B6, B7, B8,B8A, B9, B11, B12)  

dem <- rast("downloaded-datasets/DEM/suli_dem.tif")
slope <- rast("downloaded-datasets/DEM/suli_slope.tif")
aspect <- rast("downloaded-datasets/DEM/suli_aspect.tif")
TPI <- rast("downloaded-datasets/DEM/suli_TPI.tif")
TRI <- rast("downloaded-datasets/DEM/suli_TRI.tif")

stream <- st_read("downloaded-datasets/Stream/suli_stream.shp") %>% st_union()

sampdata %<>% mutate(elevation = extract(dem, field_data)[,2], slope = extract(slope, field_data)[,2], aspect = extract(aspect, field_data)[,2], TPI = extract(TPI, field_data)[,2], TRI = extract(TRI, field_data)[,2]) %>% mutate(river_dist = st_distance(field_data, stream))


### use ANN to map-------
model_nnet <- readRDS("intermediate_rds/model_nnet.rds")
sampdata$Subject <- predict(model_nnet, newdata = sampdata)
write_rds(sampdata, "external-data/avail.pts.rsf_habitat.rds")
