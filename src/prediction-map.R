### load library----
library(tidyverse)
library(sf)
library(rpart)
library(dismo)
library(terra)
library(caret)
library(randomForest)
library(parallel)

### read in data----
suli <- st_read("downloaded-datasets/Suli/suli.shp")
month8 <- rast("maps/Aug_satellite.tif") %>% aggregate(fact = 5)
suli_rast <- rasterize(vect(suli), month8)
suli_cells <- xyFromCell(suli_rast, 1:ncell(suli_rast)) %>% as.data.frame() %>% mutate(CellID = 1:ncell(suli_rast))

dem <- rast("downloaded-datasets/DEM/suli_dem.tif") %>% resample(month8)
slope <- rast("downloaded-datasets/DEM/suli_slope.tif") %>% resample(month8)
aspect <- rast("downloaded-datasets/DEM/suli_aspect.tif") %>% resample(month8)
TPI <- rast("downloaded-datasets/DEM/suli_TPI.tif") %>% resample(month8)
TRI <- rast("downloaded-datasets/DEM/suli_TRI.tif") %>% resample(month8)

suli_cells[,c(4:15)] <- as.data.frame(values(month8))[,1:12]
plot(rasterFromXYZ(suli_cells[,c("x","y","B4")]))
suli_cells$elevation <- values(dem)[,1]
suli_cells$slope <- values(slope)[,1]
suli_cells$aspect <- values(aspect)[,1]
suli_cells$TPI <- values(TPI)[,1]
suli_cells$TRI <- values(TRI)[,1]


stream <- st_read("downloaded-datasets/Stream/suli_stream.shp") %>% st_union()
suli_cells$river_dist <-st_distance(st_as_sf(suli_cells[,1:2],coords = c("x","y"),crs = 4326), stream)[,1]

write_rds(suli_cells, "intermediate_rds/suli_cells.rds")

suli_cells <- na.omit(suli_cells)
# use model
model_rf <- readRDS("intermediate_rds/model_rf.rds")
suli_cells$Subject <- predict(model_rf,suli_cells)

tmp <- readRDS("intermediate_rds/suli_cells.rds")
tmp <- left_join(tmp, suli_cells[,c("CellID","Subject")])
summary(tmp$Subject)
tmp$Subject <- as.character(tmp$Subject)
tmp$Subject[is.na(tmp$Subject)] <- "Null"
tmp$Subject[tmp$Subject=="Shrubland"] <- 1
tmp$Subject[tmp$Subject=="Grassland"] <- 2
tmp$Subject[tmp$Subject=="JijiGrass"] <- 3
tmp$Subject[tmp$Subject=="Bareland"] <- 4
tmp$Subject[tmp$Subject=="Seabuckthorn"] <- 5
tmp$Subject[tmp$Subject=="Snow"] <- 6
tmp$Subject[tmp$Subject=="Null"] <- 0
tmp$Subject <- as.numeric(tmp$Subject)
write_rds(tmp,"intermediate_rds/suli_prediction.rds")


plot(rasterFromXYZ(tmp[,c("x","y","Subject")]))

writeRaster(rasterFromXYZ(tmp[,c("x","y","Subject")]), "maps/suli_habitat_prediction.tif")

### make 10 meter resolution map----
suli <- st_read("downloaded-datasets/Suli/suli.shp")
month8 <- rast("maps/Aug_satellite.tif")
suli_rast <- rasterize(vect(suli), month8)
suli_cells <- xyFromCell(suli_rast, 1:ncell(suli_rast)) %>% as.data.frame() %>% mutate(CellID = 1:ncell(suli_rast))
partition <- floor(nrow(suli_cells)/30)
for(i in 1:30){
  start = (i-1)*partition+1
  end = i*partition
  if(i == 30){end = nrow(suli_cells)}
  saveRDS(suli_cells[start:end,],paste0("intermediate_rds/suli_partition",i,".rds"))
}

### extract data and make predictions
month8 <- rast("maps/Aug_satellite.tif")
dem <- rast("downloaded-datasets/DEM/suli_dem.tif") %>% resample(month8)
slope <- rast("downloaded-datasets/DEM/suli_slope.tif") %>% resample(month8)
aspect <- rast("downloaded-datasets/DEM/suli_aspect.tif") %>% resample(month8)
TPI <- rast("downloaded-datasets/DEM/suli_TPI.tif") %>% resample(month8)
TRI <- rast("downloaded-datasets/DEM/suli_TRI.tif") %>% resample(month8)
stream <- st_read("downloaded-datasets/Stream/suli_stream.shp") %>% st_union()
model_rf <- readRDS("intermediate_rds/model_rf.rds")



for(i in 2:30){
  cat(paste0("start partitioning",i,"\n"))
  start = (i-1)*partition+1
  end = i*partition
  if(i == 30){end = nrow(suli_cells)}
  tmp <- readRDS(paste0("intermediate_rds/suli_partition",i,".rds"))
  
  tmp[,4:15] <- as.data.frame(values(month8))[start:end,1:12]
  colnames(tmp)[4:15] <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12")
  tmp$elevation <- values(dem)[start:end,1]
  tmp$slope <- values(slope)[start:end,1]
  tmp$aspect <- values(aspect)[start:end,1]
  tmp$TPI <- values(TPI)[start:end,1]
  tmp$TRI <- values(TRI)[start:end,1]
  cat("extract finished\n")
  tmp$river_dist <-st_distance(st_as_sf(tmp[,1:2],coords = c("x","y"),crs = 4326), stream)[,1]
  cat("distance calculated\n")
  tmp_predict <- na.omit(tmp)
  if(nrow(tmp_predict)==0){break}
  # use model
  
  tmp_predict$Subject <- predict(model_rf,tmp_predict)
  cat("prediction finished\n")
  tmp <- left_join(tmp, tmp_predict[,c("CellID","Subject")])
  write_rds(tmp,paste0("intermediate_rds/suli_partition_predict",i,".rds"))
  rm(tmp)
  rm(tmp_predict)
  gc()
}

allpixels <- NULL
for(i in 1:30){
  tmp <- readRDS(paste0("intermediate_rds/suli_partition_predict",i,".rds"))
  tmp <- tmp[,c("x","y","Subject")]
  allpixels <- bind_rows(allpixels,tmp)
  rm(tmp)
  gc()
  cat(paste0("finish binding",i))
}
tmp<- allpixels
tmp$Subject <- as.character(tmp$Subject)
tmp$Subject[is.na(tmp$Subject)] <- "Null"
tmp$Subject[tmp$Subject=="Shrubland"] <- 1
tmp$Subject[tmp$Subject=="Grassland"] <- 2
tmp$Subject[tmp$Subject=="JijiGrass"] <- 3
tmp$Subject[tmp$Subject=="Bareland"] <- 4
tmp$Subject[tmp$Subject=="Seabuckthorn"] <- 5
tmp$Subject[tmp$Subject=="Snow"] <- 6
tmp$Subject[tmp$Subject=="Null"] <- 0
tmp$Subject <- as.numeric(tmp$Subject)

writeRaster(rasterFromXYZ(tmp), "maps/suli_habitat_10m_resolution.tif")
plot(rasterFromXYZ(tmp))