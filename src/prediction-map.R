# load library
library(tidyverse)
library(sf)
library(rpart)
library(dismo)
library(terra)
library(caret)
library(randomForest)

# read in data
suli <- st_read("downloaded-datasets/Suli/suli.shp")
ext <- ext(suli)
suli_rast <- rasterize(vect(suli), rast(suli, ncols = round((ext[2]-ext[1])/0.005), nrow = round((ext[4]-ext[3])/0.005)))
suli_cells <- xyFromCell(suli_rast, 1:ncell(suli_rast)) %>% as.data.frame() %>% mutate(CellID = 1:ncell(suli_rast))
