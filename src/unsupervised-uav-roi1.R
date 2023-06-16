library(raster)
library(sp)
library(terra)

roi <- sf::read_sf("roi/CP0710-1.shp")
M2 <- brick("roi/Month02_roi1/myExportImageTask.tif")[[1:12]]
seasons <- brick("roi/Month08_roi1/myExportImageTask.tif")[[c(4,8)]]

seasons[[3]] <- (seasons[[2]]-seasons[[1]])/(seasons[[2]]+seasons[[1]])
plot(seasons[[3]])



set.seed(99)
kmncluster <- kmeans(values(seasons), centers = 6, iter.max = 500, nstart = 5, algorithm = "Lloyd")
knr <- setValues(seasons, kmncluster$cluster)
plot(knr[[1]])
writeRaster(knr[[1]],"roi/unsupervised-uav.tif", overwrite = T)
