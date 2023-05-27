library(raster)
library(sp)
library(terra)

uav <- brick("E:/Jiangyue/无人机/odm_orthophoto.tif")[[1:3]]
uav
set.seed(99)
kmncluster <- kmeans(values(uav), centers = 6, iter.max = 500, nstart = 5, algorithm = "Lloyd")
knr <- setValues(uav, kmncluster$cluster)
plot(knr[[1]], col = c())
writeRaster(knr,"unsupervised-uav.tif")
