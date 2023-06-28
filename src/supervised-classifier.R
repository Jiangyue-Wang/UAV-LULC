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
library()
#read data
field_data <- st_read("field-data/field-data-bands.shp")
head(field_data)
sampdata <- field_data %>% as.data.frame() %>% dplyr::select(-geometry)%>% dplyr::select(Subject, B1, B2, B3, B4, B5, B6, B7, B8,B8A, B9, B11, B12)  

dem <- rast("downloaded-datasets/DEM/suli_dem.tif")
slope <- rast("downloaded-datasets/DEM/suli_slope.tif")
aspect <- rast("downloaded-datasets/DEM/suli_aspect.tif")
TPI <- rast("downloaded-datasets/DEM/suli_TPI.tif")
TRI <- rast("downloaded-datasets/DEM/suli_TRI.tif")

stream <- st_read("downloaded-datasets/Stream/suli_stream.shp") %>% st_union()

sampdata %<>% mutate(elevation = extract(dem, field_data)[,2], slope = extract(slope, field_data)[,2], aspect = extract(aspect, field_data)[,2], TPI = extract(TPI, field_data)[,2], TRI = extract(TRI, field_data)[,2]) %>% mutate(river_dist = st_distance(field_data, stream))

# write_rds(sampdata, "intermediate_rds/sampdata.rds")
sampdata$Subject[sampdata$Subject=="Jiji Grass"] <- "JijiGrass"
### using classification tree----------
cart <- rpart(as.factor(Subject)~., data=sampdata, method = 'class')
windows()
plot(cart, uniform=TRUE, main="Classification Tree")
text(cart, cex = 0.8)

set.seed(99)
j <- kfold(sampdata, k = 5, b=sampdata$Subject)
table(j)
x <- list()
for (k in 1:5) {
  train <- sampdata[j!= k, ]
  test <- sampdata[j == k, ]
  cart <- rpart(as.factor(Subject)~., data=train, method = 'class')
  pclass <- predict(cart, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$Subject, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- unique(sampdata$Subject)
rownames(conmat) <- unique(sampdata$Subject)
conmat

n <- sum(conmat)
n
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
OA


rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa


### random forest--------
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(sampdata$Subject,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)
dt_train <- sampdata[idx_train,]
dt_test <- sampdata[-idx_train,]

n_folds <- 5
folds <- createFolds(1:nrow(dt_train), k = n_folds)
seeds <- vector(mode = "list", length = n_folds+1)
for(i in 1:n_folds) {
  seeds[[i]] <- sample.int(1000, n_folds)
}
seeds[n_folds+1] <- sample.int(1000,1)

ctrl <- trainControl(summaryFunction = multiClassSummary, method = "cv", number = n_folds, search = "grid", classProbs = T, savePredictions = T, index = folds, seeds = seeds)

cl <- makeCluster(detectCores()-4)
registerDoParallel(cl)

model_rf <- caret::train(Subject~., method = "rf", data = dt_train, allowParallel = T, trControl= ctrl)

stopCluster(cl); remove(cl)
registerDoSEQ()

cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test), as.factor(dt_test$Subject))
cm_rf
