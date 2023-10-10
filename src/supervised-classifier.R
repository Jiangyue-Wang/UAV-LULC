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
field_data <- st_read("field-data/field-data-bands_1.shp") %>% bind_cols(st_read("field-data/field-data-bands_2.shp")[,1:6]) %>% st_as_sf()%>%st_set_crs(4326)
head(field_data) 
field_xy <- st_coordinates(field_data$geometry...15)
sampdata <- field_data %>% as.data.frame() %>% dplyr::select(-geometry...8, -geometry...15)%>% dplyr::select(Subject, B1, B2, B3, B4, B5, B6, B7, B8,B8A, B9, B11, B12)  

dem <- rast("downloaded-datasets/DEM/suli_dem.tif")
slope <- rast("downloaded-datasets/DEM/suli_slope.tif")
aspect <- rast("downloaded-datasets/DEM/suli_aspect.tif")
TPI <- rast("downloaded-datasets/DEM/suli_TPI.tif")
TRI <- rast("downloaded-datasets/DEM/suli_TRI.tif")

stream <- st_read("downloaded-datasets/Stream/suli_stream.shp") %>% st_union()

sampdata %<>% mutate(elevation = extract(dem, field_xy)[,1], slope = extract(slope, field_xy)[,1], aspect = extract(aspect, field_xy)[,1], TPI = extract(TPI, field_xy)[,1], TRI = extract(TRI, field_xy)[,1]) %>% mutate(river_dist = st_distance(field_data, stream)[,1])

# write_rds(sampdata, "intermediate_rds/sampdata.rds")
sampdata <- readRDS("intermediate_rds/sampdata.rds")
sampdata$Subject[sampdata$Subject=="Jiji Grass"] <- "JijiGrass"
sampdata <- na.omit(sampdata)
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


### other ML models--------
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(sampdata$Subject,
                                 p = 0.8, 
                                 list = FALSE)
dt_train <- sampdata[idx_train,]
dt_test <- sampdata[-idx_train,]

n_folds <- 5

TrainingParameters<- trainControl(method = "repeatedcv", number = n_folds, repeats = n_folds)

## support vector machine
model_SVM <- train(Subject ~ ., data = dt_train, method = "svmPoly", trControl = TrainingParameters, preProcess = c("scale","center"), na.action = na.omit)

cm_SVM <- confusionMatrix(predict(model_SVM, dt_test), as.factor(dt_test$Subject))
cm_SVM

## random forest
model_rf <- caret::train(Subject ~ ., method = "rf", data = dt_train,  preProcess = c("scale", "center"),trControl= TrainingParameters, na.action = na.omit)

cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test), as.factor(dt_test$Subject))
cm_rf

## decision tree
model_decTree <- caret::train(Subject ~ ., method = "C5.0", data = dt_train,  preProcess = c("scale", "center"),trControl= TrainingParameters, na.action = na.omit)

cm_decTree <- confusionMatrix(data = predict(model_decTree, newdata = dt_test), as.factor(dt_test$Subject))
cm_decTree

## naive Bayes
model_nb <- caret::train(Subject ~ ., method = "nb", data = dt_train,  preProcess = c("scale", "center"),trControl= TrainingParameters, na.action = na.omit)

cm_nb <- confusionMatrix(data = predict(model_nb, newdata = dt_test), as.factor(dt_test$Subject))
cm_nb

## artificial neural network
model_nnet <- caret::train(Subject ~ ., method = "nnet", data = dt_train, preProcess = c("scale", "center"), trControl= ctrl)


cm_nnet <- confusionMatrix(data = predict(model_nnet, newdata = dt_train), as.factor(dt_train$Subject))
cm_nnet

modelPerformance <- data.frame(model = c("SVM", "RF", "NB", "NN","decTree"), Accuracy = c(cm_SVM$overall[1],cm_rf$overall[1],cm_nb$overall[1],cm_nnet$overall[1], cm_decTree$overall[1]))

### test if training data is enough----
modelPerformance <- NULL

for(i in seq(0.1,0.99,0.1)){
  print(i)
  set.seed(321)
  # A stratified random split of the data
  idx_train <- createDataPartition(sampdata$Subject,
                                   p = i, 
                                   list = FALSE)
  dt_train <- sampdata[idx_train,]
  dt_test <- sampdata[-idx_train,]
  
  n_folds <- 5
  
  TrainingParameters<- trainControl(method = "repeatedcv", number = n_folds, repeats = n_folds)
  
  ## support vector machine
  model_SVM <- train(Subject ~ ., data = dt_train, method = "svmPoly", trControl = TrainingParameters, preProcess = c("scale","center"), na.action = na.omit)
  
  cm_SVM <- confusionMatrix(predict(model_SVM, dt_test), as.factor(dt_test$Subject))
  
  ## random forest
  model_rf <- caret::train(Subject ~ ., method = "rf", data = dt_train,  preProcess = c("scale", "center"),trControl= TrainingParameters, na.action = na.omit)
  
  cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test), as.factor(dt_test$Subject))
  
  ## decision tree
  model_decTree <- caret::train(Subject ~ ., method = "C5.0", data = dt_train,  preProcess = c("scale", "center"),trControl= TrainingParameters, na.action = na.omit)
  
  cm_decTree <- confusionMatrix(data = predict(model_decTree, newdata = dt_test), as.factor(dt_test$Subject))

  ## naive Bayes
  model_nb <- caret::train(Subject ~ ., method = "nb", data = dt_train,  preProcess = c("scale", "center"),trControl= TrainingParameters, na.action = na.omit)
  
  cm_nb <- confusionMatrix(data = predict(model_nb, newdata = dt_test), as.factor(dt_test$Subject))

  
  ## artificial neural network
  model_nnet <- caret::train(Subject ~ ., method = "nnet", data = dt_train, preProcess = c("scale", "center"), trControl= TrainingParameters)
  
  
  cm_nnet <- confusionMatrix(data = predict(model_nnet, newdata = dt_train), as.factor(dt_train$Subject))

  tmp <- data.frame(model = c("SVM", "RF", "NB", "NN","decTree"), Accuracy = c(cm_SVM$overall[1],cm_rf$overall[1],cm_nb$overall[1],cm_nnet$overall[1], cm_decTree$overall[1]), DataProp = i)
  
  modelPerformance <- bind_rows(modelPerformance, tmp)

}
saveRDS(modelPerformance, "intermediate_rds/modelPerformance.rds")
ggplot(modelPerformance, aes(x = DataProp, y = Accuracy, color = model)) + geom_line(linetype = "dashed") + geom_point() + theme_bw() + xlab("Proportion of data used for model training") + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))

ggsave("figures/supervised-accuracy-trend.pdf", width = 6, height = 3, dpi = 600)

### use ANN to map-------
model_nnet <- caret::train(Subject ~ ., method = "nnet", data = sampdata, preProcess = c("scale", "center"), trControl= TrainingParameters)
saveRDS(model_nnet, "intermediate_rds/model_nnet.rds")

cm_nnet <- confusionMatrix(data = predict(model_nnet, newdata = sampdata), as.factor(sampdata$Subject))
cm_nnet

model_rf <- caret::train(Subject ~ ., method = "rf", data = sampdata,  preProcess = c("scale", "center"),trControl= TrainingParameters, na.action = na.omit)
saveRDS(model_rf, "intermediate_rds/model_rf.rds")

