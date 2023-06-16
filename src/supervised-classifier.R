# load library
library(tidyverse)
library(sf)
library(ggmap)
library(rpart)
library(dismo)
# read data
field_data <- st_read("field-data/field-data-bands.shp")
head(field_data)
sampdata <- field_data %>% as.data.frame() %>% dplyr::select(-geometry)%>% dplyr::select(Subject, B1, B2, B3, B4, B5, B6, B7, B8,B8A, B9, B11, B12)  

### using classification tree----------
cart <- rpart(as.factor(Subject)~., data=sampdata, method = 'class')
quartz()
plot(cart, uniform=TRUE, main="Classification Tree")
text(cart, cex = 0.8)

set.seed(99)
j <- kfold(sampdata, k = 3, b=sampdata$Subject)
table(j)
x <- list()
for (k in 1:3) {
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
