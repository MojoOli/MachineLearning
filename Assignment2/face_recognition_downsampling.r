### Libraries and workspace setup
library(corrplot)
library(png)
library(plyr)
library(caret) # caret ML framework
library(doMC) # parallelization
library(imager)
registerDoMC(4) # register 3 cores (more cores require more RAM)
###

### Import data
# Import data from the images and add the person number as the first column
imgData <- ldply(dir(path = 'dsr-preprocessed-1000x1333_faces_haar_gray_resized150_equalized_50x50/', 
                     full.names= T, pattern = 'png'), 
                 function(f) {
                   t(
                     c(
                       as.numeric(strsplit(f, '_')[[1]][8]),
                       as.numeric(readPNG(f))))
                 });
# Convert the person number into a "normalized" string by adding P and ensuring it is always (at least) 3 characters long
# Then convert it to a factor (a class label)
imgData[,1] <- as.factor(sprintf("P%02d", imgData[,1]))
levels(imgData$`1`)
summary(imgData$`1`)
### 

downsampleAndTrain <- function(data, pcaThreshold) {
  pca_res <- prcomp(x = as.matrix(data[,2:2501]), retx = T, center = T, scale. = T, tol = pcaThreshold)
  downsampled <- scale(data[,2:2501], center = pca_res$center, scale = pca_res$scale) %*% pca_res$rotation
  all(abs(downsampled - pca_res$x) < 0.00001)
  
  trainedModel <- train(x = downsampled,   # Train with downsampled data
                              y = data[,1], # Still look for the persons
                              preProcess = NULL, # Preprocessing is already done
                              method = 'lda', 
                              tuneGrid = NULL, 
                              metric = 'Kappa', 
                              trControl = trainControl(method = 'repeatedcv', 
                                                       number = 10, 
                                                       repeats = 20, 
                                                       returnData = F, 
                                                       classProbs = T, 
                                                       returnResamp = 'final',
                                                       allowParallel = T
                                                       ))
  return(trainedModel)
}

## Try out different combinations
models <- list()
models$Thres0.9 <- downsampleAndTrain(imgData, 0.9)
models$Thres0.5 <- downsampleAndTrain(imgData, 0.5)
models$Thres0.3 <- downsampleAndTrain(imgData, 0.3)
models$Thres0.2 <- downsampleAndTrain(imgData, 0.2)
models$Thres0.1 <- downsampleAndTrain(imgData, 0.1)

for (thres in seq(0.1, 0.9, 0.1)) {
  models[[paste('Thres', thres, sep = "")]] <- downsampleAndTrain(imgData, thres)
}

results <- resamples(models)
summary(results)
bwplot(results)

cvConfMatrix <- confusionMatrix(models$Thres0.9)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))

downsampleAndRestore <- function(data, pcaThreshold) {
  # Do Downsampling
  pca_res <- prcomp(x = as.matrix(data[,2:2501]), retx = T, center = T, scale. = T, tol = pcaThreshold)
  downsampled <- scale(data[,2:2501], center = pca_res$center, scale = pca_res$scale) %*% pca_res$rotation
  all(abs(downsampled - pca_res$x) < 0.00001)
  # Reverse the transformations
  reversed <- scale(pca_res$x %*% t(pca_res$rotation), scale = 1/pca_res$scale, center= -pca_res$center/pca_res$scale)
  all(abs(data[,2:2501] - reversed) < 0.00001)
  return(reversed)
}

downsampleAndPlot <- function (data, pcaThreshold) {
  reversed <- downsampleAndRestore(data, pcaThreshold)
  # show difference in images
  normalImage <- as.cimg(matrix(data = unlist(data[1,2:2501]), nrow = 50, ncol = 50))
  restImage <- as.cimg(matrix(data = unlist(reversed[1,]), nrow = 50, ncol = 50))
  sidePlot <- as.imlist(list("original" = normalImage, "downsampled" = restImage))
  plot(sidePlot, layout="row")
}

downsampleAndPlot(imgData, 0.4)
downsampled09 <- downsampleAndRestore(imgData, 0.9)
downsampled05 <- downsampleAndRestore(imgData, 0.5)
downsampled03 <- downsampleAndRestore(imgData, 0.3)
downsampled02 <- downsampleAndRestore(imgData, 0.2)
downsampled01 <- downsampleAndRestore(imgData, 0.1)

normalImage <- as.cimg(matrix(data = unlist(data[1,2:2501]), nrow = 50, ncol = 50))
restImage09 <- as.cimg(matrix(data = unlist(downsampled09[1,]), nrow = 50, ncol = 50))
restImage05 <- as.cimg(matrix(data = unlist(downsampled05[1,]), nrow = 50, ncol = 50))
restImage03 <- as.cimg(matrix(data = unlist(downsampled03[1,]), nrow = 50, ncol = 50))
restImage02 <- as.cimg(matrix(data = unlist(downsampled02[1,]), nrow = 50, ncol = 50))
restImage01 <- as.cimg(matrix(data = unlist(downsampled01[1,]), nrow = 50, ncol = 50))

images <- as.imlist(list("original" = normalImage
                        , "downsampled_0.1" = restImage01
                        , "downsampled_0.2" = restImage02
                        , "downsampled_0.3" = restImage03
                        , "downsampled_0.5" = restImage05
                        , "downsampled_0.9" = restImage09))
plot(images)
