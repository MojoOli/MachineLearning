###########################################################################################
#                           Assignment 02: Face Recognition                               #
#                                                                                         #
# • Goal is to build (a) successful face recognition model(s). The model should learn to  #
#   distinguish between N people from face images, then being able to decide for new      #
#   face images which of the N participants shown. Load the data, derive features and     #
#   evaluate different models. Try to achieve the best possible performance by changing   #
#   your feature derivation (always use proper data partitioning):                        #
#     – Take some of the original pixel features of your data that lie new to each other  #
#       (e.g. first 100) and do a correlation plot for them. What do you see?             #
#     – Feature derivation likely is a multi-step-process. You can use other feature      #
#       derivation approaches than those discussed in the lecture too – there are many    #
#       of them. Use “whatever floats your boat”, but be sure to understand your          #
#       own toolchain. E.g. try changing details, like differently sized input images,    #
#       more/less/different features, and compare differences in results.                 #
#     – What is the best approach (features, model, . . . ) you can come up with to       #
#       successfully distinguish people? What do you think of the data/the results?       #
#       Chose appropriate metrics to underline your statements.                           #
# • After solving the above points, answer this question: what happens if we build our    #
#   model from such a data set, then somebody not part of it uses a system where the      #
#   model is deployed? What could you do about it/how could such systems possibly         #
#   work?                                                                                 #
###########################################################################################

### Libraries and workspace setup
<<<<<<< HEAD
library(imager)
=======
install.packages("doMC")
>>>>>>> origin/master
library(corrplot)
library(png)
library(plyr)
library(imager)
library(caret) # caret ML framework
library(doMC) # parallelization
registerDoMC(8) # register 8 cores (more cores require more RAM)
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

####### Edge Detection

# convert data to matrix to be able to use plyr without variable grouping 
# and to calculate the imgradient for every picture

gMagData <- alply(as.matrix(imgData[,2:2501]), .margins=1, .fun=function(x){
  cImgData <- as.cimg(matrix(data = x, nrow = 50, ncol = 50))
  imgradient(cImgData,"xy") %>% enorm
})

# show difference in images
normalImage <- as.cimg(matrix(data = unlist(imgData[1,2:2501]), nrow = 50, ncol = 50))
sidePlot <- as.imlist(list(grayscale=normalImage, "gradient magnitude" =gMagData[[1]]))
plot(sidePlot, layout="row", xlab="pixel", ylab="pixel")

# threshold(gMagData[[1]],"60%") %>% plot(main="Determinant: 40% highest values")

# convert to data frame
# threshold where only the 40% highest values will be taken into account
gMagDataFrame <- ldply(gMagData, .fun=function(x){
  base<-x[,,1,1]
  as.vector(base)
  # as.numeric(threshold(base,"60%"))
})
# get rid of levels
gMagDataFrame<-Filter(is.numeric, gMagDataFrame)

#######

### Sliding window approach
library(zoo)
windowSize<-4
swImgData <- alply(as.matrix(imgData[,2:2501]), .margins=1, .fun=function(x){
  imgData <- matrix(data = x, nrow = 50, ncol = 50)
  imgData <- rollapply(data = imgData, width = windowSize, by = windowSize, FUN = IQR)
  imgData <- as.cimg(t(rollapply(data = t(imgData), width = windowSize, by = windowSize, FUN = IQR)))
})

# show difference in images
sidePlot <- as.imlist(list(grayscale=normalImage, "sliding window image" = swImgData[[1]]))
plot(sidePlot, layout="row", xlab="pixel", ylab="pixel")

# convert to data frame
swImgDataFrame <- ldply(swImgData, .fun=function(x){
  base<-x[,,1,1]
  as.vector(base)
})
# get rid of levels
swImgDataFrame<-Filter(is.numeric, swImgDataFrame)

# data partitioning, group by person
indexes_train = createDataPartition(imgData$`1`, p=0.75, list=F)
indexes_test = (1:nrow(imgData))[-indexes_train]

rawDataTraining <- imgData[indexes_train,]
rawDataTesting <- imgData[indexes_test,]

gMagDataTraining <- gMagDataFrame[indexes_train,]
gMagDataTesting <- gMagDataFrame[indexes_test,]

swImgDataTrainig <- swImgDataFrame[indexes_train,]
swImgDataTesting <- swImgDataFrame[indexes_test,]

### Draw correlation plots for the first couple of features
corrplot(cor(imgData[,2:50]))
corrplot(cor(imgData[,1001:1101]))
corrplot(cor(t(imgData[1:60,1201:1301])))
###

### Training + Setup
models <- list()
trControl <- trainControl(method = 'repeatedcv', 
                          number = 10, 
                          repeats = 20, 
                          returnData = F, 
                          classProbs = T, 
                          returnResamp = 'final', 
                          allowParallel = T)


## LDA Model 
# Preprocessing: centering, scaling and PCA 
# Partitioning: 10CV Repeated 20 times
# Raw data
# models$modelLda <- readRDS('modelRawDataLda.rds')
names(getModelInfo('lda')) # linear discriminant analysis
getModelInfo('lda')[[1]]$parameters # this model does not have any hyperparameters
<<<<<<< HEAD
models$modelLda <- train(x = rawDataTraining[,-1], # exclude the person id 
                         y = rawDataTraining[,1],
=======
?train
models$modelLda <- train(x = gMagDataFrame,
                         y = imgData[,1],
>>>>>>> origin/master
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'lda', 
                         tuneGrid = NULL, 
                         metric = 'Kappa', 
                         trControl = trControl)
models$modelLda
cvConfMatrix <- confusionMatrix(models$modelLda)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelLda, file='modelRawDataLda.rds')

# gMagDataFrame
# models$modelLdaGMag <- readRDS('modelLdaGMag.rds')
models$modelLdaGMag <- train(x = gMagDataTraining,
                         y = rawDataTraining[,1],
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'lda', 
                         tuneGrid = NULL, 
                         metric = 'Kappa', 
                         trControl = trControl)
models$modelLdaGMag
cvConfMatrix <- confusionMatrix(models$modelLdaGMag)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelLdaGMag, file='modelLdaGMag.rds')

# sw approach
# models$modelLdaSW <- readRDS('modelLdaSW.rds')
models$modelLdaSW <- train(x = swImgDataTrainig,
                         y = rawDataTraining[,1],
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'lda', 
                         tuneGrid = NULL, 
                         metric = 'Kappa', 
                         trControl = trControl)
models$modelLdaSW
cvConfMatrix <- confusionMatrix(models$modelLdaSW)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelLdaSW, file='modelLdaSW.rds')
##

## KNN model
# Raw data
# models$modelKnnRawData <- readRDS('modelKnnRawData.rds')
names(getModelInfo('knn'))
getModelInfo('knn')[[2]]$parameters  # this model has hyperparameters k (number of neighbors)
models$modelKnnRawData <- train(x = rawDataTraining[,-1], # exclude the person id 
                  y = rawDataTraining[,1],
                  preProcess = c('center', 'scale', 'pca'),  
                  method = 'knn',
                  tuneGrid = expand.grid(k=1:10), 
                  metric = 'Kappa', 
                  trControl = trControl)
models$modelKnnRawData
cvConfMatrix <- confusionMatrix(models$modelKnnRawData)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelKnnRawData, file='modelKnnRawData.rds')

# gMagDataFrame
# models$modelKnnGMagwData <- readRDS('modelKnnGMagwData.rds')
models$modelKnnGMagwData <- train(x = gMagDataTraining, 
                                y = rawDataTraining[,1],
                                preProcess = c('center', 'scale', 'pca'),  
                                method = 'knn',
                                tuneGrid = expand.grid(k=1:10), 
                                metric = 'Kappa', 
                                trControl = trControl)
models$modelKnnGMagwData
cvConfMatrix <- confusionMatrix(models$modelKnnGMagwData)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelKnnGMagwData, file='modelKnnGMagwData.rds')

# sw approach
# models$modelKnnSW <- readRDS('modelKnnSW.rds')
models$modelKnnSW <- train(x = swImgDataTrainig, 
                                  y = rawDataTraining[,1],
                                  preProcess = c('center', 'scale', 'pca'),  
                                  method = 'knn',
                                  tuneGrid = expand.grid(k=1:10), 
                                  metric = 'Kappa', 
                                  trControl = trControl)
models$modelKnnSW
cvConfMatrix <- confusionMatrix(models$modelKnnSW)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelKnnSW, file='modelKnnSW.rds')

## random forest
# Raw data
# models$modelRandomForestRawData <- readRDS('modelRandomForestRawData.rds')
names(getModelInfo('rf')) # random forest
getModelInfo('rf')[[2]]$parameters
models$modelRandomForestRawData <- train( x = rawDataTraining[,-1], 
                                   y = rawDataTraining[,1], 
                                   preProcess = c('center', 'scale', 'pca'),   
                                   method = 'rf',
                                   tuneGrid = expand.grid(mtry=1:4),
                                   metric = 'Kappa', 
                                   trControl = trControl)
models$modelRandomForestRawData
plot(models$modelRandomForestRawData)
models$modelRandomForestRawData$finalModel
plot(models$modelRandomForestRawData$finalModel)
cvConfMatrix <- confusionMatrix(models$modelRandomForestRawData)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelRandomForestRawData, file='modelRandomForestRawData.rds')

# gMadData
# models$modelRandomForestGMagData <- readRDS('modelRandomForestGMagData.rds')
models$modelRandomForestGMagData <- train( x = gMagDataTraining, 
                                          y = rawDataTraining[,1], 
                                          preProcess = c('center', 'scale', 'pca'),   
                                          method = 'rf',
                                          tuneGrid = expand.grid(mtry=1:4),
                                          metric = 'Kappa', 
                                          trControl = trControl)
models$modelRandomForestGMagData
plot(models$modelRandomForestGMagData)
models$modelRandomForestGMagData$finalModel
plot(models$modelRandomForestGMagData$finalModel)
cvConfMatrix <- confusionMatrix(models$modelRandomForestGMagData)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelRandomForestGMagData, file='modelRandomForestGMagData.rds')

# sw approach
# models$modelRandomForestSW  <- readRDS('modelRandomForestSW.rds')
# models$modelRandomForestSWMean  <- readRDS('modelRandomForestSWMean.rds')
# models$modelRandomForestSWMad  <- readRDS('modelRandomForestSWMad.rds')
# models$modelRandomForestSWIQR  <- readRDS('mmodelRandomForestSWIQR.rds')
models$modelRandomForestSWIQR <- train( x = swImgDataTrainig, 
                                        y = rawDataTraining[,1], 
                                        preProcess = c('center', 'scale', 'pca'),   
                                        method = 'rf',
                                        tuneGrid = expand.grid(mtry=1:4),
                                        metric = 'Kappa', 
                                        trControl = trControl)
models$modelRandomForestSWIQR
saveRDS(models$modelRandomForestSWIQR, file='modelRandomForestSWIQR.rds')

models$modelRandomForestSWMad <- train( x = swImgDataTrainig, 
                                         y = rawDataTraining[,1], 
                                         preProcess = c('center', 'scale', 'pca'),   
                                         method = 'rf',
                                         tuneGrid = expand.grid(mtry=1:4),
                                         metric = 'Kappa', 
                                         trControl = trControl)
models$modelRandomForestSWMad
saveRDS(models$modelRandomForestSWMad, file='modelRandomForestSWMad.rds')

models$modelRandomForestSWMean <- train( x = swImgDataTrainig, 
                                     y = rawDataTraining[,1], 
                                     preProcess = c('center', 'scale', 'pca'),   
                                     method = 'rf',
                                     tuneGrid = expand.grid(mtry=1:4),
                                     metric = 'Kappa', 
                                     trControl = trControl)
models$modelRandomForestSWMean
saveRDS(models$modelRandomForestSWMean, file='modelRandomForestSWMean.rds')

models$modelRandomForestSW <- train( x = swImgDataTrainig, 
                                           y = rawDataTraining[,1], 
                                           preProcess = c('center', 'scale', 'pca'),   
                                           method = 'rf',
                                           tuneGrid = expand.grid(mtry=1:4),
                                           metric = 'Kappa', 
                                           trControl = trControl)
models$modelRandomForestSW
plot(models$modelRandomForestSW)
models$modelRandomForestSW$finalModel
cvConfMatrix <- confusionMatrix(models$modelRandomForestSW)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelRandomForestSW, file='modelRandomForestSW.rds')

results <- resamples(list(RFMEAD=models$modelRandomForestSW, RFMEAN=models$modelRandomForestSWMean, RFIQR=models$modelRandomForestSWIQR, RFMAD=models$modelRandomForestSWMad))
summary(results)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


## Naive Bayes
# Raw Data
# models$modelNaiveBayesRawData <- readRDS('modelNaiveBayesRawData.rds')
names(getModelInfo('nb')) # naive bayes 
getModelInfo('nb')[[1]]$parameters 
models$modelNaiveBayesRawData <- train( x = rawDataTraining[,-1], 
                                 y = rawDataTraining[,1], 
                                 preProcess = c('center', 'scale', 'pca'),   
                                 method = 'nb', 
                                 tuneGrid = NULL,
                                 metric = 'Kappa', 
                                 trControl = trControl)
models$modelNaiveBayesRawData
plot(models$modelNaiveBayesRawData)
str(models$modelNaiveBayesRawData$finalModel) 
cvConfMatrix <- confusionMatrix(models$modelNaiveBayesRawData)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelNaiveBayesRawData, file='modelNaiveBayesRawData.rds')

# gMag Data
# models$modelNaiveBayesGMagData <- readRDS('modelNaiveBayesGMagData.rds')
models$modelNaiveBayesGMagData <- train( x = gMagDataTraining, 
                                         y = rawDataTraining[,1], 
                                        preProcess = c('center', 'scale', 'pca'),   
                                        method = 'nb', 
                                        tuneGrid = NULL,
                                        metric = 'Kappa', 
                                        trControl = trControl)
models$modelNaiveBayesGMagData
plot(models$modelNaiveBayesGMagData)
str(models$modelNaiveBayesGMagData$finalModel) 
cvConfMatrix <- confusionMatrix(models$modelNaiveBayesGMagData)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelNaiveBayesGMagData, file='modelNaiveBayesGMagData.rds')


# SW approach
# models$modelNaiveBayesSW <- readRDS('modelNaiveBayesSW.rds')
models$modelNaiveBayesSW <- train( x = swImgDataTrainig, 
                                   y = rawDataTraining[,1],  
                                        preProcess = c('center', 'scale', 'pca'),   
                                        method = 'nb', 
                                        tuneGrid = NULL,
                                        metric = 'Kappa', 
                                        trControl = trControl)
models$modelNaiveBayesSW
plot(models$modelNaiveBayesSW)
str(models$modelNaiveBayesSW$finalModel) 
cvConfMatrix <- confusionMatrix(models$modelNaiveBayesSW)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
saveRDS(models$modelNaiveBayesSW, file='modelNaiveBayesSW.rds')

results <- resamples(list(LDARawData=models$modelLda, LDAGMagData=models$modelLdaGMag, LDASW=models$modelLdaSW, KNNRawData=models$modelKnnRawData, KNNGMadData=models$modelKnnGMagwData, KNNSW=models$modelKnnSW, RFRawData=models$modelRandomForestRawData, RFGMagData=models$modelRandomForestGMagData, RFSW=models$modelRandomForestSW, NBRawData=models$modelNaiveBayesRawData, NBGMagData=models$modelNaiveBayesGMagData, NBSW=models$modelNaiveBayesSW))
summary(results)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

# Testing with RF
predictedFinal <- predict(models$modelRandomForestRawData, newdata=rawDataTesting[,-1])
conf <- confusionMatrix(data = predictedFinal, reference = rawDataTesting[,1])
levelplot(sweep(x = conf$table, STATS = colSums(conf$table), MARGIN = 2, FUN = `/`), 
          col.regions=gray(100:0/100))
plot(models$modelRandomForestRawData)
###
