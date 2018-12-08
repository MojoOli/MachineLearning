### time domain features
# preprocess data
data <- preprocess(raw_x,raw_y,raw_z)

# length
length_x <- getFeatureLength(raw_x)

# AUC
auc <- calc_auc(data)

### frequency domain features
# power & phase  spectrum
data_mag <- generateMagnitude(raw_x, raw_y, raw_z)
power <- getFeatureMagnitude(data_mag)
phase <- getFeaturePhase(generateMagnitude(raw_x, raw_y, raw_z))

# spectral centroid
sc <- getSpectralCentroid(power)

# spectrum band energy
band_energy <- getBandEnergy(power, 40)


#sliding window
sw_mad <- slidingWindow(data, mad, 20)
sw_IQR <- slidingWindow(data, IQR, 20)
sw_median <- slidingWindow(data, median, 20)


df<-data.frame(length_x,sw_mad,sw_IQR,sw_median,sc,band_energy)


#models: (svm,lda,lda2,knn,rf)
models <- list()
registerDoMC(10) # register 3 cores (more cores require more RAM)

trControl <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 10, 
                          returnData = F, 
                          classProbs = T, 
                          returnResamp = 'final', 
                          allowParallel = T)


###KNN###

names(getModelInfo('knn'))
getModelInfo('knn')[[2]]$parameters  # this model has hyperparameters k (number of neighbors)
models$modelKnnRawData <- train(x = df, # exclude the person id 
                                y = general_data[,1],
                                preProcess = c('center', 'scale', 'pca'),  
                                method = 'knn',
                                tuneGrid = expand.grid(k=1:50), 
                                metric = 'Kappa', 
                                trControl = trControl)
models$modelKnnRawData
cvConfMatrix <- confusionMatrix(models$modelKnnRawData)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
#saveRDS(models$modelKnnRawData, file='modelKnnRawData.rds')


###Random-Forest###
names(getModelInfo('rf')) # random forest
getModelInfo('rf')[[2]]$parameters
models$modelRandomForestRawData <- train( x = df, 
                                          y = general_data[,1], 
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
#saveRDS(models$modelRandomForestRawData, file='modelRandomForestRawData.rds')



###SVM-Radial###
names(getModelInfo('svm')) # linear discriminant analysis
getModelInfo('svmLinear')[[1]]$parameters # this model does not have any hyperparameters
models$modelLda <- train(x = df, # exclude the person id 
                         y = general_data[,1],
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'svmRadial', 
                         tuneGrid = expand.grid(C=3**(-5:5), sigma=3**(-5:5)), 
                         metric = 'Kappa', 
                         trControl = trControl)
models$modelLda
cvConfMatrix <- confusionMatrix(models$modelLda)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
#saveRDS(models$modelLda, file='modelRawDataLda.rds')


###SVM-Linear###
