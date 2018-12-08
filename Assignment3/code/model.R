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
band_energy <- getBandEnergy(power, 20)


#sliding window
sw_mad <- slidingWindow(data, mad, 20)
sw_IQR <- slidingWindow(data, IQR, 20)
sw_median <- slidingWindow(data, median, 20)
sw_max <-slidingWindow(data,max,20)
sw_min <-slidingWindow(data,min,20)


df<-data.frame(length_x,sw_mad,sw_IQR,sw_median,sw_max,sw_min,sc,band_energy)


#models: (svm-linear,lda,knn,rf)
models <- list()
registerDoMC(10) # register 3 cores (more cores require more RAM)

trControl <- trainControl(method = 'repeatedcv', 
                          number = 10, 
                          repeats = 5, 
                          returnData = F, 
                          classProbs = T, 
                          returnResamp = 'final', 
                          allowParallel = T)


###LDA###
names(getModelInfo('lda')) # linear discriminant analysis
getModelInfo('lda')[[1]]$parameters # this model does not have any hyperparameters
models$lda <- train(x = df, # exclude the person id 
                         y = general_data[,1],
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'lda', 
                         tuneGrid = NULL, 
                         metric = 'Kappa', 
                         trControl = trControl)
models$lda
cvConfMatrix <- confusionMatrix(models$lda)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
#saveRDS(models$modelLda, file='lda.rds')

###KNN###

names(getModelInfo('knn'))
getModelInfo('knn')[[2]]$parameters  #  k numeric #Neighbors
models$knn <- train(x = df, # exclude the person id 
                                y = general_data[,1],
                                preProcess = c('center', 'scale', 'pca'),  
                                method = 'knn',
                                tuneGrid = expand.grid(k=1:50), 
                                metric = 'Kappa', 
                                trControl = trControl)
models$knn
cvConfMatrix <- confusionMatrix(models$knn)
cvConfMatrix
plot(models$knn)
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
#saveRDS(models$modelKnnRawData, file='knn.rds')


###Random-Forest###
names(getModelInfo('rf')) # random forest
getModelInfo('rf')[[2]]$parameters #mtry numeric #Randomly Selected Predictors
models$rf <- train( x = df, 
                                          y = general_data[,1], 
                                          preProcess = c('center', 'scale', 'pca'),   
                                          method = 'rf',
                                          tuneGrid = expand.grid(mtry=1:20),
                                          metric = 'Kappa', 
                                          trControl = trControl)
models$rf
plot(models$rf)
models$rf$finalModel
plot(models$rf$finalModel)
cvConfMatrix <- confusionMatrix(models$rf)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
#saveRDS(models$modelRandomForestRawData, file='rf.rds')



###SVM-Radial###
names(getModelInfo('svmLinear')) # linear discriminant analysis
getModelInfo('svmLinear')[[1]]$parameters # tau numeric Regularization Parameter
models$svm <- train(x = df, # exclude the person id 
                         y = general_data[,1],
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'svmLinear', 
                         tuneGrid = expand.grid(C=3**(-3:3)), 
                         metric = 'Kappa', 
                         trControl = trControl)
models$svm
cvConfMatrix <- confusionMatrix(models$svm)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
#saveRDS(models$modelLda, file='svm.rds')

