

### preprocess data
data_x <- preprocess(raw_x)
data_y <- preprocess(raw_y)
data_z <- preprocess(raw_z)
data_m <- preprocess(raw_m)

### feature extraction
# length
length_x <- getFeatureLength(raw_x)
length_y <- getFeatureLength(raw_y)
length_z <- getFeatureLength(raw_z)
# power spectrum
power_x <- getFeatureMagnitude(data_x)
power_y <- getFeatureMagnitude(data_y)
power_z <- getFeatureMagnitude(data_z)
# phase spectrum
phase_x <- getFeaturePhase(data_x)
phase_y <- getFeaturePhase(data_y)
phase_z <- getFeaturePhase(data_z)
# AUC
auc_x <- calc_auc_one_axis(data_x)
auc_y <- calc_auc_one_axis(data_y)
auc_z <- calc_auc_one_axis(data_z)

df<-data.frame(length_x,length_y,length_z,power_x,power_y,power_z,phase_x,phase_y,phase_z,auc_x,auc_y,auc_z)


# raw_data partitioning
# mad
raw_mad_df <- create_df(raw_x, raw_y, raw_z, mad, 5)
# mean
raw_mean_df <- create_df(raw_x, raw_y, raw_z, mean, 5)
# max
raw_max_df <- create_df(raw_x, raw_y, raw_z, max, 5)
# min
raw_min_df <- create_df(raw_x, raw_y, raw_z, min, 5)
# median
raw_median_df <- create_df(raw_x, raw_y, raw_z, median, 5)

# partitioning
# population dependent (first shuffle)
set.seed(81295)
raw_shuffle <- raw_mad_df[sample(nrow(raw_mad_df)),]

raw_train = createDataPartition(raw_shuffle$person_id, p=0.75, list=F)
raw_test = (1:nrow(raw_shuffle))[-raw_train]

# mad
pd_raw_mad_train <- raw_mad_df[raw_train,]
pd_raw_mad_test <- raw_mad_df[raw_test,]

# mean
pd_raw_mean_train <- raw_mean_df[raw_train,]
pd_raw_mean_test <- raw_mean_df[raw_test,]

# max
pd_raw_max_train <- raw_max_df[raw_train,]
pd_raw_max_test <- raw_max_df[raw_test,]

# min
pd_raw_min_train <- raw_min_df[raw_train,]
pd_raw_min_test <- raw_min_df[raw_test,]


# median
pd_raw_median_train <- raw_median_df[raw_train,]
pd_raw_median_test <- raw_median_df[raw_test,]

# population independent (remove person with id 0)
# mad
pi_raw_mad_train <- subset(raw_mad_df, person_id!=0)
pi_raw_mad_test <- subset(raw_mad_df, person_id==0)

# mean
pi_raw_mean_train <- subset(raw_mean_df, person_id!=0)
pi_raw_mean_test <- subset(raw_mean_df, person_id==0)

# max
pi_raw_max_train <- subset(raw_max_df, person_id!=0)
pi_raw_max_test <- subset(raw_max_df, person_id==0)

# min
pi_raw_min_train <- subset(raw_min_df, person_id!=0)
pi_raw_min_test <- subset(raw_min_df, person_id==0)

# median
pi_raw_median_train <- subset(raw_median_df, person_id!=0)
pi_raw_median_test <- subset(raw_median_df, person_id==0)

# magnitude data partitioning
mag_x <- getFeatureMagnitude(raw_x)
mag_y <- getFeatureMagnitude(raw_y)
mag_z <- getFeatureMagnitude(raw_z)
# mad
mag_mad_df <- create_df(mag_x, mag_y, mag_z, mad, 5)
# mean
mag_mean_df <- create_df(mag_x, mag_y, mag_z, mean, 5)
# max
mag_max_df <- create_df(mag_x, mag_y, mag_z, max, 5)
# min
mag_min_df <- create_df(mag_x, mag_y, mag_z, min, 5)
# median
mag_median_df <- create_df(mag_x, mag_y, mag_z, median, 5)

# partitioning
# population dependent (first shuffle)
set.seed(81295)
mag_shuffle <- mag_mad_df[sample(nrow(mag_mad_df)),]

mag_train = createDataPartition(mag_shuffle$person_id, p=0.75, list=F)
mag_test = (1:nrow(mag_shuffle))[-mag_train]

# mad
pd_mg_mad_train <- mag_mad_df[mag_train,]
pd_mag_mad_test <- mag_mad_df[mag_test,]

# mean
pd_mag_mean_train <- mag_mean_df[mag_train,]
pd_mag_mean_test <- mag_mean_df[mag_test,]

# max
pd_mag_max_train <- mag_max_df[mag_train,]
pd_mag_max_test <- mag_max_df[mag_test,]

# min
pd_mag_min_train <- mag_min_df[mag_train,]
pd_mag_min_test <- mag_min_df[mag_test,]


# median
pd_mag_median_train <- mag_median_df[mag_train,]
pd_mag_median_test <- mag_median_df[mag_test,]

# population independent (remove person with id 0)
# mad
pi_mag_mad_train <- subset(mag_mad_df, person_id!=0)
pi_mag_mad_test <- subset(mag_mad_df, person_id==0)

# mean
pi_mag_mean_train <- subset(mag_mean_df, person_id!=0)
pi_mag_mean_test <- subset(mag_mean_df, person_id==0)

# max
pi_mag_max_train <- subset(mag_max_df, person_id!=0)
pi_mag_max_test <- subset(mag_max_df, person_id==0)

# min
pi_mag_min_train <- subset(mag_min_df, person_id!=0)
pi_mag_min_test <- subset(mag_min_df, person_id==0)

# median
pi_mag_median_train <- subset(mag_median_df, person_id!=0)
pi_mag_median_test <- subset(mag_median_df, person_id==0)

#population independent (raw/feature) vs population dependent (raw/feature)
#models: (svm,lda,lda2,knn,rf)
models <- list()
registerDoMC(12) # register 3 cores (more cores require more RAM)

trControl <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 10, 
                          returnData = F, 
                          classProbs = T, 
                          returnResamp = 'final', 
                          allowParallel = T)


names(getModelInfo('lda')) # linear discriminant analysis
getModelInfo('lda')[[1]]$parameters # this model does not have any hyperparameters
models$modelLda <- train(x = df, # exclude the person id 
                         y = raw[,1],
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'lda', 
                         tuneGrid = NULL, 
                         metric = 'Kappa', 
                         trControl = trControl)
models$modelLda
cvConfMatrix <- confusionMatrix(models$modelLda)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
#saveRDS(models$modelLda, file='modelRawDataLda.rds')
