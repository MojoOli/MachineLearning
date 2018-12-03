### Sample Length ###

getFeatureLength <- function (data){
  # get feature length and store it into datafram
  feature_length <- adply(.data = data, .margins = 1, .fun = function(x){
    length(which(!is.na(x)))
  })
  
  # get rid of levels
  feature_length<-Filter(is.numeric, feature_length)
}

### Sample Length ###

### Frequency power###

getFeatureMagnitude <- function (data){
  data[is.na(data)] <- 0
  
  feature_mag <- adply(.data = data, .margins = 1, .fun = function(x){
    Mod(fft(x))
  })
  
  # get rid of levels
  feature_mag<-Filter(is.numeric, feature_mag)
}

### Frequency power ###

### Frequency phase###

getFeaturePhase <- function (data){
  data[is.na(data)] <- 0
  
  feature_mag <- adply(.data = data, .margins = 1, .fun = function(x){
    Arg(fft(x))
  })
  
  # get rid of levels
  feature_mag<-Filter(is.numeric, feature_mag)
}

### Frequency phase ###

### Sliding window ###

slidingWindow <- function(data, func, w_size){
  data[is.na(data)] <- 0
  
  sw_data <- adply(.data = data, .margins = 1, .fun = function(x){
    axis_data <- as.double(rollapply(data = x, width = w_size, by = w_size, partial = T, FUN = func))
  })
}

### Sliding window ###


### AUC ###
# install.package("flux")
library("flux")

# function call auc specified with package name because another function also have function with the same name
calc_auc <- function(data_x, data_y, data_z){
  
  data_x <- as.matrix(data_x)
  data_y <- as.matrix(data_y)
  data_z <- as.matrix(data_z)
  
  calc_auc_one_axis <- function(data) {
    adply(.data = data, .margins = 1, .fun = function(row) {
      flux::auc(x = 0:length(row), y = row)
    })
  }
  
  auc_x <- calc_auc_one_axis(data_x)
  auc_y <- calc_auc_one_axis(data_y)
  auc_z <- calc_auc_one_axis(data_z)
  
  # combine in one data frame and throw away row number columns
  result <- data.frame(c(auc_x, auc_y, auc_z))
  result <- result[,c(-1,-3,-5)]
  # colnames(result) <- c("auc_x", "auc_y", "auc_z") --> produces error, idk why
  return(result)
}

### AUC ###


### Plots ###
plotInterpolatedVsRawData <- function(rawData, interpolatedData, index){
  matplot((data.frame(rawData[index,], interpolatedData[index,])), type='l')
}

feature_length <- getFeatureLength(as.matrix(raw_x))
feature_length$gestures <- lab_x$gesture
boxplot(feature_length$V1~feature_length$gesture)

feature_magnitude = getFeatureMagnitude(raw_x)
barplot(as.vector(t(feature_magnitude[1,])), ylab = 'Frequency power', xlab = 'Frequency', main = 'Frequency power spectrum')

feature_phase = getFeaturePhase(raw_x)
barplot(as.vector(t(feature_phase[1,])), ylab = 'Frequency phase', xlab = 'Frequency', main = 'Frequency phase spectrum')

interpolatedData <- interpolateData(raw_x)
plotInterpolatedVsRawData(raw_x, interpolatedData, 200)