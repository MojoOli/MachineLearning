### Sample Length ###

getFeatureLength <- function (data){
  # get feature length and store it into dataframe
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
calc_auc <- function(data) {
  aaply(.data = data, .margins = 1, .fun = function(row) {
    flux::auc(x = 0:length(row), y = row)
  })
}

### AUC ###
