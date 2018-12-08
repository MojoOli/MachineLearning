### Sample Length ###

getFeatureLength <- function (data){
  # get feature length and store it into dataframe
  feature_length <- adply(.data = data, .margins = 1, .fun = function(x){
    length(which(!is.na(x)))
  })
  
  # get rid of levels
  feature_length<-Filter(is.numeric, feature_length)
}


### Frequency power###

getFeatureMagnitude <- function (data){
  data[is.na(data)] <- 0
  
  feature_mag <- aaply(.data = data, .margins = 1, .fun = function(x){
    Mod(fft(x))
  })
}


### Frequency phase###

getFeaturePhase <- function (data){
  data[is.na(data)] <- 0
  
  feature_mag <- adply(.data = data, .margins = 1, .fun = function(x){
    Arg(fft(x))
  })
  
  # get rid of levels
  feature_mag<-Filter(is.numeric, feature_mag)
}


### Spectral Centroid ###

getSpectralCentroid <- function(data){
  feature_sc <- adply(.data = data, .margins = 1, .fun = function(x){
    data_na <- x[!is.na(x)]
    mean(data_na)
  })
  
  # get rid of levels
  feature_sc<-Filter(is.numeric, feature_sc)
}

### Band energy ###

getBandEnergy <- function(data, width){
  feature_band <- adply(.data = data, .margins = 1, .fun = function(x){
    x[1:width]
  })
  
  # get rid of levels
  feature_band<-Filter(is.numeric, feature_band)
}


### Sliding window ###

slidingWindow <- function(data, func, w_size){
  data[is.na(data)] <- 0
  
  sw_data <- adply(.data = data, .margins = 1, .fun = function(x){
    axis_data <- as.double(rollapply(data = x, width = w_size, by = w_size, partial = T, FUN = func))
  })
  sw_data<-Filter(is.numeric, sw_data)
  droplevels(sw_data)
}

### Sliding window ###


### AUC ###
calc_auc <- function(data) {
  feature_auc <- adply(.data = data, .margins = 1, .fun = function(row) {
    flux::auc(x = 0:length(row), y = row)
  })
  # get rid of levels
  feature_auc<-Filter(is.numeric, feature_auc)
}

