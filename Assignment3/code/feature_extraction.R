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
