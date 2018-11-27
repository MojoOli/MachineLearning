### Sample Length ###

# get feature length and store it into datafram
feature_length <- adply(.data = data_x, .margins = 1, .fun = function(x){
  length(which(!is.na(x)))
})

# get rid of levels
feature_length<-Filter(is.numeric, feature_length)

str(feature_length)

### Sample Length ###

### Frequency power and phase ###

feature_mag <- adply(.data = data_x, .margins = 1, .fun = function(x){
  Mod(fft(x))
})

# get rid of levels
feature_mag<-Filter(is.numeric, feature_mag)
str(feature_mag)

?matplot
matplot(t(feature_mag), type='h', col=factor(data_x[1,]))

### Frequency power and phase ###


