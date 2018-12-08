### Plots
# + correlation of signals (x/y/z/m) ✓
# + filtered vs non signal plot
# + model parameter boxplots
# + interpolated vs normal signal plot ✓
###
registerDoMC(4) # register 3 cores (more cores require more RAM)

#### Plots from other scripts
feature_length <- getFeatureLength(as.matrix(raw_x))
feature_length$gestures <- lab_x$gesture
boxplot(feature_length$V1~feature_length$gesture)

feature_magnitude = getFeatureMagnitude(raw_x)
barplot(as.vector(t(feature_magnitude[1,])), ylab = 'Frequency power', xlab = 'Frequency', main = 'Frequency power spectrum')

feature_phase = getFeaturePhase(raw_x)
barplot(as.vector(t(feature_phase[1,])), ylab = 'Frequency phase', xlab = 'Frequency', main = 'Frequency phase spectrum')
####

#### Preparation 

### Interpolate data (using function from preprocessing.R)
interpolated_x <- interpolateData(raw_x)
interpolated_y <- interpolateData(raw_y)
interpolated_z <- interpolateData(raw_z)
interpolated_m <- interpolateData(raw_m)
###

####

#### Interpolated vs normal signal
plotInterpolatedVsRawData <- function(rawData, interpolatedData, index){
  matplot((data.frame(rawData[index,], interpolatedData[index,])), type='l')
}

# Single signal
plotInterpolatedVsRawData(raw_x, interpolated_x, 200)
plotInterpolatedVsRawData(raw_y, interpolated_y, 200)
plotInterpolatedVsRawData(raw_z, interpolated_z, 200)
plotInterpolatedVsRawData(raw_m, interpolated_m, 200)

# All signals
matplot(t(raw_x), type='l', col= alpha(colour = 1, 0.03))
matplot(t(interpolated_x), type='l', col= alpha(colour = 1, 0.03))

matplot(t(raw_y), type='l', col= alpha(colour = 1, 0.03))
matplot(t(interpolated_y), type='l', col= alpha(colour = 1, 0.03))

matplot(t(raw_z), type='l', col= alpha(colour = 1, 0.03))
matplot(t(interpolated_z), type='l', col= alpha(colour = 1, 0.03))

matplot(t(raw_m), type='l', col= alpha(colour = 1, 0.03))
matplot(t(interpolated_m), type='l', col= alpha(colour = 1, 0.03))
####

#### Correlation of signals

### Correlation of x
corrplot(cor(interpolated_x[,1:200]))
corrplot(cor(interpolated_x[,201:400]))
corrplot(cor(interpolated_x[,301:500]))
### 

### Correlation of y
corrplot(cor(interpolated_y[,1:200]))
corrplot(cor(interpolated_y[,201:400]))
corrplot(cor(interpolated_y[,301:500]))
### 

### Correlation of z
corrplot(cor(interpolated_z[,1:200]))
corrplot(cor(interpolated_z[,201:400]))
corrplot(cor(interpolated_z[,301:500]))
### 

### Correlation of m
corrplot(cor(interpolated_m[,1:200]))
corrplot(cor(interpolated_m[,201:400]))
corrplot(cor(interpolated_m[,301:500]))
### 

####