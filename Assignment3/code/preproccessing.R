library(signal) # needed for Savitzky-Golay filter

### Remove gravity ###

removeGravity <- function(data){
  aaply(data, .margins = 1, .fun = function(x){
    x - mean(x[!is.na(x)])
  })
}

### Remove gravity ###

### Filter noise ###

filterWithRunMed <- function(data, k, endrule = "median"){
  aaply(data, .margins = 1, .fun = function(x){
    na_data <- x[!is.na(x)]
    matrixData <- matrix(, nrow = 1, ncol = length(x))
    matrixData[1:length(na_data)] <- runmed(as.vector(na_data), k, endrule)
    matrixData
  })
}

filterWithSGolay <- function(data, n){
  aaply(data, .margins = 1, .fun = function(x){
    na_data <- x[!is.na(x)]
    matrixData <- matrix(, nrow = 1, ncol = length(x))
    matrixData[1:length(na_data)] <- sgolayfilt(as.vector(na_data), n = n)
    matrixData
  })
}

# plotting
filtered_med_x <- filterWithRunMed(raw_x, 5)
filtered_sg_x <- filterWithSGolay(raw_x, 11)

plot_data_raw <- raw_x[15,]
plot_data_raw <- plot_data_raw[!is.na(plot_data_raw)]

plot_data_med <- filtered_med_x[15,]
plot_data_med <- plot_data_med[!is.na(plot_data_med)]

plot_data_sg <- filtered_sg_x[15,]
plot_data_sg <- plot_data_sg[!is.na(plot_data_sg)]

# filtered_sg_x <- getFeatureMagnitude(filtered_sg_x)

matplot(data.frame(plot_data_raw, plot_data_sg), type='l')
# matplot(t(filtered_sg_x[1,]), type='l')

### Filter noise ###

### Interpolation ###

interpolateData <- function(data){
  stepwidth <- 0.002 # we want 500 values
  interpolated <- aaply(data, .margins = 1, .fun = function(x){
    x <- x[!is.na(x)]
    approx(x = seq(0,1,1/(length(x)-1)), y = x, xout = seq(0,1,stepwidth))$y
  })
  interpolated[,-501]
}

### Interpolation ###