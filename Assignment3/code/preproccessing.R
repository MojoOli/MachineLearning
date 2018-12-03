library(signal) # needed for Savitzky-Golay filter
library(zoo) # for sliding window

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

### Create dataframe for model training ###
create_df <- function(data_x, data_y, data_z, func, sw_size){
  # reduce dimensions
  mad_x <- slidingWindow(drop_nas(raw_x), func, sw_size)
  mad_y <- slidingWindow(drop_nas(raw_y), func, sw_size)
  mad_z <- slidingWindow(drop_nas(raw_z), func, sw_size)
  
  # convert list to dataframe
  mad_x_df <- data.frame(matrix(unlist(mad_x), nrow=nrow(mad_x) , ncol=ncol(mad_x)))
  mad_y_df <- data.frame(matrix(unlist(mad_y), nrow=nrow(mad_y), ncol=ncol(mad_y)))
  mad_z_df <- data.frame(matrix(unlist(mad_z), nrow=nrow(mad_z), ncol=ncol(mad_z)))
  
  # calculate area under the curve for all axis
  auc_xyz <- calc_auc(data_x, data_y, data_z)
  
  # combine general, x, y, z dataframes.
  # structure: gesture;id;sampleNr; 85 columns x-value; 85 columns y-value; 85 columns z-value
  mad_df = data.frame(c(
    general_data, 
    auc_xyz,
    mad_x_df[,2:ncol(mad_x)],
    mad_y_df[,2:ncol(mad_y)], 
    mad_z_df[,2:ncol(mad_z)]
  ))
  
  # resert the column names
  colnames(mad_df) <- c(
    "gesture",
    "person_id",
    "record_nr",
    "auc_x",
    "auc_y",
    "auc_z",
    paste("x_", 1:(ncol(mad_x_df)-1), sep = ""),
    paste("y_", 1:(ncol(mad_y_df)-1), sep = ""),
    paste("z_", 1:(ncol(mad_z_df)-1), sep = "")
  )
  
  return(mad_df)
}

### Create dataframe for model training ###
