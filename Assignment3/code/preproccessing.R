

### Remove gravity ###

removeGravity <- function(data){
  aaply(data, .margins = 1, .fun = function(x){
    x - mean(x[!is.na(x)])
  })
}

### Remove gravity ###

### Magnitude ###

generateMagnitude <- function(data_x, data_y, data_z){
  index <- 0
  lab_m <- adply(data_x, .margins = 1, .fun = function(row){
    index <<- index+1
    sqrt(data_x[index,]**2 + data_y[index,] ** 2 + data_z[index,] ** 2)
  })
  lab_m <- lab_m[,-1]
  lab_m
}

### Magnitude ###

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
  print(n)
  aaply(data, .margins = 1, .fun = function(x){
    print(x)
    na_data <- x[!is.na(x)]
    matrixData <- matrix(, nrow = 1, ncol = length(x))
    matrixData[1:length(na_data)] <- sgolayfilt(as.vector(na_data), n = n)
    matrixData
  })
}

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

### Preprocess ###

preprocess <- function(data_x, data_y, data_z){
  data_x <- removeGravity(raw_x)
  data_y <- removeGravity(raw_y)
  data_z <- removeGravity(raw_z)
  data_mag <- generateMagnitude(data_x, data_y, data_z)
  str(data_mag)
  data_mag <- filterWithSGolay(data_mag, 15)
  interpolateData(data_mag)
}

### Preprocess ###

preprocessedData <- preprocess(raw_x)

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

concat_df <- function(data_x, data_y, data_z){
  
  # convert list to dataframe
  mad_x_df <- data.frame(matrix(unlist(data_x), nrow=nrow(data_x) , ncol=ncol(data_x)))
  mad_y_df <- data.frame(matrix(unlist(data_y), nrow=nrow(data_y), ncol=ncol(data_y)))
  mad_z_df <- data.frame(matrix(unlist(data_z), nrow=nrow(data_z), ncol=ncol(data_z)))
  
  # combine general, x, y, z dataframes.
  # structure: gesture;id;sampleNr; 85 columns x-value; 85 columns y-value; 85 columns z-value
  mad_df = data.frame(c(
    general_data, 
    mad_x_df[,2:ncol(data_x)],
    mad_y_df[,2:ncol(data_y)], 
    mad_z_df[,2:ncol(data_z)]
  ))
  
  # resert the column names
  colnames(mad_df) <- c(
    "gesture",
    "person_id",
    "record_nr",
    paste("x_", 1:(ncol(mad_x_df)-1), sep = ""),
    paste("y_", 1:(ncol(mad_y_df)-1), sep = ""),
    paste("z_", 1:(ncol(mad_z_df)-1), sep = "")
  )
  
  return(mad_df)
}

### Create dataframe for model training ###
