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