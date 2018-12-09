library(signal) # needed for Savitzky-Golay filter
library(zoo) # for sliding window
library(caret) # for data partition
library(flux)
library(magrittr) #needed for pipes
library(plyr) #for vectorized commands
library(dplyr) #needed for filter
library(scales)
library(corrplot)
library(doMC) # parallelization

#installer (optional)
install.packages("signal")
install.packages("flux")
install.packages("corrplot")
install.packages("doMC")
install.packages("caret",dependencies = TRUE)
