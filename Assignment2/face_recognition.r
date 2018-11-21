###########################################################################################
#                           Assignment 02: Face Recognition                               #
#                                                                                         #
# • Goal is to build (a) successful face recognition model(s). The model should learn to  #
#   distinguish between N people from face images, then being able to decide for new      #
#   face images which of the N participants shown. Load the data, derive features and     #
#   evaluate different models. Try to achieve the best possible performance by changing   #
#   your feature derivation (always use proper data partitioning):                        #
#     – Take some of the original pixel features of your data that lie new to each other  #
#       (e.g. first 100) and do a correlation plot for them. What do you see?             #
#     – Feature derivation likely is a multi-step-process. You can use other feature      #
#       derivation approaches than those discussed in the lecture too – there are many    #
#       of them. Use “whatever floats your boat”, but be sure to understand your          #
#       own toolchain. E.g. try changing details, like differently sized input images,    #
#       more/less/different features, and compare differences in results.                 #
#     – What is the best approach (features, model, . . . ) you can come up with to       #
#       successfully distinguish people? What do you think of the data/the results?       #
#       Chose appropriate metrics to underline your statements.                           #
# • After solving the above points, answer this question: what happens if we build our    #
#   model from such a data set, then somebody not part of it uses a system where the      #
#   model is deployed? What could you do about it/how could such systems possibly         #
#   work?                                                                                 #
###########################################################################################

### Libraries and workspace setup
library(corrplot)
library(png)
library(plyr)
library(caret) # caret ML framework
library(doMC) # parallelization
registerDoMC(3) # register 3 cores (more cores require more RAM)

setwd('/home/rsp/FH_Hagenberg/MCM/3.Semester/MC_520-MachineLearning/Exercises/Exercise_FaceRecognition/')
###

### Import data
# Import data from the images and add the person number as the first column
imgData <- ldply(dir(path = 'dsr-preprocessed-1000x1333_faces_haar_gray_resized150_equalized_50x50/', 
                     full.names= T, pattern = 'png'), 
                 function(f) {
                   t(
                     c(
                       as.numeric(strsplit(f, '_')[[1]][8]),
                       as.numeric(readPNG(f))))
                   });
# Convert the person number into a "normalized" string by adding P and ensuring it is always (at least) 3 characters long
# Then convert it to a factor (a class label)
imgData[,1] <- as.factor(sprintf("P%02d", imgData[,1]))
levels(imgData$`1`)
str(imgData)
### 

### Draw correlation plots for the first couple of features
corrplot(cor(imgData[,2:450])) # 16 pixles
corrplot(cor(imgData[,2:50])) # 1st pixel
corrplot(cor(imgData[,1001:1200])) # 200 pixels from the middle
corrplot(cor(imgData[,1201:1500])) # 300 pixel from the middle
###

# ldply(dir(path = 'dsr-preprocessed-1000x1333_faces_haar_gray_resized150_equalized_50x50/', full.names= T, pattern = 'png'), function(f) strsplit(print(f), "_")[20])
# testfile <- dir(path = 'dsr-preprocessed-1000x1333_faces_haar_gray_resized150_equalized_50x50/', full.names= T, pattern = 'png')[21]
# person <- strsplit(testfile, '_')[[1]][8]
# personTable <- t(c(as.factor(paste('P', strsplit(testfile, '_')[[1]][8], sep = "")), 1:10))
# str(personTable)