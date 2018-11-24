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
install.packages("doMC")
library(corrplot)
library(png)
library(plyr)
library(imager)
library(caret) # caret ML framework
library(doMC) # parallelization
registerDoMC(8) # register 8 cores (more cores require more RAM)
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

####### Edge Detection

# convert data to matrix to be able to use plyr without variable grouping 
# and to calculate the imgradient for every picture

gMagData <- alply(as.matrix(imgData[,2:2501]), .margins=1, .fun=function(x){
  cImgData <- as.cimg(matrix(data = x, nrow = 50, ncol = 50))
  imgradient(cImgData,"xy") %>% enorm
})

# show difference in images
normalImage <- as.cimg(matrix(data = unlist(imgData[1,2:2501]), nrow = 50, ncol = 50))
sidePlot <- as.imlist(list(grayscale=normalImage, "gradient magnitude" =gMagData[[1]]))
plot(sidePlot, layout="row", xlab="pixel", ylab="pixel")

# threshold(gMagData[[1]],"60%") %>% plot(main="Determinant: 40% highest values")

# convert to data frame
# threshold where only the 40% highest values will be taken into account
gMagDataFrame <- ldply(gMagData, .fun=function(x){
  base<-x[,,1,1]
  as.vector(base)
  # as.numeric(threshold(base,"60%"))
})
# get rid of levels
gMagDataFrame<-Filter(is.numeric, gMagDataFrame)

#######

### Sliding window approach
library(zoo)
swImgDataTransposed <- rollapply(data = t(imgData[,2:2501]), width = 5, by=5, FUN = median)
swImgData = as.data.frame(t(swImgDataTransposed))
dim(swImgData)
###

# Convert the person number into a "normalized" string by adding P and ensuring it is always (at least) 3 characters long
# Then convert it to a factor (a class label)
imgData[,1] <- as.factor(sprintf("P%02d", imgData[,1]))
levels(imgData$`1`)
summary(imgData$`1`)
### 

### Draw correlation plots for the first couple of features
corrplot(cor(imgData[,2:50]))
corrplot(cor(imgData[,1001:1101]))
corrplot(cor(t(imgData[1:60,1201:1301])))
###

### Training + Setup
models <- list()
trControl <- trainControl(method = 'repeatedcv', 
                          number = 10, 
                          repeats = 20, 
                          returnData = F, 
                          classProbs = T, 
                          returnResamp = 'final', 
                          allowParallel = T)


## LDA Model 
# Preprocessing: centering, scaling and PCA 
# Partitioning: 10CV Repeated 20 times
names(getModelInfo('lda')) # linear discriminant analysis
getModelInfo('lda')[[1]]$parameters # this model does not have any hyperparameters
?train
models$modelLda <- train(x = gMagDataFrame,
                         y = imgData[,1],
                         preProcess = c('center', 'scale', 'pca'), 
                         method = 'lda', 
                         tuneGrid = NULL, 
                         metric = 'Kappa', 
                         trControl = trControl)
models$modelLda
cvConfMatrix <- confusionMatrix(models$modelLda)
cvConfMatrix
levelplot(sweep(x = cvConfMatrix$table, STATS = colSums(cvConfMatrix$table), MARGIN = 2, FUN = '/'), col.regions=gray(100:0/100))
##

###