getwd()
setwd('ENI/MC520/Assignment\_1')

csv_file <- read.csv('segmentationData.csv')
str(csv_file)
is.factor(csv_file$Class)

# task 1
summary(csv_file$Class)

# task 2
pairs(csv_file, pch = '.', upper.panel = NULL, col=csv_file$Class)
plot(csv_file[,c(6,8)], pch = '.', xlab = 'AvgIntenCh3', ylab = 'ConvexHullAreaRationCh1', col=csv_file$Class)
plot(csv_file[,c(6,9)], pch = '.', xlab = 'AvgIntenCh3', ylab = 'ConvexHullPerimRationCh1', col=csv_file$Class)

