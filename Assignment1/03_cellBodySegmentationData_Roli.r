###########################################################
#   Assignment 01 Part 3: Cell Body Segmentation Data     #
#                                                         #
#   Author: Roland Spindelbalker                          #
#   Date: 16.11.2018                                      #
###########################################################

# Analyze the data using the same techniques as for the last task. Decide for yourself which
# and how to use the specific commands. Answer the following questions in the report and
# include figures supporting your answers:
#   1. Which classes exist? Are they (roughly) balanced?
#   2. Which noteworthy trends of features and relations between features as well as features
#   and Class do you see?
#   3. If you would need to distinguish between classes, which features do you think would
#   be most helpful? Why?

# Setup - Loading libraries and setting up working directory ###################################
library('ggplot2')
library('lattice')
library('caret')
library('plyr')
getwd()
setwd('/home/rsp/FH_Hagenberg/MCM/3.Semester/MC_520-MachineLearning/Exercises/Exercise01')
getwd()
segDat <- read.table(file = 'segmentationData.csv.gz', sep = ',', header = T)
################################################################################################


####   1. Which classes exist? Are they (roughly) balanced?
str(segDat)
summary(segDat)
barplot(summary(segDat$Class))

# There are two classes, WS and PS, between which there is a 1:2 imbalance (719:1300)


####   2. Which noteworthy trends of features and relations between features as well as features
####   and Class do you see?

# Pairs with all data to get an overview
# Note: I decided to keep the upper panel to compare the two orientations
pairs(segDat[,2:15], pch='.', main="Segmentation Data Overview")#, upper.panel = NULL)
pairs(segDat[,2:15], pch='.', col=segDat$Class, main="Segmentation Data Overview including Classes")#, upper.panel = NULL)

### Trends in features
# ConvexHullAreaRatioCh1 and ConvexHullAreaPerimeterCh1 seem to be inversed in their relation 
# to almost all other features
# This can be seen in the pair plots as well as in the feature plot later on

### Relations between Features (correlation)

# Constant
plot(segDat$AvgIntenCh1 ~ segDat$AngleCh1, main='AngleCh1 vs AvgIntenCh1', xlab='AngleCh1', ylab='AvgIntenCh1')
plot(segDat$AvgIntenCh3 ~ segDat$AngleCh1, main='AngleCh1 vs AvgIntenCh3', xlab='AngleCh1', ylab='AvgIntenCh3')
plot(segDat$DiffIntenDensityCh1 ~ segDat$AngleCh1, main='AngleCh1 vs DiffIntenDensityCh1', xlab='AngleCh1', ylab='DiffIntenDensityCh1')
plot(segDat$NeighborMinDistCh1 ~ segDat$AngleCh1, main='AngleCh1 vs NeighborMinDistCh1', xlab='AngleCh1', ylab='NeighborMinDistCh1')
plot(segDat$SkewIntenCh4 ~ segDat$AngleCh1, main='AngleCh1 vs SkewInternCh4', xlab='AngleCh1', ylab='SkewInternCh4')

# Linear correlation (positive)
plot(segDat$AvgIntenCh1 ~ segDat$DiffIntenDensityCh1, main='DiffIntenDensityCh1 vs AvgIntenCh1', xlab='DiffIntenDensityCh1', ylab='AvgIntenCh1')
plot(segDat$SkewIntenCh4 ~ segDat$IntenCoocMaxCh4, main='IntenCoocMaxCh4 vs SkewIntenCh4', xlab='IntenCoocMaxCh4', ylab='SkewIntenCh4')

# Non-linear correlation (negative)
plot(segDat$AvgIntenCh1 ~ segDat$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh1', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh1')
plot(segDat$AvgIntenCh2 ~ segDat$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh2', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh2')
plot(segDat$AvgIntenCh4 ~ segDat$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh4', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh4')
plot(segDat$AvgIntenCh4 ~ segDat$IntenCoocMaxCh4, main='IntenCoocMaxCh4 vs AvgIntenCh4', xlab='IntenCoocMaxCh4', ylab='AvgIntenCh4')

# Absolutely no correlation
plot(segDat$FiberAlign2Ch3 ~ segDat$AngleCh1, main='AngleCh1 vs FiberAlign2Ch3', xlab='AngleCh1', ylab='FiberAlign2Ch3')

# No correlation
plot(segDat$AvgIntenCh2 ~ segDat$AngleCh1, main='AngleCh1 vs AvgIntenCh2', xlab='AngleCh1', ylab='AvgIntenCh2')
plot(segDat$AreaCh1 ~ segDat$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AreaCh1', xlab='ConvexHullAreaRatioCh1', ylab='AreaCh1')
plot(segDat$AvgIntenCh3 ~ segDat$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh3', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh3')


### Relations between Features and Classes - Detailed

# There seems to be no correlation between the classes and the features if you look at the pair plots from above
# However, this feature plot shows an entirely different picture
featurePlot(x = segDat[,2:15], y=segDat$Class, plot='density', 
            plot.points = F, scales=list(relation='free'), 
            auto.key=list(columns=2),
            main="Feature count by classes")

# It seems that there are at least 5 features that show a really distinct trend for a class
# For Class WS:
#   ConvexHullAreaRatioCh1
#   ConvexHullAreaPerimeterCh1
#   AvgIntenCh2
# For Class PS:
#   AvgIntenCh1
#   IntenCoocMaxCh3
# I'm guessing that those could be used to try to predict the class


####   3. If you would need to distinguish between classes, which features do you think would
####   be most helpful? Why?

# For Class WS:
#   ConvexHullAreaRatioCh1
#   ConvexHullAreaPerimeterCh1
#   AvgIntenCh2
# For Class PS:
#   AvgIntenCh1
#   IntenCoocMaxCh3
# I'm guessing that those could be used to try to predict the classs

featurePlot(x = segDat[,2:15], y=segDat$Class, plot='box', 
            plot.points = F, scales=list(relation='free'), 
            auto.key=list(columns=2),
            main="Feature count by classes")

# WS correlation detailed
plot(segDat$ConvexHullPerimRatioCh1 ~ segDat$AvgIntenCh2, col=segDat$Class, main='AvgIntenCh2 vs ConvexHullPerimRatioCh1 class colorized', xlab='AvgIntenCh2', ylab='ConvexHullPerimRatioCh1')
plot(segDat$ConvexHullAreaRatioCh1 ~ segDat$AvgIntenCh2, col=segDat$Class, main='AvgIntenCh2 vs ConvexHullAreaRatioCh1 class colorized', xlab='AvgIntenCh2', ylab='ConvexHullAreaRatioCh1')
plot(segDat$ConvexHullAreaRatioCh1 ~ segDat$ConvexHullPerimRatioCh1, col=segDat$Class, main='ConvexHullPerimRatioCh1 vs ConvexHullAreaRatioCh1 class colorized', xlab='ConvexHullPerimRatioCh1', ylab='ConvexHullAreaRatioCh1')

# PS correlation detailed
plot(segDat$AvgIntenCh1 ~ segDat$IntenCoocMaxCh3, col=segDat$Class, main='IntenCoocMaxCh3 vs AvgIntenCh1 class colorized', xlab='IntenCoocMaxCh3', ylab='AvgIntenCh1')

# Correlations of the selected features with class coloring detailed
plot(segDat$ConvexHullPerimRatioCh1 ~ segDat$AvgIntenCh1, col=segDat$Class, main='AvgIntenCh1 vs ConvexHullPerimRatioCh1 class colorized', xlab='AvgIntenCh1', ylab='ConvexHullPerimRatioCh1')
plot(segDat$ConvexHullAreaRatioCh1 ~ segDat$AvgIntenCh1, col=segDat$Class, main='AvgIntenCh1 vs ConvexHullAreaRatioCh1 class colorized', xlab='AvgIntenCh1', ylab='ConvexHullAreaRatioCh1')
plot(segDat$AvgIntenCh2 ~ segDat$AvgIntenCh1, col=segDat$Class, main='AvgIntenCh1 vs AvgIntenCh2 class colorized', xlab='AvgIntenCh1', ylab='AvgIntenCh2')

plot(segDat$ConvexHullPerimRatioCh1 ~ segDat$IntenCoocMaxCh3, col=segDat$Class, main='IntenCoocMaxCh3 vs ConvexHullPerimRatioCh1 class colorized', xlab='IntenCoocMaxCh3', ylab='ConvexHullPerimRatioCh1')
plot(segDat$ConvexHullAreaRatioCh1 ~ segDat$IntenCoocMaxCh3, col=segDat$Class, main='IntenCoocMaxCh3 vs ConvexHullAreaRatioCh1 class colorized', xlab='IntenCoocMaxCh3', ylab='ConvexHullAreaRatioCh1')
plot(segDat$AvgIntenCh2 ~ segDat$IntenCoocMaxCh3, col=segDat$Class, main='IntenCoocMaxCh3 vs AvgIntenCh2 class colorized', xlab='IntenCoocMaxCh3', ylab='AvgIntenCh2')

# Other correlations that could be promising
plot(segDat$AvgIntenCh2 ~ segDat$AvgIntenCh3, col=segDat$Class, main='AvgIntenCh3 vs AvgIntenCh2 class colorized', xlab='AvgIntenCh3', ylab='AvgIntenCh2')
plot(segDat$AvgIntenCh3 ~ segDat$DiffIntenDensityCh1, col=segDat$Class, main='DiffIntenDensityCh1 vs AvgIntenCh3 class colorized', xlab='DiffIntenDensityCh1', ylab='AvgIntenCh3')
plot(segDat$AreaCh1 ~ segDat$AvgIntenCh1, col=segDat$Class, main='AvgIntenCh1 vs AreaCh1 class colorized', xlab='AvgIntenCh1', ylab='AreaCh1')
