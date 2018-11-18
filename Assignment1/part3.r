library('ggplot2')
library('lattice')
library('caret')
library('plyr')

getwd()
setwd('ENI/MC520/Assignment\_1')

csv_file <- read.csv('segmentationData.csv')
str(csv_file)
is.factor(csv_file$Class)
barplot(summary(csv_file$Class))

# task 1
summary(csv_file$Class)

# task 2
pairs(csv_file[2:15], pch = '.', upper.panel = NULL, col=csv_file$Class)

### Relations between Features (correlation)

# Constant
plot(csv_file$AvgIntenCh1 ~ csv_file$AngleCh1, main='AngleCh1 vs AvgIntenCh1', xlab='AngleCh1', ylab='AvgIntenCh1', col=csv_file$Class)
plot(csv_file$AvgIntenCh3 ~ csv_file$AngleCh1, main='AngleCh1 vs AvgIntenCh3', xlab='AngleCh1', ylab='AvgIntenCh3', col=csv_file$Class)
plot(csv_file$DiffIntenDensityCh1 ~ csv_file$AngleCh1, main='AngleCh1 vs DiffIntenDensityCh1', xlab='AngleCh1', ylab='DiffIntenDensityCh1', col=csv_file$Class)
plot(csv_file$NeighborMinDistCh1 ~ csv_file$AngleCh1, main='AngleCh1 vs NeighborMinDistCh1', xlab='AngleCh1', ylab='NeighborMinDistCh1', col=csv_file$Class)
plot(csv_file$SkewIntenCh4 ~ csv_file$AngleCh1, main='AngleCh1 vs SkewInternCh4', xlab='AngleCh1', ylab='SkewInternCh4', col=csv_file$Class)

# Linear correlation (positive)
plot(csv_file$AvgIntenCh1 ~ csv_file$DiffIntenDensityCh1, main='DiffIntenDensityCh1 vs AvgIntenCh1', xlab='DiffIntenDensityCh1', ylab='AvgIntenCh1', col=csv_file$Class)
plot(csv_file$SkewIntenCh4 ~ csv_file$IntenCoocMaxCh4, main='IntenCoocMaxCh4 vs SkewIntenCh4', xlab='IntenCoocMaxCh4', ylab='SkewIntenCh4', col=csv_file$Class)
plot(csv_file$SkewIntenCh4 ~ csv_file$AvgIntenCh4, main='IntenCoocMaxCh4 vs SkewIntenCh4', xlab='IntenCoocMaxCh4', ylab='SkewIntenCh4', col=csv_file$Class)


# Non-linear correlation (negative)
plot(csv_file$AvgIntenCh1 ~ csv_file$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh1', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh1', col=csv_file$Class)
plot(csv_file$AvgIntenCh2 ~ csv_file$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh2', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh2', col=csv_file$Class)
plot(csv_file$AvgIntenCh4 ~ csv_file$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh4', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh4', col=csv_file$Class)
plot(csv_file$AvgIntenCh4 ~ csv_file$IntenCoocMaxCh4, main='IntenCoocMaxCh4 vs AvgIntenCh4', xlab='IntenCoocMaxCh4', ylab='AvgIntenCh4', col=csv_file$Class)

# Absolutely no correlation
plot(csv_file$FiberAlign2Ch3 ~ csv_file$AngleCh1, main='AngleCh1 vs FiberAlign2Ch3', xlab='AngleCh1', ylab='FiberAlign2Ch3', col=csv_file$Class)

# No correlation
plot(csv_file$AvgIntenCh2 ~ csv_file$AngleCh1, main='AngleCh1 vs AvgIntenCh2', xlab='AngleCh1', ylab='AvgIntenCh2', col=csv_file$Class)
plot(csv_file$AreaCh1 ~ csv_file$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AreaCh1', xlab='ConvexHullAreaRatioCh1', ylab='AreaCh1', col=csv_file$Class)
plot(csv_file$AvgIntenCh3 ~ csv_file$ConvexHullAreaRatioCh1, main='ConvexHullAreaRatioCh1 vs AvgIntenCh3', xlab='ConvexHullAreaRatioCh1', ylab='AvgIntenCh3', col=csv_file$Class)

# task 3
featurePlot(x = csv_file[,2:15], y=csv_file$Class, plot='box', 
            plot.points = F, scales=list(relation='free'), 
            auto.key=list(columns=2),
            main="Feature count by classes")
# WS correlation detailed
plot(csv_file$ConvexHullPerimRatioCh1 ~ csv_file$AvgIntenCh2, col=csv_file$Class, main='AvgIntenCh2 vs ConvexHullPerimRatioCh1 class colorized', xlab='AvgIntenCh2', ylab='ConvexHullPerimRatioCh1')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')
plot(csv_file$ConvexHullAreaRatioCh1 ~ csv_file$AvgIntenCh2, col=csv_file$Class, main='AvgIntenCh2 vs ConvexHullAreaRatioCh1 class colorized', xlab='AvgIntenCh2', ylab='ConvexHullAreaRatioCh1')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')
plot(csv_file$ConvexHullAreaRatioCh1 ~ csv_file$ConvexHullPerimRatioCh1, col=csv_file$Class, main='ConvexHullPerimRatioCh1 vs ConvexHullAreaRatioCh1 class colorized', xlab='ConvexHullPerimRatioCh1', ylab='ConvexHullAreaRatioCh1')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')

# PS correlation detailed
plot(csv_file$AvgIntenCh1 ~ csv_file$IntenCoocMaxCh3, col=csv_file$Class, main='IntenCoocMaxCh3 vs AvgIntenCh1 class colorized', xlab='IntenCoocMaxCh3', ylab='AvgIntenCh1')

# Correlations of the selected features with class coloring detailed
plot(csv_file$ConvexHullPerimRatioCh1 ~ csv_file$AvgIntenCh1, col=csv_file$Class, main='AvgIntenCh1 vs ConvexHullPerimRatioCh1 class colorized', xlab='AvgIntenCh1', ylab='ConvexHullPerimRatioCh1')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')
plot(csv_file$ConvexHullAreaRatioCh1 ~ csv_file$AvgIntenCh1, col=csv_file$Class, main='AvgIntenCh1 vs ConvexHullAreaRatioCh1 class colorized', xlab='AvgIntenCh1', ylab='ConvexHullAreaRatioCh1')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')
plot(csv_file$AvgIntenCh2 ~ csv_file$AvgIntenCh1, col=csv_file$Class, main='AvgIntenCh1 vs AvgIntenCh2 class colorized', xlab='AvgIntenCh1', ylab='AvgIntenCh2')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')

plot(csv_file$ConvexHullPerimRatioCh1 ~ csv_file$IntenCoocMaxCh3, col=csv_file$Class, main='IntenCoocMaxCh3 vs ConvexHullPerimRatioCh1 class colorized', xlab='IntenCoocMaxCh3', ylab='ConvexHullPerimRatioCh1')
plot(csv_file$ConvexHullAreaRatioCh1 ~ csv_file$IntenCoocMaxCh3, col=csv_file$Class, main='IntenCoocMaxCh3 vs ConvexHullAreaRatioCh1 class colorized', xlab='IntenCoocMaxCh3', ylab='ConvexHullAreaRatioCh1')
plot(csv_file$AvgIntenCh2 ~ csv_file$IntenCoocMaxCh3, col=csv_file$Class, main='IntenCoocMaxCh3 vs AvgIntenCh2 class colorized', xlab='IntenCoocMaxCh3', ylab='AvgIntenCh2')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')

# Other correlations that could be promising
plot(csv_file$AvgIntenCh2 ~ csv_file$AvgIntenCh3, col=csv_file$Class, main='AvgIntenCh3 vs AvgIntenCh2 class colorized', xlab='AvgIntenCh3', ylab='AvgIntenCh2')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')
plot(csv_file$AvgIntenCh3 ~ csv_file$DiffIntenDensityCh1, col=csv_file$Class, main='DiffIntenDensityCh1 vs AvgIntenCh3 class colorized', xlab='DiffIntenDensityCh1', ylab='AvgIntenCh3')
plot(csv_file$AreaCh1 ~ csv_file$AvgIntenCh1, col=csv_file$Class, main='AvgIntenCh1 vs AreaCh1 class colorized', xlab='AvgIntenCh1', ylab='AreaCh1')
legend("topright", legend = levels(csv_file$Class), col=unique(csv_file$Class), pch='o')