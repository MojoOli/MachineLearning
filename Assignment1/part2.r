library(ggplot2)
library(lattice)
library(caret)
library(plyr)

data(diamonds)

#task 1
?diamonds

str(diamonds)
max(summary(diamonds$color)) / min(summary(diamonds$color))
max(summary(diamonds$cut)) / min(summary(diamonds$cut))
max(summary(diamonds$clarity)) / min(summary(diamonds$clarity))

#task 2
qplot(diamonds$price)
boxplot(diamonds$price)
densityplot(diamonds$price, plot.points = F)

#task 3
summary(diamonds$price)
sd(diamonds$price)
mad(diamonds$price)

#task 4
plot(diamonds$price,diamonds$carat,pch = ".")

#task 5
pairs(diamonds[,7:10], upper.panel = NULL, pch='.')

#task 6
boxplot(diamonds$price~diamonds$color)
featurePlot(diamonds$price, diamonds$color,col=diamonds$color, plot = 'density', plot.points = F)

#task 7
dim(diamonds[diamonds$price > 9500.0,])
dim(diamonds[diamonds$price > 9500.0 & diamonds$color == "D",])

diamonds_filtered1 = diamonds[diamonds$color == "D" & diamonds$cut == "Fair",]
summary(diamonds_filtered1$price)
summary(diamonds_filtered1$carat)

diamonds_filtered2 = diamonds[diamonds$color == "J" & diamonds$cut == "Ideal",]
summary(diamonds_filtered2$price)
summary(diamonds_filtered2$carat)

#task 8
numerics<-Filter(is.numeric, diamonds)
aaply(.data = as.matrix(numerics), .margins = 2, .fun = mean)
aaply(.data = as.matrix(numerics), .margins = 2, .fun = median)
aaply(.data = as.matrix(numerics), .margins = 2, .fun = sd)
aaply(.data = as.matrix(numerics), .margins = 2, .fun = function(col){
  mad(col,constant = 1)
})



