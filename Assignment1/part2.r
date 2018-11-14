library(ggplot2)
library(lattice)
library(caret)
library(plyr)

data(diamonds)

# part 1
?diamonds

str(diamonds)
summary(diamonds$color)
plot(diamonds$color)
summary(diamonds$cut)
summary(diamonds$clarity)

# part 2
qplot(diamonds$price)
boxplot(diamonds$price)
densityplot(diamonds$price, plot.points = F)

# part 3
summary(diamonds$price)
sd(diamonds$price)
mad(diamonds$price)

# part 4
plot(diamonds$price,diamonds$carat,pch = ".")

# part 5

# part 6
boxplot(diamonds$price~diamonds$color)
featurePlot(diamonds$price, diamonds$color,col=diamonds$color, plot = 'density', plot.points = F)
?densityplot

str(diamonds)
numerics<-Filter(is.numeric, diamonds)
# part 8
aaply(.data = as.matrix(numerics), .margins = 2, .fun = mean)
aaply(.data = as.matrix(numerics), .margins = 2, .fun = median)
aaply(.data = as.matrix(numerics), .margins = 2, .fun = sd)
aaply(.data = as.matrix(numerics), .margins = 2, .fun = function(col){
  mad(col,constant = 1)
})
