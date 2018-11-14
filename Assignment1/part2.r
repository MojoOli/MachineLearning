setwd("D:/FHHagenberg/Master/S3/ML/Assignments/Assignment1")
library(ggplot2)
data(diamonds)

# part 1
?diamonds

str(diamonds)
max(summary(diamonds$color)) / min(summary(diamonds$color))
max(summary(diamonds$cut)) / min(summary(diamonds$cut))
max(summary(diamonds$clarity)) / min(summary(diamonds$clarity))

# part 3
summary(diamonds$price)
sd(diamonds$price)
mad(diamonds$price)

#part 5
pairs(diamonds[,7:10], upper.panel = NULL, pch='.')

#part 7
dim(diamonds[diamonds$price > 9500.0,])
dim(diamonds[diamonds$price > 9500.0 & diamonds$color == "D",])

diamonds_filtered1 = diamonds[diamonds$color == "D" & diamonds$cut == "Fair",]
summary(diamonds_filtered1$price)
summary(diamonds_filtered1$carat)

diamonds_filtered2 = diamonds[diamonds$color == "J" & diamonds$cut == "Ideal",]
summary(diamonds_filtered2$price)
summary(diamonds_filtered2$carat)
