library(ggplot2)
data(diamonds)

# part 1
?diamonds

str(diamonds)
summary(diamonds$color)
plot(diamonds$color)
summary(diamonds$cut)
summary(diamonds$clarity)

# part 3
summary(diamonds$price)
sd(diamonds$price)
mad(diamonds$price)

#part 5
