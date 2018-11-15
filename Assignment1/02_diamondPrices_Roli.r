#############################################
#   Assignment 01 Part 2: Diamond Prices    #
#                                           #
#   Author: Roland Spindelbalker            #
#   Date: 15.11.2018                        #
#############################################

# Load library and data
library('ggplot2')
library('lattice')
library('caret')
library('plyr')
data("diamonds")

# Look through dataset
?diamonds

# 1. Give an overview of the dataset structure:
#   • How many samples and features are in the dataset?
#   • What are the feature data types?
#   • Are diamonds balanced across color, cut and clarity? 
#   (Hint: roughly 1:1 means balanced, e.g. 1:2 is a “1:2 imbalance”)

str(diamonds)
# After executing str on the dataset, we see that there are
# 53940 samples with 10 features in the dataset

# The datatypes of the features are:
# $ carat  : num                      <- Numeric  
# $ cut    : Ord.factor w/ 5 levels   <- Factor with 5 different levels
# $ color  : Ord.factor w/ 7 levels   <- Factor with 7 different levels
# $ clarity: Ord.factor w/ 8 levels   <- Factor with 8 different levels
# $ depth  : num                      <- Numeric
# $ table  : num                      <- Numeric
# $ price  : int                      <- Integer
# $ x      : num                      <- Numeric
# $ y      : num                      <- Numeric
# $ z      : num                      <- Numeric

# To find out if the diamods are balanced, accross the features, we need
# to use
summary(diamonds$cut)
summary(diamonds$color)
summary(diamonds$clarity)

# There we see that the features are imbalanced
# This can be seen even better in a plot
barplot(summary(diamonds$cut))
barplot(summary(diamonds$color))
barplot(summary(diamonds$clarity))


# 2. Visualize diamond prices using a histogram, boxplot and densityplot:
#   • Is there a visible trend? If yes, which is it and from which plots can you derive it?

hist(diamonds$price, main = 'Diamonds Price', xlab = "Price values", ylab = "Amount of diamonds")
boxplot(diamonds$price, horizontal = T, main = 'Diamonds Price', xlab = "Price values")
densityplot(diamonds$price, plot.points = F, main = 'Diamonds Price', xlab = "Price")

# There is a visible trend, namely that the amount of diamonds decreases with rising prices.
# This trend is reflected in all three visualizations, but not easy to see in all of them.
# In my opinion, the histogram is the clearest visualization of the three.
# There you can see that the amount decreases exponentially with increasing prices.
# Additionally, you can see that there is a irregular jump in the price range between 4000 and 5000


# 3. Calculate and state the mean, median, standard deviation, median absolute deviation
#    (MAD), 1st and 3rd quantile, and inner quantile range of the diamond price:
#      • If you are not familiar with those functions, try Google, Wikipedia, . . .
#      • Hint: summary() will be useful.

# Minimum, 1st Quartile, Median, Mean, 3rd Quartile, Maximum 
summary(diamonds$price)
# Standard deviation
sd(diamonds$price)
# Median absolute deviation
mad(diamonds$price)
# Inner quartile range
IQR(diamonds$price)


# 4. Plot the diamond price against the carat values as an xy-plot:
#   • Is there a trend visible in the plot? If yes, which is it?
#   • Hint: plotting many samples will be slow. Changing the plot symbol to '.' will
#     cause a speedup.

plot(x = diamonds$price, y = diamonds$carat, pch='.', main='Carat vs. Diamond price', xlab='Price', ylab='Carat')
plot(x = diamonds$carat, y = diamonds$price, pch='.', main='Carat vs. Diamond price', xlab='Carat', ylab='Price')

# The second plot, where the carat are on the x axis, shows the trend well.
# Even though there are clear segmentations in the data (in the carat values), the price rises exponentially with
# rising carat values


# 5. Analyze the correlation between diamond price and diamond x, y, and z dimensions:
#   • Create pairwise plots for these features.
#   • Is there a trend visible between x, y, and z? If yes, which is it?
#   • Is there a trend visible between the dimensions and the price? If yes, which is it?
#   • Hint: remember what a linear correlation between 2 variables (=features) looks
#     like:
#     – Linear correlation: feature A low –> feature B low, and feature A high –>
#       feature B high.
#     – (Inverse) linear correlation: feature A low –> feature B high, and feature A
#       high –> feature B low: inverse linear correlation. Usually also just called
#       linear correlation.
#     – When plotting feature A against feature B, both will cause a “straight line”
#       (+ some noise = scatter). This means: if you see straight lines, there is a
#       linear relation between features.

pairs(diamonds[,7:10], pch='.', main='X vs Y vs Z vs Price of Diamonds')

# The correlation between x, y and z is clearly a linear one. 
# The correlation between x,y,z and the price seems to be exponential.


# 6. Analyze diamond prices per diamond color:
#   • Create boxplots showing diamond price boxes for each diamond color. Use a single
#     plot command to obtain this plot (hint: use the formula interface of boxplot with a “price ~ color” formula)
#   • Create densityplots showing diamond prices for each diamond color (hint: featurePlot).
#   • Is there a trend visible? If yes, which is it?

boxplot(diamonds$price ~ diamonds$color)
featurePlot(x = diamonds$price, y=diamonds$color, plot='density', plot.points = F, scales=list(relation='free'), no.legend = F)

# For all diamonds, there are many more cheap ones than expensive ones. This is also true for the colors.
# For some diamond colors, there are more cheaper ones and less expensive ones than for others.


# 7. Use vectorized commands to answer these questions:
#   • How many diamonds have a price above 9500?
#   • How many diamonds have a price above 9500 and have color “D”?
#   • For all color “D” diamonds with cut “Fair” show the summary for price and carat
#   • For all color “J” diamonds with cut “Ideal” show the summary for price and carat

count(diamonds$price > 9500) # 5734
count(diamonds$price > 9500 & diamonds$color == "D") # 461
summary(diamonds[diamonds$color == "D" & diamonds$cut == "Fair",c(1,7)])
# carat            price      
# Min.   :0.2500   Min.   :  536  
# 1st Qu.:0.7000   1st Qu.: 2204  
# Median :0.9000   Median : 3730  
# Mean   :0.9201   Mean   : 4291  
# 3rd Qu.:1.0100   3rd Qu.: 4797  
# Max.   :3.4000   Max.   :16386 

summary(diamonds[diamonds$color == "J" & diamonds$cut == "Ideal",c(1,7)])
# carat           price      
# Min.   :0.230   Min.   :  340  
# 1st Qu.:0.540   1st Qu.: 1132  
# Median :1.030   Median : 4096  
# Mean   :1.064   Mean   : 4918  
# 3rd Qu.:1.410   3rd Qu.: 6732  
# Max.   :3.010   Max.   :18508 


# 8. Utilize functions from the plyr library or the standard apply function on the diamonds
#    data.frame to compute the following:
#   1. Calculate the mean (?mean) for all numerics features using a single command.
#   2. Do the same with median (?median).
#   3. Do the same with standard deviation (?sd).
#   4. Do the same with median absolute deviation (?mad) and set the constant parameter
#      of the mad function to 1.
#   5. Hint: usually, you would compute the first 2 using the functions colMeans and
#      summary, but for the purpose of applying arbitrary function on data.frames, don’t
#      use these functions for this task.

apply(X = diamonds[laply(.data = as.list(diamonds[1,]), .fun = is.numeric)], MARGIN=2, FUN=mean)
apply(X = diamonds[laply(.data = as.list(diamonds[1,]), .fun = is.numeric)], MARGIN=2, FUN=median)
apply(X = diamonds[laply(.data = as.list(diamonds[1,]), .fun = is.numeric)], MARGIN=2, FUN=sd)
apply(X = diamonds[laply(.data = as.list(diamonds[1,]), .fun = is.numeric)], MARGIN=2, FUN= function(col) {
  mad(x = col, constant = 1)
})

