library(magrittr) #needed for pipes
library(dplyr) #needed for filter

#load data
x<-read.csv("../data/raw_data_wear_x.csv")
y<-read.csv("../data/raw_data_wear_y.csv") 
z<-read.csv("../data/raw_data_wear_z.csv")

#change names to something useful
naming<-seq(1:427)
naming[1:3]<-c("gesture","participant","recording") 
names(x) <-naming
names(y) <-naming
names(z) <-naming

#select gesture (and participant)
subset_x<-x %>% filter(gesture == 'left') #%>% filter(participant==0)
subset_y<-z %>% filter(gesture == 'left') #%>% filter(participant==0)
subset_z<-y %>% filter(gesture == 'left') #%>% filter(participant==0)

#drop NA collumns
subset_x <- subset_x[,colSums(is.na(subset_x))<nrow(subset_x)] 
subset_y <- subset_y[,colSums(is.na(subset_y))<nrow(subset_y)]
subset_z <- subset_z[,colSums(is.na(subset_z))<nrow(subset_z)]

#replace remaining NA with zeros
subset_x[is.na(subset_x)] <- 0
subset_y[is.na(subset_y)] <- 0
subset_z[is.na(subset_z)] <- 0

#calculate new length of rows
len_x=dim(subset_x)[2]
len_y=dim(subset_y)[2]
len_z=dim(subset_z)[2]

#convert dataframe to matrix
data_x<-data.matrix(subset_x[,4:len_x])
data_y<-data.matrix(subset_y[,4:len_x])
data_z<-data.matrix(subset_z[,4:len_x])

#utility function for plotting 
plot_axis=function(dataset){
  color<-rgb(0,0,0,alpha=0.3)
  plot(dataset[1,],type = 'l',col=color,ylim=range(min(dataset),max(dataset)),xlab = "sample",ylab = "acceleration")
  
  for (i in seq(2:dim(dataset)[1])){
    lines(data_x[i,],col=color)
  }
}


plot_axis(data_x)
plot_axis(data_y)
plot_axis(data_z)
 


