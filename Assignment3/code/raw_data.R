library(magrittr) #needed for pipes
library(plyr) #for vectorized commands
library(dplyr) #needed for filter
library(scales)

#load data
files=c("../data/raw_data_wear_x.csv","../data/raw_data_wear_y.csv","../data/raw_data_wear_z.csv")
data=lapply(files, read.table, sep=',', fill = T, col.names = c('gesture', 'person', 'record', paste('acc', 1:500, sep='')))

lab_x<-data[[1]]
lab_y<-data[[2]]
lab_z<-data[[3]]

as_raw=function(data){
  data[,4:dim(data)[2]]
}

drop_nas=function(data){
  data[,colSums(is.na(data))<nrow(data)]
}

raw_x<-as_raw(lab_x)
raw_y<-as_raw(lab_y)
raw_z<-as_raw(lab_z)

#select gesture (and participant)
gesture_x<-lab_x %>% filter(gesture == 'left') #%>% filter(participant==0)
gesture_y<-lab_y %>% filter(gesture == 'left') #%>% filter(participant==0)
gesture_z<-lab_y %>% filter(gesture == 'left') #%>% filter(participant==0)

#drop NA collumns
dropped_x <- drop_nas(gesture_x) 
dropped_y <- drop_nas(gesture_y) 
dropped_z <- drop_nas(gesture_z)  

matplot(t(as_raw(dropped_x)), type='l', col= alpha(colour = 1, 0.03))


