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

#takes only numeric things and creates a matrix
as_raw=function(data){
  as.matrix(data[,4:dim(data)[2]])
}

#drops the last NAs
drop_nas=function(data){
  data[,colSums(is.na(data))<nrow(data)]
}

raw_x<-as_raw(lab_x)
raw_y<-as_raw(lab_y)
raw_z<-as_raw(lab_z)


generate_magnitude<-function(){
  index<-0
  lab_m<-adply(raw_x,.margins = 1,.fun = function(row){
    index<<-index+1
    sqrt(raw_x[index,]**2+raw_y[index,]**2+raw_z[index,]**2)
  })
  lab_m<-lab_m[,-1]
  
  lab_m<-cbind(record=lab_x[,3], lab_m)
  lab_m<-cbind(person=lab_x[,2], lab_m)
  lab_m<-cbind(gesture=lab_x[,1], lab_m)
  lab_m
}
lab_m<-generate_magnitude()
raw_m<-as_raw(lab_m)


#returns specific gestures
subset_gesture=function(data,gesture,dropNA=FALSE){
  sub<-data %>% filter(gesture == gesture) #%>% filter(participant==0)
  if(dropNA){
    sub<-drop_nas(sub) 
  }
  sub
}


test<-subset_gesture(data = lab_m,gesture = "right",dropNA = TRUE) %>% as_raw

matplot(t(test), type='l', col= alpha(colour = 1, 0.03))


