rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_tbls.RData")

library(dplyr)

#get a list of data.frames
file.names<-ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']


i <- 2
for(i in 1:length(file.names)){

tmp<- get(file.names[i])
names(tmp)<-gsub("-","", names(tmp))
names(tmp)<-gsub(" ", "_", names(tmp))
names(tmp)<-gsub("[()]", "", names(tmp))
names(tmp)<-gsub("/","_",  names(tmp))
names(tmp)<-gsub("\\%", "", names(tmp))
assign(file.names[i], tmp)
rm(tmp)

}



