rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_tbls.RData")

library(dplyr)

#get a list of data.frames
file.names<-ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

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

JOIN <- right_join(prop %>% select(API, PROPNUM, LEASE, TYPE_CURVE_REGION, TYPE_CURVE_SUBREGION, LATITUDE ,LATITUDE_BH, 
                                   LONGITUDE, LONGITUDE_BH, FIRST_PROD, EFF_LAT, RSV_CAT), 
                   daily %>% select(Aries_Prop_Num, Prod_Dt, Vol_Gas_Wh_Mcf, Vol_Gas_Prod_Mcf, Vol_Oil_Wh_Bbl, Vol_Oil_Prod_Bbl, Vol_Wtr_Wh_Bbl), 
                   by = c("PROPNUM" = "Aries_Prop_Num"))






TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/prodvol_tbls.RData',sep=''), RFormat=T )))

