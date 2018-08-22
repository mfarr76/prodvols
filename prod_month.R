rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_month.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_month_join.RData")


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
                                  LONGITUDE, LONGITUDE_BH, FIRST_PROD, EFF_LAT, RSV_CAT, ZONE, REGION9), 
                   mon_prod %>% select(Aries_Prop_Num, Prod_Dt, Vol_Gas_Prod_Mcf, Vol_Oil_Prod_Bbl), by = c("PROPNUM" = "Aries_Prop_Num")) %>%
  #na.omit() %>%
  group_by(PROPNUM) %>%
  arrange(PROPNUM, Prod_Dt) %>%
  mutate(VINTAGE = format(as.Date(FIRST_PROD, format = "%Y/%m/%d"), "%Y"), 
         prod = 1, 
         MONTHS = cumsum(prod),
         GAS_MCF_1000 = Vol_Gas_Prod_Mcf / EFF_LAT * 1000, 
         OIL_BBL_1000 = Vol_Oil_Prod_Bbl / EFF_LAT * 1000,
         CUMGAS_MCF = cumsum(Vol_Gas_Prod_Mcf), 
         CUMOIL_BBL = cumsum(Vol_Oil_Prod_Bbl),
         CUMGAS_3MMCF =  mean(ifelse(MONTHS == 3, CUMGAS_MCF / 1000, NA), na.rm = TRUE),
         CUMGAS_6MMCF =  mean(ifelse(MONTHS == 6, CUMGAS_MCF / 1000, NA), na.rm = TRUE),
         CUMGAS_12MMCF =  mean(ifelse(MONTHS == 12,CUMGAS_MCF / 1000, NA), na.rm = TRUE), 
         CUMOIL_3MBO =  mean(ifelse(MONTHS == 3, CUMOIL_BBL / 1000, NA), na.rm = TRUE),
         CUMOIL_6MBO =  mean(ifelse(MONTHS == 6, CUMOIL_BBL / 1000, NA), na.rm = TRUE),
         CUMOIL_12MBO =  mean(ifelse(MONTHS == 12, CUMOIL_BBL / 1000, NA), na.rm = TRUE), 
         VINTAGE = format(as.Date(FIRST_PROD, format = "%Y/%m/%d"), "%Y"))
         

