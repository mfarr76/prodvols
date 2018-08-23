rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_tbls.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_daily_join.RData")



library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

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


JOIN <- right_join(prop %>% select(API, PROPNUM, WELLCOMPID, LEASE, BUDGET_NODE, TYPE_CURVE_REGION, 
                                   TYPE_CURVE_SUBREGION, LATITUDE ,LATITUDE_BH, LONGITUDE, 
                                   LONGITUDE_BH, FIRST_PROD, EFF_LAT, RSV_CAT), 
                   daily %>% select(Aries_Prop_Num, Prod_Dt, Vol_Gas_Wh_Mcf, Vol_Gas_Prod_Mcf, 
                                    Vol_Oil_Wh_Bbl, Vol_Oil_Prod_Bbl, Vol_Wtr_Wh_Bbl) %>%
                     mutate(MON_END_DATE = ceiling_date(Prod_Dt, "month") - days(1)), 
                   by = c("PROPNUM" = "Aries_Prop_Num")) %>%
  group_by(PROPNUM) %>%
  arrange(PROPNUM, Prod_Dt) %>%
  mutate(prod = 1, 
         DAYS = cumsum(prod), 
         BOE = Vol_Oil_Wh_Bbl + Vol_Gas_Wh_Mcf / 6, 
         CUMGAS_MCF = cumsum(Vol_Gas_Prod_Mcf), 
         CUMOIL_BBL = cumsum(Vol_Oil_Prod_Bbl)) %>%
  left_join(., gas %>% 
  mutate(DATE = (ceiling_date(SampleDate, "month") - days(1))) %>%
  group_by(WellCompletionEKey, DATE) %>%
  select(DATE, WellCompletionEKey, Ethane_C2_Dry, Propane_C3_Dry, 
         Isobutane_iC4_Dry, n_Butane_nC4_Dry,  Isopentane_iC5_Dry, 
         n_Pentane_nC5_Dry, Hexanes_Plus_C6Plus_Dry, Heating_Value_Dry) %>%
  mutate(GPM = Ethane_C2_Dry + Propane_C3_Dry + Isobutane_iC4_Dry +
           n_Butane_nC4_Dry + Isopentane_iC5_Dry + n_Pentane_nC5_Dry + 
           Hexanes_Plus_C6Plus_Dry, 
         NGLYield = GPM / 42 * 1000) %>%
  summarise(BTU = mean(Heating_Value_Dry, na.rm = TRUE),
            C2 = mean(Ethane_C2_Dry, na.rm = TRUE),
            C3 = mean(Propane_C3_Dry, na.rm = TRUE),
            IC4 = mean(Isobutane_iC4_Dry, na.rm = TRUE),
            NC4 = mean(n_Butane_nC4_Dry, na.rm = TRUE),
            IC5 = mean(Isopentane_iC5_Dry, na.rm = TRUE),
            NC5 = mean(n_Pentane_nC5_Dry, na.rm = TRUE),
            C6Plus = mean(Hexanes_Plus_C6Plus_Dry, na.rm = TRUE),
            GPM = mean(GPM, na.rm = TRUE), 
            NGLYield = mean(NGLYield, na.rm = TRUE)), 
  by = c("WELLCOMPID" = "WellCompletionEKey", c("MON_END_DATE" = "DATE"))) %>%
  left_join(., interest %>%
              select(WellCompletionEKey, NetRevenueInterest, GrossWorkingInterest), by = c("WELLCOMPID" = "WellCompletionEKey")) %>%
  left_join(., cond %>% 
              select(WellCompletionEKey, SampleDate, Shrinkage_Factor_Stock_Tank_Liquid, 
                     API_Gravity_Separator_Liquid) %>%
              mutate(SAMPLE_END_DATE = ceiling_date(SampleDate, "month") - days(1)), 
            by = c("WELLCOMPID" = "WellCompletionEKey", c("MON_END_DATE" = "SAMPLE_END_DATE"))) %>%
  fill(BTU, C2, C3, IC4, NC4,IC5, NC5, C6Plus, NGLYield, GPM) %>%
  mutate(NETGAS_Prod_Mcf = Vol_Gas_Prod_Mcf * ( GrossWorkingInterest / 100 ), 
         NETOIL_Prod_Bbl = Vol_Oil_Prod_Bbl * ( GrossWorkingInterest / 100 ), 
         NETNGL_Bbl = NETGAS_Prod_Mcf / 1000 * NGLYield,
         NETBOE_Prod = NETOIL_Prod_Bbl + NETGAS_Prod_Mcf / 6 + NETNGL_Bbl)




gas_comp <- gas %>% 
  mutate(DATE = (ceiling_date(SampleDate, "month") - days(1))) %>%
  group_by(WellCompletionEKey, DATE) %>%
  select(DATE, WellCompletionEKey, Ethane_C2_Dry, Propane_C3_Dry, 
         Isobutane_iC4_Dry, n_Butane_nC4_Dry,  Isopentane_iC5_Dry, 
         n_Pentane_nC5_Dry, Hexanes_Plus_C6Plus_Dry, Heating_Value_Dry) %>%
  mutate(GPM = Ethane_C2_Dry + Propane_C3_Dry + Isobutane_iC4_Dry +
           n_Butane_nC4_Dry + Isopentane_iC5_Dry + n_Pentane_nC5_Dry + 
           Hexanes_Plus_C6Plus_Dry, 
         NGLYield = GPM / 42 * 1000) %>%
  summarise(BTU = mean(Heating_Value_Dry, na.rm = TRUE),
            C2 = mean(Ethane_C2_Dry, na.rm = TRUE),
            C3 = mean(Propane_C3_Dry, na.rm = TRUE),
            IC4 = mean(Isobutane_iC4_Dry, na.rm = TRUE),
            NC4 = mean(n_Butane_nC4_Dry, na.rm = TRUE),
            IC5 = mean(Isopentane_iC5_Dry, na.rm = TRUE),
            NC5 = mean(n_Pentane_nC5_Dry, na.rm = TRUE),
            C6Plus = mean(Hexanes_Plus_C6Plus_Dry, na.rm = TRUE),
            GPM = mean(GPM, na.rm = TRUE), 
            NGLYield = mean(NGLYield, na.rm = TRUE))















TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/prodvol_tbls.RData',sep=''), RFormat=T )))





df <- data.frame(Month = 1:12, Year = c(2000, rep(NA, 11)))
df %>% fill(Year)


