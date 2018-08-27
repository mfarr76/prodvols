rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_tbls.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/prodvol_daily_join.RData")


#install.packages("tidyr", repos =  "https://cran.microsoft.com/snapshot/2017-07-01/")

library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
#library(zoo, warn.conflicts = FALSE)

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

##input varaibles


##Shrink factors from GPA Standard 2145-09 converted from 14.696 to 14.73 
##factors have units of scf/gal liquid...divide by 1000  to mcf/gal liquid
C2_Shrink	<- 37.40146965 / 1000
C3_Shrink	<- 36.30700177 / 1000
iC4_Shrink	<- 30.56628323 / 1000
nC4_Shrink	<- 31.72759647 / 1000
iC5_Shrink	<- 27.35072261 / 1000
nC5_Shrink	<- 27.5941594 / 1000
C6Plus_Shrink	<- 24.32372573 / 1000
##=======================================================================


JOIN <- right_join(prop %>% select(API, PROPNUM, WELLCOMPID, LEASE, BUDGET_NODE, TYPE_CURVE_REGION, 
                                   TYPE_CURVE_SUBREGION, LATITUDE ,LATITUDE_BH, LONGITUDE, 
                                   LONGITUDE_BH, FIRST_PROD, EFF_LAT, RSV_CAT), 
                   daily %>% 
                     group_by(Aries_Prop_Num) %>%
                     arrange(Aries_Prop_Num, Prod_Dt) %>%
                     mutate(MON_END_DATE = ceiling_date(Prod_Dt, "month") - days(1)) %>% ##time zero
                     rename(GR_Gas_Prod_Mcf = Vol_Gas_Prod_Mcf, 
                            GR_Oil_Prod_Bbl = Vol_Oil_Prod_Bbl) %>%
                     select(Aries_Prop_Num, Prod_Dt, MON_END_DATE, GR_Gas_Prod_Mcf, GR_Oil_Prod_Bbl),
                   by = c("PROPNUM" = "Aries_Prop_Num")) %>%
  ##join well interest table
  left_join(., interest %>%
              select(WellCompletionEKey, NetRevenueInterest, GrossWorkingInterest), 
            by = c("WELLCOMPID" = "WellCompletionEKey")) %>%
  left_join(., gas %>%
              mutate(DATE = (ceiling_date(SampleDate, "month") - days(1))) %>% 
              group_by(WellCompletionEKey, DATE) %>%
              select(DATE, WellCompletionEKey, Ethane_C2_Dry, Propane_C3_Dry,
                     Isobutane_iC4_Dry, n_Butane_nC4_Dry,  Isopentane_iC5_Dry,
                     n_Pentane_nC5_Dry, Hexanes_Plus_C6Plus_Dry, Heating_Value_Dry) %>%
              mutate(GPM = Ethane_C2_Dry + Propane_C3_Dry + Isobutane_iC4_Dry + 
                       n_Butane_nC4_Dry + Isopentane_iC5_Dry + n_Pentane_nC5_Dry + 
                       Hexanes_Plus_C6Plus_Dry, NGLYield = GPM / 42 * 1000) %>% 
              summarise(BTU = mean(Heating_Value_Dry, na.rm = TRUE),
                        C2_WH = mean(Ethane_C2_Dry, na.rm = TRUE),
                        C3_WH = mean(Propane_C3_Dry, na.rm = TRUE),
                        iC4_WH = mean(Isobutane_iC4_Dry, na.rm = TRUE),
                        nC4_WH = mean(n_Butane_nC4_Dry, na.rm = TRUE),
                        iC5_WH = mean(Isopentane_iC5_Dry, na.rm = TRUE),
                        nC5_WH = mean(n_Pentane_nC5_Dry, na.rm = TRUE), 
                        C6Plus_WH = mean(Hexanes_Plus_C6Plus_Dry, na.rm = TRUE), 
                        GPM_WH = mean(GPM, na.rm = TRUE), 
                        NGLYield_WH = mean(NGLYield, na.rm = TRUE)), 
            by = c("WELLCOMPID" = "WellCompletionEKey", c("MON_END_DATE" = "DATE"))) %>% 
  ##join condensate table
  left_join(., cond %>% 
              select(WellCompletionEKey, SampleDate, Shrinkage_Factor_Stock_Tank_Liquid, 
                     API_Gravity_Separator_Liquid) %>%
              mutate(SAMPLE_END_DATE = ceiling_date(SampleDate, "month") - days(1)), 
            by = c("WELLCOMPID" = "WellCompletionEKey", c("MON_END_DATE" = "SAMPLE_END_DATE"))) %>%
  fill(BTU, C2_WH, C3_WH, iC4_WH, nC4_WH, iC5_WH, nC5_WH, C6Plus_WH, NGLYield_WH, GPM_WH,
       Shrinkage_Factor_Stock_Tank_Liquid, API_Gravity_Separator_Liquid, .direction = "up") %>% ##fill in missing values
  fill(BTU, C2_WH, C3_WH, iC4_WH, nC4_WH, iC5_WH, nC5_WH, C6Plus_WH, NGLYield_WH, GPM_WH,
       Shrinkage_Factor_Stock_Tank_Liquid, API_Gravity_Separator_Liquid, .direction = "down") %>% ##fill in missing values
  ##join plant recovery factor table
  merge(., plant_rf[-1] %>% 
          summarise_all(., mean)) %>%
  group_by(PROPNUM) %>%
  arrange(PROPNUM, Prod_Dt) %>%
  mutate(prod = 1, 
         DAYS = cumsum(prod), 
         C2_Sales = C2_WH * C2_RF, 
         C3_Sales = C3_WH * C3_RF, 
         iC4_Sales = iC4_WH * iC4_RF, 
         nC4_Sales = nC4_WH * nC4_RF, 
         iC5_Sales = iC5_WH * iC5_RF, 
         nC5_Sales = nC5_WH * nC5_RF, 
         C6Plus_Sales = C6Plus_WH * C6Plus_RF,
         GPM_Sales_w_Ethane = C2_Sales + C3_Sales + iC4_Sales + nC4_Sales + iC5_Sales + nC5_Sales + C6Plus_Sales,
         GPM_Sales_wo_Ethane = C3_Sales + iC4_Sales + nC4_Sales + iC5_Sales + nC5_Sales + C6Plus_Sales,
         NGLYield_Sales_w_Ethane = GPM_Sales_w_Ethane / 42 * 1000,
         NGLYield_Sales_wo_Ethane = GPM_Sales_wo_Ethane / 42 * 1000,
         C2_Shrink_Mcf = GR_Gas_Prod_Mcf * C3_Sales * C3_Shrink,
         C3_Shrink_Mcf = GR_Gas_Prod_Mcf * C3_Sales * C3_Shrink,
         iC4_Shrink_Mcf = GR_Gas_Prod_Mcf * iC4_Sales * iC4_Shrink,
         nC4_Shrink_Mcf = GR_Gas_Prod_Mcf * nC4_Sales * nC4_Shrink,
         iC5_Shrink_Mcf = GR_Gas_Prod_Mcf * iC5_Sales * iC5_Shrink,
         nC5_Shrink_Mcf = GR_Gas_Prod_Mcf * nC5_Sales * nC5_Shrink,
         C6Plus_Shrink_Mcf = GR_Gas_Prod_Mcf * C6Plus_Sales * C6Plus_Shrink,
         Total_Shrink_Mcf_w_Ethane = C2_Shrink_Mcf + C3_Shrink_Mcf + iC4_Shrink_Mcf + nC4_Shrink_Mcf + iC5_Shrink_Mcf + nC5_Shrink_Mcf + C6Plus_Shrink_Mcf,
         Total_Shrink_Mcf_wo_Ethane = C3_Shrink_Mcf + iC4_Shrink_Mcf + nC4_Shrink_Mcf + iC5_Shrink_Mcf + nC5_Shrink_Mcf + C6Plus_Shrink_Mcf,
         GR_Shrunk_Gas_Mcf_w_Ethane = GR_Gas_Prod_Mcf - Total_Shrink_Mcf_w_Ethane,
         GR_Shrunk_Gas_Mcf_wo_Ethane = GR_Gas_Prod_Mcf - Total_Shrink_Mcf_wo_Ethane,
         NETGAS_Shrunk_Mcf_w_Ethane = GR_Shrunk_Gas_Mcf_w_Ethane * NetRevenueInterest / 100,
         NETGAS_Shrunk_Mcf_wo_Ethane = GR_Shrunk_Gas_Mcf_wo_Ethane * NetRevenueInterest / 100,
         NETOIL_Bbl = GR_Oil_Prod_Bbl * NetRevenueInterest / 100, 
         NETNGL_Bbl_w_Ethane = GR_Gas_Prod_Mcf / 1000 * NetRevenueInterest / 100 * NGLYield_Sales_w_Ethane,
         NETNGL_Bbl_wo_Ethane = GR_Gas_Prod_Mcf / 1000 * NetRevenueInterest / 100 * NGLYield_Sales_wo_Ethane,
         NETBOE_Prod_w_Ethane = NETOIL_Bbl + NETGAS_Shrunk_Mcf_w_Ethane / 6 + NETNGL_Bbl_w_Ethane, 
         NETBOE_Prod_wo_Ethane = NETOIL_Bbl + NETGAS_Shrunk_Mcf_wo_Ethane / 6 + NETNGL_Bbl_wo_Ethane) 

b334 <- JOIN %>% filter(LEASE == "BRISCOE B 333H") %>% 
  #select(LEASE, Prod_Dt, C2_WH, C2_RF)
  select(LEASE, Prod_Dt, C2_Shrink_Mcf, GPM_WH, GPM_Sales, Vol_Gas_Prod_Mcf, Total_Shrink_Mcf, GR_Shrunk_Gas_Mcf)

B334H <- subset(JOIN, LEASE == "BRISCOE B 333H")

plant_rf[-1] %>%
  summarise_all(., mean)


dg <- gas %>% 
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
            C2_WH = mean(Ethane_C2_Dry, na.rm = TRUE),
            C3_WH = mean(Propane_C3_Dry, na.rm = TRUE),
            iC4_WH = mean(Isobutane_iC4_Dry, na.rm = TRUE),
            nC4_WH = mean(n_Butane_nC4_Dry, na.rm = TRUE),
            iC5_WH = mean(Isopentane_iC5_Dry, na.rm = TRUE),
            nC5_WH = mean(n_Pentane_nC5_Dry, na.rm = TRUE),
            C6Plus_WH = mean(Hexanes_Plus_C6Plus_Dry, na.rm = TRUE),
            GPM_WH = mean(GPM, na.rm = TRUE), 
            NGLYield = mean(NGLYield, na.rm = TRUE)) %>%
  merge(., plant_rf[-1] %>% 
          summarise_all(., mean)) %>%
  mutate(C2_Sales = C2_WH * C2_RF, 
         C3_Sales = C3_WH * C3_RF, 
         iC4_Sales = iC4_WH * iC4_RF, 
         nC4_Sales = nC4_WH * nC4_RF, 
         iC5_Sales = iC5_WH * iC5_RF, 
         nC5_Sales = nC5_WH * nC5_RF, 
         C6Plus_Sales = C6Plus_WH * C6Plus_RF,
         GPM_Sales = C2_Sales + C3_Sales + iC4_Sales + nC4_Sales + iC5_Sales + nC5_Sales + C6Plus_Sales)

head(
dg %>% mutate(GPM_Perc = GPM_Sales / GPM_WH) %>% select(GPM_Perc))



GAS.COMP <- gas %>% 
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
            C2_WH = mean(Ethane_C2_Dry, na.rm = TRUE),
            C3_WH = mean(Propane_C3_Dry, na.rm = TRUE),
            iC4_WH = mean(Isobutane_iC4_Dry, na.rm = TRUE),
            nC4_WH = mean(n_Butane_nC4_Dry, na.rm = TRUE),
            iC5_WH = mean(Isopentane_iC5_Dry, na.rm = TRUE),
            nC5_WH = mean(n_Pentane_nC5_Dry, na.rm = TRUE),
            C6Plus_WH = mean(Hexanes_Plus_C6Plus_Dry, na.rm = TRUE),
            GPM_WH = mean(GPM, na.rm = TRUE), 
            NGLYield = mean(NGLYield, na.rm = TRUE)) %>%
  merge(., plant_rf[-1] %>% 
          summarise_all(., mean)) %>%
  mutate(C2_Sales = C2_WH * C2_RF, 
         C3_Sales = C3_WH * C3_RF, 
         iC4_Sales = iC4_WH * iC4_RF, 
         nC4_Sales = nC4_WH * nC4_RF, 
         iC5_Sales = iC5_WH * iC5_RF, 
         nC5_Sales = nC5_WH * nC5_RF, 
         C6Plus_Sales = C6Plus_WH * C6Plus_RF,
         GPM_Sales = C2_Sales + C3_Sales + iC4_Sales + nC4_Sales + iC5_Sales + nC5_Sales + C6Plus_Sales)







TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/prodvol_tbls.RData',sep=''), RFormat=T )))





df <- data.frame(Month = 1:12, Year = c(2000, rep(NA, 11)))
df %>% fill(Year)


