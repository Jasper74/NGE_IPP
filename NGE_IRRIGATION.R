dataPath <- "Niger/DATA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

library(haven)
library(stringr)
library(plyr)
library(dplyr)
library(reshape2)

options(scipen=999)

#Conveyance method: 1 Gravity   2 Discharge (pump)   3 Manual Scooping    4 Drip   5 Other (specify) 
#costs per hectare  $50[2]      $259 [1]             $0 [2]               $50

# [1] Xie et al. (2014); costs given in dollars and therefore do not need to be converted using exchange rates 
# [2] Own estimation
# 

#==========================================================================================
#THE FOLLOWING TWO LINES CAN BE EDITED TO INDICATE IRRIGATION COSTS OF CONVEYANCE METHODS
#==========================================================================================
irrigation_cost_1 <- data.frame(convey_method=c(1:5), irrigationcost=c(50, 259, 0, 50, 0))
irrigation_cost_2 <- data.frame(convey_method2=c(1:5), irrigationcost2=c(50, 259, 0, 50, 0))


#######################################
############### DRY SEASON ############
#######################################

region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)
ngegeo <-  read_dta(file.path(dataPath, "NER_EA_Offsets.dta")) %>% 
  dplyr::select(ea=grappe, lat=LAT_DD_MOD, lon=LON_DD_MOD)
ir <- read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>% 
  dplyr::select(hid, ea=grappe, field=as01q03, parcel=as01q05, 
                water_source=as01q34a, convey_method=as01q34b, 
                water_source2=as01q38a, convey_method2=as01q38b)
ir <- left_join(ir, region)
ir <- left_join(ir, ngegeo)

#Other; unsure whether its irrigated or not
ir <- dplyr::filter(ir, water_source!=5)
ir <- dplyr::filter(ir, water_source2!=9 | is.na(water_source2))
ir <- dplyr::filter(ir, convey_method!=9 & convey_method!=5 | is.na(convey_method))
ir <- dplyr::filter(ir, water_source2!=5 | is.na(convey_method2))

#1: waterway 2: well 3: drilling 4: dam, retention water 5: other 6: not applicable
ir$irrig[ir$water_source %in% c(6)] <- 0
ir$irrig[ir$water_source %in% c(1,2,3,4)] <- 1

ir <- left_join(ir, irrigation_cost_1)
ir <- left_join(ir, irrigation_cost_2)

ir[ir$water_source==6, c("irrigationcost","irrigationcost2")] <- 0
ir$irrigationcost2 <- ifelse((ir$irrigationcost>=0 & is.na(ir$irrigationcost2)), 0, ir$irrigationcost2)

ir$irrigation_cost <- ir$irrigationcost + ir$irrigationcost2
ir <- dplyr::select(ir, hid, parcel, field, water_source, water_source2, convey_method, convey_method2, irrigation_cost)

#write to file
write.csv(ir, "Niger/Output/NGE_irrigation_cost.csv")

#######################################
############### RAINY SEASON ##########
#######################################

ir_rainy<- read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>% 
  dplyr::select(hid, ea=grappe, field=as01q03, parcel=as01q05, water_source=as01q39)
region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)
ngegeo <-  read_dta(file.path(dataPath, "NER_EA_Offsets.dta")) %>% 
  dplyr::select(ea=grappe, lat=LAT_DD_MOD, lon=LON_DD_MOD)

ir_rainy <- left_join(ir_rainy, region)
ir_rainy <- left_join(ir_rainy, ngegeo)

#Remove 'other' source
ir_rainy<- ir_rainy[ir_rainy$water_source!=6,]
ir_rainy$irrigated[ir_rainy$water_source %in% c(5,6,7)] <- 0
ir_rainy$irrigated[ir_rainy$water_source %in% c(1,2,3,4)] <- 1


