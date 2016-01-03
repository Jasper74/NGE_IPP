#######################################
########## NIGER 2010-11 ##############
#######################################

#Set the path for storage of output 
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

#######################################
############### OUTPUT ################
#######################################

# WDswitch
oput <- read_dta(file.path(dataPath, "ecvmaas2e_p2_en.dta")) %>%
  dplyr::select(hid, order=as02eq0, ea=grappe, field=as02eq01, parcel=as02eq03, crop=as02eq05, crop_code=as02eq06,
                harvest_kg=as02eq07c, sold=as02eq11, sold_volume_kg = as02eq12c, sold_value = as02eq13) 

#PROPER CROP CODES
oput <- left_join(oput, data.frame(crop_code=c(1:48), cropname = c("Millet","Sorghum","Paddy_rice","Maize","Souchet","Wheat","Fonio","Cowpeas","Voandzou","Peanuts",
                                "Gombo","Sorrel","Sesame","Cassava","Sweet_potato","Potato","Pepper","Ginger","Cloves","Mint",
                                "Spinach","Celery","Parsley","Spice(pepper)","Melon","Watermelon","Lettuce","Cabbage","Tomato","Carrot",
                                "Jaxatu","Eggplant","Onion","Cucumber","Squash","Garlic","Green_beans","Gourd","Radish","Turnip",
                                "Leeks","Amarante","Cotton","Beets","Peas","Taro","Yams","Other")))
oput <- dplyr::select(oput, hid, ea, field, parcel, cropname, crop_code, harvest_kg, sold_volume_kg, sold_value)

#######################################
############### IRRIGATION ############
#######################################
ir <- read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>% 
  dplyr::select(hid, field=as01q03, parcel=as01q05,
                water_source=as01q39)
ir$convey_method <- NA
ir$water_source2 <- NA
ir$convey_method2 <- NA


#######################################
############### FERTILIZER ############
#######################################

fr <- read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>%    
  dplyr::select(hid, fert_use=as02aq10, field=as01q03, parcel=as01q05, urea_qty=as02aq11a, urea_unit=as02aq11b,
                dap_qty=as02aq12a, dap_unit=as02aq12b, npk_qty=as02aq13a, npk_unit=as02aq13b, fertmix_qty=as02aq14a, 
                fertmix_unit=as02aq14b)

#Take value from urea_unit and turn it into a character
fr <- data.frame(lapply(fr, unclass))

fr <- fr[!is.na(fr$fert_use),] 
#If no fert use, then qtys and units are 0
fr[fr$fert_use==2, c("urea_qty","urea_unit","dap_qty","dap_unit","npk_qty","npk_unit","fertmix_qty","fertmix_unit")] <- 0 
fr <- fr[!fr$urea_unit %in% c(6,7),]
fr <- fr[!fr$dap_unit %in% c(6,7),]
fr <- fr[!fr$npk_unit %in% c(6,7),]
fr <- fr[!fr$fertmix_unit %in% c(6,7),]

fr <- left_join(fr, data.frame(urea_unit = c(1:8), urea_conv=c(1, 5, 10, 25, 50, NA, NA, NA))) 
fr <- left_join(fr, data.frame(dap_unit = c(1:8), dap_conv=c(1, 5, 10, 25, 50, NA, NA, NA)))
fr <- left_join(fr, data.frame(npk_unit = c(1:8), npk_conv=c(1, 5, 10, 25, 50, NA, NA, NA)))
fr <- left_join(fr, data.frame(fertmix_unit = (1:8), fertmix_conv=c(1, 5, 10, 25, 50, NA, NA, NA)))

fr$urea <- as.numeric(fr$urea_conv) * as.numeric(fr$urea_qty)
fr$dap <- as.numeric(fr$dap_conv) * as.numeric(fr$dap_qty)
fr$npk <- as.numeric(fr$npk_conv) * as.numeric(fr$npk_qty)
fr$fertmix <- as.numeric(fr$fertmix_conv) * as.numeric(fr$fertmix_qty)

fr <- dplyr::select(fr, hid, field, parcel, urea, dap, npk, fertmix)
fr[is.na(fr)] <- 0

#Convert to NPK
fr$N <- (0.18*fr$dap + 0.46*fr$urea + 0.15*fr$npk + 0.2633*fr$fertmix)    
fr$P <- (0.46*fr$dap + 0.00*fr$urea + 0.15*fr$npk + 0.2033*fr$fertmix)    
fr$K <- (0*fr$dap    + 0*fr$urea    + 0*fr$npk    + 0*fr$fertmix)   

fr <- dplyr::select(fr, hid, field, parcel, plot_urea_kg=urea, plot_dap_kg=dap, plot_npk_kg=npk, plot_fertmix_kg=fertmix, plot_N_kg=N, plot_P_kg=P, plot_K_kg=K)

#######################################
############### INPUT COST ############
#######################################
ic <- read_dta(file.path(dataPath, "ecvmaas2c_p1_en.dta")) %>%
  dplyr::select(hid, type=as02cq01, typecode=as02cq02, used=as02cq03, crop=as02cq04, qty=as02cq05a, qty_unit=as02cq05b,
                purch=as02cq07, purch_qty=as02cq08a, valu=as02cq08b)
ic <- dplyr::arrange(ic, hid, typecode)
ic <- data.frame(lapply(ic, unclass))

ic <- ic[!is.na(ic$used),] 
ic[ic$used==2, c("qty","qty_unit","purch","purch_qty","valu")] <- 0
ic[ic$used==1 & ic$purch %in% c(0,2), c("purch","purch_qty","valu")] <- 0

conv <- data.frame(qty_unit=c(1:11), conv=c(1,5,10,25,50,NA,NA,NA,NA,NA,NA))
ic <- left_join(ic, conv)
ic$purch_qty <- ic$purch_qty * ic$conv

#Split off fert costs
fert_cost <- dplyr::filter(ic, typecode %in% c(3,4,5,6))
fert_cost$urea_valu <- ifelse((fert_cost$typecode==3), fert_cost$valu, NA)
fert_cost$dap_valu <- ifelse((fert_cost$typecode==4), fert_cost$valu, NA)
fert_cost$npk_valu <- ifelse((fert_cost$typecode==5), fert_cost$valu, NA)
fert_cost$fertmix_valu <- ifelse((fert_cost$typecode==6), fert_cost$val, NA)

fert_cost <- dplyr::select(fert_cost, hid, urea_valu, dap_valu, npk_valu, fertmix_valu)  
fert_cost[is.na(fert_cost)] <- 0

#Translate costs into N, P and K
fert_cost$N_cost <- (0.28125*fert_cost$dap_valu + 1*fert_cost$urea_valu + 0.5*fert_cost$npk_valu + 0.5643*fert_cost$fertmix_valu)
fert_cost$P_cost <- (0.71875*fert_cost$dap_valu + 0.5*fert_cost$npk_valu + 0.4347*fert_cost$fertmix_valu)
fert_cost$K_cost <- 0

fert_hh <- summarise(group_by(fert_cost, hid),
                     urea_cost = sum(urea_valu),
                     dap_cost = sum(dap_valu),
                     npk_cost = sum(npk_valu),
                     fertmix_cost = sum(fertmix_valu),
                     N_cost = sum(N_cost),
                     P_cost = sum(P_cost),
                     K_cost = sum(K_cost))
fert_hh[is.na(fert_hh)] <- 0

#Seed cost 
seed <- dplyr::filter(ic, typecode %in% c(11:17))
seed <- dplyr::select(seed, hid, crop_code=crop, seed_cost=valu)

#All other input costs
ic <- dplyr::filter(ic, !typecode %in% c(3,4,5,6,11,12,13,14,15,16,17))
ic_hh <- summarise(group_by(ic, hid), 
                   input_cost = sum(valu, na.rm=T))

#OTHER COSTS OF AGRICULTURAL ACTIVITIES (household level)
oc <- read_dta(file.path(dataPath, "ecvmaas2d_p2_en.dta")) %>%
    dplyr::select(hid, other_type=as02dq01, other_code=as02dq02, used=as02dq03,
                other_occurrence=as02dq04, other_occ_value=as02dq05) 

oc <- oc[!is.na(oc$used),]
oc[oc$input_used==2, c("other_occurrence","other_occ_value")] <- 0

oc$other_cost <- oc$other_occ_value * oc$other_occurrence 
oc_hh <- summarise(oc_hh <- group_by(oc, hid), 
                           other_cost = sum(other_cost, na.rm=T))

tc <- left_join(ic_hh, oc_hh)
tc <- left_join(tc, fert_hh, by="hid")
tc$input_cost <- tc$input_cost + tc$other_cost
tc <- dplyr::select(tc, -other_cost)

tc <- dplyr::select(tc, hid, 
                    household_urea_cost=urea_cost,
                    household_dap_cost=dap_cost,
                    household_npk_cost=npk_cost,
                    household_fertmix_cost=fertmix_cost,
                    household_N_cost=N_cost,
                    household_P_cost=P_cost,
                    household_K_cost=K_cost,
                    household_input_cost=input_cost)
                    
#######################################
############### labor ################
#######################################

lab_sp <-  read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>% 
  dplyr::select(hid, ea=grappe, field=as01q03, parcel=as01q05,
                fam1_days=as02aq20b, fam2_days=as02aq21b, fam3_days=as02aq22b, 
                fam4_days=as02aq23b, fam5_days=as02aq24b, fam6_days=as02aq25b,
                mut_men=as02aq26b, mut_women=as02aq26c, mut_child=as02aq26d,
                nonfam_men=as02aq27b, nonfam_women=as02aq27c, nonfam_child = as02aq27d, 
                mut_cost=as02aq26e, nonfam_cost = as02aq27e)

#If all rows are NA, remove observations, else we can assume that NA's are 0
lab_sp <- lab_sp[rowSums(is.na(lab_sp))<14,]
lab_sp[is.na(lab_sp)] <- 0

#add up labor days (men, women and children together)
lab_sp$fam_days <- rowSums(lab_sp[,5:10])
lab_sp$hire_days <- rowSums(lab_sp[,11:16])
lab_sp$hire_cost <- rowSums(lab_sp[,17:18])

lab_sp <- dplyr::select(lab_sp, hid, ea, field, parcel, fam_days, hire_days, hire_cost)

lab_harv <-  read_dta(file.path(dataPath, "ecvmaas1_p2_en.dta")) %>%  
  dplyr::select(hid, field=as01q03, parcel=as01q05,
                fam1_days=as02aq36b, fam2_days=as02aq37b, fam3_days=as02aq38b, fam4_days=as02aq39b, 
                fam5_days=as02aq40b, fam6_days=as02aq41b,
                mut_men=as02aq42b,mut_women=as02aq42c, mut_child=as02aq42d, 
                nonfam_men=as02aq43b, nonfam_women=as02aq43c, nonfam_child = as02aq43d, 
                mut_cost=as02aq42e, nonfam_cost = as02aq43e)

lab_harv <- lab_harv[rowSums(is.na(lab_harv))<14,]
lab_harv[is.na(lab_harv)] <- 0

#add up labor days (men, women and children together)
lab_harv$fam_days_harvest <- rowSums(lab_harv[,4:9])
lab_harv$hire_days_harvest <- rowSums(lab_harv[,10:15])
lab_harv$hire_cost_harvest <- rowSums(lab_harv[,16:17])


lab_harv <- dplyr::select(lab_harv, hid, field, parcel, fam_days_harvest, hire_days_harvest, hire_cost_harvest)
lab_harv$field <- as.integer(lab_harv$field)
lab_harv$parcel <- as.integer(lab_harv$parcel)

lab <- left_join(lab_sp, lab_harv, by=c("hid","field","parcel"))

lab$total_fam_days = lab$fam_days + lab$fam_days_harvest
lab$total_hire_days = lab$hire_days + lab$hire_days_harvest
lab$total_hire_cost = lab$hire_cost + lab$hire_cost_harvest

region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)
ngegeo <-  read_dta(file.path(dataPath, "NER_EA_Offsets.dta")) %>% 
  dplyr::select(ea=grappe, lat=LAT_DD_MOD, lon=LON_DD_MOD)

lab <- left_join(lab, region)
lab <- left_join(lab, ngegeo)

lab <- dplyr::select(lab, hid, field, parcel, 
                     plot_familylabor_days=total_fam_days, 
                     plot_hiredlabor_days=total_hire_days, 
                     plot_hiredlabor_cost=total_hire_cost)

#######################################
############### REGIONS and EA GPS ####
#######################################
region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)
ngegeo <-  read_dta(file.path(dataPath, "NER_EA_Offsets.dta")) %>% 
  dplyr::select(ea=grappe, lat=LAT_DD_MOD, lon=LON_DD_MOD)

plot <- read_dta(file.path(dataPath, "ecvmaas2b_p1_en.dta")) %>%
  dplyr::select(hid, field=as02bq01, parcel=as02bq03, crop_code=as02bq06, worked=as02bq04, harv_area_m2=as02bq08)

nge <- oput     
nge <- left_join(nge, plot, by=c("hid","field","parcel","crop_code"))
nge <- left_join(nge, region, by="hid")
nge <- left_join(nge, ngegeo)
nge <- left_join(nge, lab, by=c("hid","field","parcel"))
nge <- left_join(nge, fr, by=c("hid","field","parcel"))
nge <- left_join(nge, tc, by="hid")
nge <- left_join(nge, seed)
nge <- left_join(nge, ir, by=c("hid","field","parcel")) #irrigation costs are already given in $
nge <- nge[nge$region %in% c(1:8),]

#Scale up inflation from 2011 to 2014
infl <- 1.029 * 1.005 * 1.023 * 0.992
exc_rate = 0.00161422 #29-11-2015, 20:09 from http://www.xe.com/currencyconverter/convert/?From=XOF&To=USD

nge[,c("sold_value", "plot_hiredlabor_cost","household_input_cost","household_urea_cost", "household_dap_cost", "household_npk_cost", "household_fertmix_cost",
       "household_N_cost", "household_P_cost", "household_K_cost","seed_cost")] <- 
  nge[,c("sold_value", "plot_hiredlabor_cost","household_input_cost","household_urea_cost", "household_dap_cost", "household_npk_cost", "household_fertmix_cost",
         "household_N_cost", "household_P_cost", "household_K_cost","seed_cost")]* exc_rate * infl
  
  write.csv(nge, "Niger/Output/NGE_rawdata.csv")
