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

#Household
house <- read_dta(file.path(dataPath, "ecvmaind_p1p2_en.dta")) %>% 
  dplyr::select(hid, id=ms01q00, sex=ms01q01, age=ms01q06a, months=ms01q06b, edu=ms02q04)
house <- data.frame(lapply(house, unclass))
house$edu <- ifelse((house$edu %in% c(1,2,3)), 1, 0)

#######################################
############### OUTPUT ################
#######################################

# WDswitch
oput <- read_dta(file.path(dataPath, "ecvmaas2e_p2_en.dta")) %>%
  dplyr::select(hid, ea=grappe, field=as02eq01, parcel=as02eq03, crop=as02eq05, crop_code=as02eq06,
                qty=as02eq07c, sold=as02eq11, qty_sold = as02eq12c, valu = as02eq13) 
#PROPER CROP CODES
oput <- left_join(oput, data.frame(crop_code=c(1:48), cropname = c("Millet","Sorghum","Paddy_rice","Maize","Souchet","Wheat","Fonio","Cowpeas","Voandzou","Peanuts",
                                "Gombo","Sorrel","Sesame","Cassava","Sweet_potato","Potato","Pepper","Ginger","Cloves","Mint",
                                "Spinach","Celery","Parsley","Spice(pepper)","Melon","Watermelon","Lettuce","Cabbage","Tomato","Carrot",
                                "Jaxatu","Eggplant","Onion","Cucumber","Squash","Garlic","Green_beans","Gourd","Radish","Turnip",
                                "Leeks","Amarante","Cotton","Beets","Peas","Taro","Yams","Other")))
legumes <- c(37, 8, 45, 10)
oput <- ddply(oput, .(hid, field, parcel), transform,
              crop_count=length(unique(crop_code[!is.na(crop_code)])),
              legume=ifelse(any(crop_code %in% legumes), 1, 0))

oput_maze <- oput[ oput$zaocode %in% 4 & ! is.na(oput$harvest) & !oput$harvest %in% 0, ]
oput_maze$maize_prc <- oput_maze$valu/oput_maze$qty_sold

oput <- dplyr::select(oput, hid, ea, field, parcel, cropname, crop_code, harvest=qty, qty_sold, valu)

#######################################
######### PLOT CHARACTERISTICS ########
#######################################

plot <- read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>%
  dplyr::select(hhid=hid, field=as01q05, parcel=as01q05, own=as01q16, title=as01q18, soil=as01q23, slope=as01q24, watersource_cs=as01q34a,
                watersource_rs=as01q39, fallow_year=as01q42, manure=as02aq06, pest=as02aq15, pest_q=as02aq16a, 
                pest_q_unit=as02aq16b, fung_q=as02aq17a, herb_q=as02aq18b)

plot$own <- ifelse(plot$own %in% 1), 1, 0)
plot$title <- ifelse(plot$title %in% c(1,2,3,4), 1, 0)  # assume that they don't have a title if NA
plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4,5), labels=c("Sandy", "Silty", "Clay", "Rocky", "Other")) #OTHER THAN TZA
plot$slope <- factor(plot$slope, levels=c(1,2,3,4,5,6), labels=c("Hill", "Plain", "Gentle slope", "Steep slope", "Valley","Other"))
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$fung <- ifelse((plot$fung_q>0), 1, 0)
plot$fung <- ifelse((is.na(plot$fung)), 0, plot$fung)
plot$herb <- ifelse((plot$herb_q>0), 1, 0)
plot$herb <- ifelse((is.na(plot$herb)), 0, plot$herb)

#Irrigation
plot$irrig_rs <- ifelse((plot$watersource_rs!=6), plot$watersource_rs, NA)
plot$irrig_cs <- ifelse((plot$watersource_cs!=5), plot$watersource_cs, NA)

plot$irrig_rs <- ifelse(plot$watersource_rs %in% c(1,2,3,4), 1, 0)
plot$irrig_cs <- ifelse(plot$watersource_cs %in% c(1,2,3,4), 1, 0)

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
#fr[fr$fert_use==2, c("urea_qty","urea_unit","dap_qty","dap_unit","npk_qty","npk_unit","fertmix_qty","fertmix_unit")] <- 0 
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

fr <- dplyr::select(fr, hid, field, parcel, N, P, K)

#######################################
############### INPUT COST ############
#######################################
fert_cost <- read_dta(file.path(dataPath, "ecvmaas2c_p1_en.dta")) %>%
  dplyr::select(hid, type=as02cq01, typecode=as02cq02, used=as02cq03, crop=as02cq04, qty=as02cq05a, qty_unit=as02cq05b,
                purch=as02cq07, purch_qty=as02cq08a, valu=as02cq08b)
fert_cost <- dplyr::arrange(fert_cost, hid, typecode)
fert_cost <- data.frame(lapply(fert_cost, unclass))

#Split off fert costs
fert_cost <- dplyr::filter(fert_cost, typecode %in% c(3,4,5,6))
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
                     urea_valu = sum(urea_valu),
                     dap_valu = sum(dap_valu),
                     npk_valu = sum(npk_valu),
                     fertmix_valu = sum(fertmix_valu),
                     N_cost = sum(N_cost),
                     P_cost = sum(P_cost),
                     K_cost = sum(K_cost))
fert_hh[is.na(fert_hh)] <- 0

#######################################
############### LABOUR ################
#######################################
lab_sp <-  read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>% 
  dplyr::select(hid, field=as01q03, parcel=as01q05,
                fam1_days=as02aq20b, fam2_days=as02aq21b, fam3_days=as02aq22b, 
                fam4_days=as02aq23b, fam5_days=as02aq24b, fam6_days=as02aq25b,
                mut_men=as02aq26b, mut_women=as02aq26c, mut_child=as02aq26d,
                nonfam_men=as02aq27b, nonfam_women=as02aq27c, nonfam_child = as02aq27d, 
                mut_cost=as02aq26e, nonfam_cost = as02aq27e)

lab_sp <- lab_sp[rowSums(is.na(lab_sp))!=14, ]
lab_sp[is.na(lab_sp)] <- 0

#add up labour days (men, women and children together)
lab_sp$soilprep_days <- rowSums(lab_sp[,4:15], na.rm=TRUE)
lab_sp$soilprep_cost <- rowSums(lab_sp[,16:17], na.rm=TRUE)

lab_sp <- dplyr::select(lab_sp, hid, field, parcel, soilprep_days, soilprep_cost)

lab_harv <-  read_dta(file.path(dataPath, "ecvmaas1_p2_en.dta")) %>%  
  dplyr::select(hid, field=as01q03, parcel=as01q05,
                fam1_days=as02aq36b, fam2_days=as02aq37b, fam3_days=as02aq38b, fam4_days=as02aq39b, 
                fam5_days=as02aq40b, fam6_days=as02aq41b,
                mut_men=as02aq42b,mut_women=as02aq42c, mut_child=as02aq42d, 
                nonfam_men=as02aq43b, nonfam_women=as02aq43c, nonfam_child = as02aq43d, 
                mut_cost=as02aq42e, nonfam_cost = as02aq43e)

#add up labour days (men, women and children together)
lab_harv$harv_days <- rowSums(lab_harv[,4:15], na.rm=TRUE)
lab_harv$harv_cost <- rowSums(lab_harv[,16:17], na.rm=TRUE)

lab_harv[is.na(lab_harv)] <- 0
lab_harv <- dplyr::select(lab_harv, hid, field, parcel, harv_days, harv_cost)
lab_harv$field <- as.integer(lab_harv$field)
lab_harv$parcel <- as.integer(lab_harv$parcel)

lab <- left_join(lab_harv, lab_sp, by=c("hid","field","parcel"))

lab$lab_days = lab$harv_days + lab$soilprep_days
lab$lab_cost = lab$harv_cost + lab$soilprep_cost

#doesn't make sense to have 0 labour on a plot
lab$lab_days <- ifelse(lab$lab_days %in% 0, NA, lab$lab_days)
lab$lab_cost <- ifelse(lab$lab_days %in% 0, NA, lab$lab_cost)

lab <- dplyr::select(lab, hid, field, parcel, lab=lab_days, lab_cost)
lab$lab_cost <- lab$lab_cost 

#######################################
############## SEEDS ##################
#######################################
seeds <- read_dta(file.path(dataPath, "ecvmaas2b_p1_en.dta")) %>% 
  dplyr::select(hid, field=as02bq01, parcel=as02bq03, type=as02bq09)
seeds$hybrd <- ifelse((seeds$type==3), 1, 0)


#######################################
############### AREAS #################
#######################################
areas <-  read.csv(file.path(dataPath, "neg_areas.csv")) %>%
  dplyr::select(hid, field, parcel, area, area_proportion)

#######################################
############### REGIONS and EA GPS ####
#######################################
region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)

neggeo <-  read_dta(file.path(dataPath, "NER_EA_Offsets.dta")) %>% 
  dplyr::select(ea=grappe, lat=LAT_DD_MOD, lon=LON_DD_MOD)


#COMBINE
neg <- oput     
neg <- left_join(neg, lab, by=c("hid","field","parcel"))
neg <- left_join(neg, areas, by=c("hid","field","parcel"))
neg <- left_join(neg, fr, by=c("hid","field","parcel"))
neg <- left_join(neg, tc, by="hid")
neg <- left_join(neg, seed)
neg <- left_join(neg, ir, by=c("hid","field","parcel"))
neg <- left_join(neg, region, by="hid")
neg <- left_join(neg, neggeo)
neg <- left_join(neg, house)
neg <- neg[neg$region %in% c(1:8),]

neg$input_cost <- neg$input_cost + neg$seed_cost
neg <- dplyr::select(neg, -seed_cost)
#Variables that need to be multiplied with area: lab, lab_cost, N, P, K, input_cost, other_cost, 
neg[,c("lab","lab_cost","N","P","K","N_cost","P_cost","K_cost","input_cost","urea_valu", "dap_valu", "npk_valu", "fertmix_valu")] <- 
  neg[,c("lab","lab_cost","N","P","K","N_cost","P_cost","K_cost","input_cost","urea_valu", "dap_valu", "npk_valu", "fertmix_valu")] * neg$area_proportion

#Construct plot_id
neg$plot_id <- paste(neg$hid, neg$field, neg$parcel, sep="")

#Irrigated area
neg$irrigated_area <- neg$irrig * neg$area * neg$area_proportion

#Scale up inflation from 2011 to 2014
infl <- 1.029 * 1.005 * 1.023 * 0.992
exc_rate = 0.00161422 #29-11-2015, 20:09 from http://www.xe.com/currencyconverter/convert/?From=XOF&To=USD

neg[,c("valu", "lab_cost","input_cost","urea_valu", "dap_valu", "npk_valu", "fertmix_valu","N_cost", "P_cost", "K_cost")] <- 
  neg[,c("valu", "lab_cost","input_cost","urea_valu", "dap_valu", "npk_valu", "fertmix_valu","N_cost", "P_cost", "K_cost")] * exc_rate * infl
  