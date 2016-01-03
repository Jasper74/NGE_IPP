#NIGER CROP PRICES
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
  dplyr::select(hid, order=as02eq0, field=as02eq01, parcel=as02eq03, crop=as02eq05, crop_code=as02eq06,
                harvest=as02eq07c, harvest_sold=as02eq11, sold_kg = as02eq12c, sold_revenue = as02eq13) %>%
  dplyr::filter(harvest>0)

#Filter out the observations with identical plot IDs 
oput$plot_id <- paste(oput$hid, oput$field, oput$parcel, sep="")
#oput <- oput[!duplicated(oput$plot_id),]

#PROPER CROP CODES
oput <- left_join(oput, data.frame(crop_code=c(1:48), cropname = c("Millet","Sorghum","Paddy_rice","Maize","Souchet","Wheat","Fonio","Cowpeas","Voandzou","Peanuts",
                                                                   "Gombo","Sorrel","Sesame","Cassava","Sweet_potato","Potato","Pepper","Ginger","Cloves","Mint",
                                                                   "Spinach","Celery","Parsley","Spice(pepper)","Melon","Watermelon","Lettuce","Cabbage","Tomato","Carrot",
                                                                   "Jaxatu","Eggplant","Onion","Cucumber","Squash","Garlic","Green_beans","Gourd","Radish","Turnip",
                                                                   "Leeks","Amarante","Cotton","Beets","Peas","Taro","Yams","Other")))
#If plot is indicated not to have a harvest, the quantity sold is also 0, same goes for revenue
oput$harvest_sold_kg[oput$harvest_sold==2] <- 0
oput$sold_revenue[oput$harvest_sold==2] <- 0
#We're only interested in having observations with sales observations
sales <- dplyr::filter(oput, harvest_sold==1 & !is.na(sold_revenue))     
#nonsales <- dplyr::filter(oput, harvest_sold!=1) #check that sales volume and revenue are 0 (2 strange observations)

region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)
sales <- left_join(sales, region)
sales <- dplyr::filter(sales, sold_revenue<100000)
sales$price <- sales$sold_revenue / sales$sold_kg

sales_bycrop <- group_by(sales, cropname)
nge_cropprices_national <- summarise(sales_bycrop,
               n = length(hid),
               crop_prc = sum(sold_revenue, na.rm=T)/sum(sold_kg, na.rm=T))
  
sales_bycropregion <- group_by(sales, region, cropname)
nge_cropprices_state <- summarise(sales_bycropregion,
                 n = length(hid),
                 crop_prc = sum(sold_revenue, na.rm=T)/sum(sold_kg, na.rm=T))

write.csv(nge_cropprices_national, file="Niger/Output/NGE_cropprices_national.csv")
write.csv(nge_cropprices_state, file="Niger/Output/NGE_cropprices_state.csv")

                 
