#NGR AREAS
#Set the path for storage of output 
dataPath <- "Niger/DATA"
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)

#Load data 
ar <-  read_dta(file.path(dataPath, "ecvmaas1_p1_en.dta")) %>%  
  dplyr::select(hid, field=as01q03, parcel=as01q05, est=as01q08,
                gps=as01q09, worked=as02aq05)
ar[,4:5] <- ar[,4:5]/10000

ar <- left_join(ar, region)

ar$area <- ar$gps
ar$area <- ifelse((ar$gps>80 & ar$est<20), ar$est, ar$gps)
ar$area <- ifelse((ar$gps==0 & ar$est<20), ar$est, ar$gps)

#AREA CORRECTION
ar$area[ar$gps>80 & ar$region==1] <- mean(ar$gps[ar$gps<20 & ar$region==1], na.rm=T)
ar$area[ar$gps>80 & ar$region==2] <- mean(ar$gps[ar$gps<20 & ar$region==2], na.rm=T)
ar$area[ar$gps>80 & ar$region==3] <- mean(ar$gps[ar$gps<20 & ar$region==3], na.rm=T)
ar$area[ar$gps>80 & ar$region==4] <- mean(ar$gps[ar$gps<20 & ar$region==4], na.rm=T)
ar$area[ar$gps>80 & ar$region==5] <- mean(ar$gps[ar$gps<20 & ar$region==5], na.rm=T)
ar$area[ar$gps>80 & ar$region==6] <- mean(ar$gps[ar$gps<20 & ar$region==6], na.rm=T)
ar$area[ar$gps>80 & ar$region==7] <- mean(ar$gps[ar$gps<20 & ar$region==7], na.rm=T)
ar$area[ar$gps>80 & ar$region==8] <- mean(ar$gps[ar$gps<20 & ar$region==8], na.rm=T)

tot_area <- summarise(group_by(ar, hid),
                      tot_area = sum(area, na.rm=T))
ar <- left_join(ar, tot_area)
ar$area_proportion <- ar$area / ar$tot_area
ar <- dplyr::select(ar, hid, field, parcel, area, area_proportion)

#WINSORIZING
lim <- stats::quantile(ar$area, probs = 0.97, na.rm = TRUE)
ar[, 'area'][ar[, 'area'] > lim] <- lim 

write.csv(ar, file=paste(dataPath,'/nge_areas.csv',sep=""))
