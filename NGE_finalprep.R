dataPath <- "Niger/DATA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

#Conveyance method: 1 Gravity   2 Discharge (pump)   3 Manual Scooping    4 Drip   5 Other (specify) 
#costs per hectare  $50[2]      $259 [1]             $0 [2]               $50[2]   $0 (method not specified)

# [1] Xie et al. (2014); costs given in dollars and therefore do not need to be converted using exchange rates 
# [2] Own estimation
# 

#IRRIGATION (only the convey method costs are estimated, not the water source)
irrigation_cost_convey_method <- data.frame(convey_method=c(1:5), irrigationcost=c(50, 259, 0, 50, 0))
irrigation_cost_convey_method2 <- data.frame(convey_method2=c(1:5), irrigationcost2=c(50, 259, 0, 50, 0))

nge_v2 <- read.csv('Niger/Output/nge_v2') %>% dplyr::select(-X)
nge2_v2 <- read.csv('Niger/Output/nge2_v2') %>% dplyr::select(-X)

#Binding
ngec <- rbind(nge_v2, nge2_v2)
ngec_backup <- ngec

ngec <- left_join(ngec, irrigation_cost_convey_method)
ngec <- left_join(ngec, irrigation_cost_convey_method2)
ngec$irrigationcost2[!is.na(ngec$irrigationcost)] <- 0
ngec$irrigation_cost <- ngec$irrigationcost + ngec$irrigationcost2
ngec$irrigation_cost[ngec$season=='rainy' & !is.na(ngec$water_source)] <- 0
ngec <- dplyr::select(ngec, -irrigationcost, -irrigationcost2)

#Corrections (only for final variables)
ngec <- dplyr::filter(ngec, harvest_kg>=0) 
ngec <- dplyr::filter(ngec, familylabor_days<400 | is.na(familylabor_days))
ngec <- dplyr::filter(ngec, hiredlabor_days<150 | is.na(hiredlabor_days))
ngec <- dplyr::filter(ngec, hiredlabor_cost<300 | is.na(hiredlabor_cost))
ngec <- dplyr::filter(ngec, N_kg<150 | is.na(N_kg))
ngec <- dplyr::filter(ngec, P_kg<80 | is.na(P_kg))
ngec <- dplyr::filter(ngec, P_cost<250 | is.na(P_cost))
ngec <- dplyr::filter(ngec, input_cost<1500 | is.na(input_cost))
ngec <- dplyr::filter(ngec, seed_cost<300 | is.na(seed_cost))
ngec <- dplyr::filter(ngec, total_cost<1200 | is.na(total_cost))
ngec <- dplyr::filter(ngec, pricecost_ratio<1 | is.na(pricecost_ratio))

ngec$unique_id <- paste(ngec$hid, ngec$field, ngec$parcel, ngec$wave, ngec$cropname,  sep="")
ngec$plot_id <- paste(ngec$hid, ngec$field, ngec$parcel, ngec$cropname,  sep="")

test <- ngec$plot_id[duplicated(ngec$plot_id)] 
test <- ngec[ngec$plot_id %in% test,]
test <- dplyr::filter(ngec, plot_id %in% b)
b <- aggregate(test[11,c(13:)], by=list(test$plot_id), FUN=sum)



b  <- ngec$unique_id[duplicated(ngec$unique_id)]
b <- ngec[ngec$unique_id %in% b,]

ngec <- dplyr::filter(ngec, !is.na(irrigati))

t_NGE <- summarise(group_by(ngec, cropname, irrigation_use),
                   n = length(hid),
                   familylabor_days = mean(familylabor_days, na.rm=T),
                   hiredlabor_days = mean(hiredlabor_days, na.rm=T),
                   hiredlabor_cost = mean(hiredlabor_cost, na.rm=T),
                   N_kg = mean(N_kg, na.rm=T),
                   P_kg = mean(P_kg, na.rm=T),
                   K_kg = mean(K_kg, na.rm=T),
                   N_cost = mean(N_cost, na.rm=T),
                   P_cost = mean(P_cost, na.rm=T),
                   K_cost = mean(K_cost, na.rm=T),
                   irrigation_cost = mean(irrigation_cost, na.rm=T),
                   input_cost = mean(input_cost, na.rm=T),
                   seed_cost = mean(input_cost, na.rm=T),
                   total_cost = mean(total_cost, na.rm=T),
                   crop_cost_perkg = mean(crop_cost_perkg, na.rm=T),
                   pricecost_ratio = mean(pricecost_ratio, na.rm=T))

write.csv(t_NGE, 'Niger/Output/NGE_summary_percrop.csv')


