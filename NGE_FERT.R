#Set the path for storage of output 
dataPath <- "Niger/DATA"
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

pfert <- read_dta(file.path(dataPath, "ecvmaas2c_p1_en.dta")) %>%
  dplyr::select(hid, ea=grappe, type=as02cq01, typecode=as02cq02, used=as02cq03, crop=as02cq04, qty=as02cq05a, qty_unit=as02cq05b,
                purch=as02cq07, purch_qty=as02cq08a, valu=as02cq08b)
pfert <- dplyr::arrange(pfert, hid, typecode)
pfert <- data.frame(lapply(pfert, unclass))
pfert <- dplyr::filter(pfert, typecode %in% c(3,4,5,6))

pfert <- pfert[!is.na(pfert$used),] 
pfert[pfert$used==2, c("qty","qty_unit")] <- 0
pfert <- pfert[!is.na(pfert$purch),]
pfert[pfert$purch==2, c("purch_qty","valu")] <- 0

conv <- data.frame(qty_unit=c(1:11), conv=c(1,5,10,25,50,NA,NA,NA,NA,NA,NA))
pfert <- left_join(pfert, conv)
pfert$purch_qty <- pfert$purch_qty * pfert$conv
pfert$price <- pfert$purch_qty / pfert$valu

pfert <- dplyr::select(pfert, hid, ea, typecode, purch_qty, valu) 
pfert[is.na(pfert)] <- 0

pfert$urea_valu <- ifelse((pfert$typecode==3), pfert$valu, NA)
pfert$dap_valu <- ifelse((pfert$typecode==4), pfert$valu, NA)
pfert$npk_valu <- ifelse((pfert$typecode==5), pfert$valu, NA)
pfert$fertmix_valu <- ifelse((pfert$typecode==6), pfert$val, NA)

pfert[is.na(pfert)] <- 0

#Translate costs into N, P and K
pfert$N_cost <- (0.28125*pfert$dap_valu + 1*pfert$urea_valu + 0.5*pfert$npk_valu + 0.5643*pfert$fertmix_valu)
pfert$P_cost <- (0.71875*pfert$dap_valu + 0.5*pfert$npk_valu + 0.4347*pfert$fertmix_valu)
pfert$K_cost <- 0

#pfert$p_N <- ifelse((pfert$purch_qty>0), pfert$N_cost / pfert$purch_qty, NA)
#pfert$p_P <- ifelse((pfert$purch_qty>0), pfert$P_cost / pfert$purch_qty, NA)
#pfert$p_K <- ifelse((pfert$purch_qty>0), pfert$K_cost / pfert$purch_qty, NA)

neggeo <-  read_dta(file.path(dataPath, "NER_EA_Offsets.dta")) %>% 
  dplyr::select(ea=grappe, lat=LAT_DD_MOD, lon=LON_DD_MOD)
pfert <- left_join(pfert, neggeo)

region <- read_dta(file.path(dataPath, "ecvmasection00_p1_en.dta")) %>%
  dplyr::select(hid, region=ms00q10)

pfert <- left_join(pfert, region)


infl <- 1.029 * 1.005 * 1.023 * 0.992
exc_rate = 0.00161422 #29-11-2015, 20:09 from http://www.xe.com/currencyconverter/convert/?From=XOF&To=USD
pfert$valu <- pfert$valu * infl * exc_rate

pfert <- dplyr::filter(pfert, purch_qty>0) %>% 
  dplyr::select(hid, region, ea, purch_qty, N_cost, P_cost, K_cost)

#############################################################################################################
####################################### CORRECTIONS #########################################################
#############################################################################################################
pfert <- pfert[pfert$N_cost<400000,]
pfert <- pfert[pfert$purch_qty<2000,]

t_pfert_ea <- summarise(group_by(pfert, ea),
                     n = length(hid),
                     p_N = sum(N_cost, na.rm=T) / sum(purch_qty),
                     p_P = sum(P_cost, na.rm=T) / sum(purch_qty),
                     p_K = sum(K_cost, na.rm=T) / sum(purch_qty))

t_pfert_region <- summarise(group_by(pfert, region),
                            n = length(hid),
                            p_N = sum(N_cost, na.rm=T) / sum(purch_qty),
                            p_P = sum(P_cost, na.rm=T) / sum(purch_qty),
                            p_K = sum(K_cost, na.rm=T) / sum(purch_qty))
                            

write.csv(t_pfert_region, file="Niger/Output/nge_fertprices_region.csv")

                 
                     




