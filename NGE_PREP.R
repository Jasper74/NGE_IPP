#Prepping and corrections for Niger 
#This code should be executed in order to avoid 

#Judgment is based on regional means and the influence of outliers
neg <- read.csv('Niger/Output/NGE_rawdata.csv')

#Labor input and cost
nge <- dplyr::filter(nge, lab<500)
nge <- dplyr::filter(nge, region %in% 5 & lab<150 | region %in% 6 & lab<200 | region %in% 7 & lab<200 | 
                       region %in% 8 & lab<500 | !region %in% c(5,6,7,8) & lab>=0 | is.na(lab))
nge <- dplyr::filter(nge, region %in% 2 & lab_cost<80 | region %in% 8 & lab_cost<300 |
                         !region %in% c(2,8) & lab_cost>=0 | is.na(lab_cost))
                      
#N,P,K 
nge <- dplyr::filter(nge, region %in% 8 & N<60 | !region %in% 8 & N>=0 | is.na(N))
nge <- dplyr::filter(nge, region %in% 8 & K<60 | !region %in% 8 & K>=0 | is.na(K))

#N,P,K costs
nge <- dplyr::filter(nge, region %in% 3 & N_cost<250 | region %in% 5 & N_cost<600 | 
                       region %in% 6 & N_cost<80 | region %in% 7 & N_cost<400 | 
                       !region %in% c(3,5,6,7) & N_cost>=0 | is.na(N_cost))
#P_cost does not seem to need corrections

#Input costs
nge <- dplyr::filter(nge, region%in% 3 & input_cost<35 | region %in% 5 & input_cost<100 |
                       region %in% 6 & input_cost <40 | !region %in% c(3,5,6) & input_cost>=0 |
                       is.na(input_cost))
                       
#############################################################################################################
####################################### DIVIDING COSTS OVER HARVESTED KG ####################################
#############################################################################################################
totharv <- summarise(group_by(nge, hid),
                    totalharv = sum(harvest))

nge_2 <- left_join(nge, totharv)
nge_2$harvproportion <- nge_2$harvest / nge_2$totalharv

nge_2[,c("lab","lab_cost","N","P","K","N_cost","P_cost","K_cost","urea_valu","dap_valu","npk_valu","fertmix_valu","input_cost")] <- 
  nge_2[,c("lab","lab_cost","N","P","K","N_cost","P_cost","K_cost","urea_valu","dap_valu","npk_valu","fertmix_valu","input_cost")]   * nge_2$harvproportion

#############################################################################################################
####################################### ELIMINATING MULTIPLE CROPS PER PLOT #################################
#############################################################################################################
multicrop <- nge[duplicated(nge$plot_id),]
bad <- multicrop$plot_id
nge_1 <- nge[!nge$plot_id %in% bad,]

#############################################################################################################
####################################### CREATING TABLES #####################################################
#############################################################################################################

#The end result is two datasets:
#nge      plots with multiple crops are removed
#nge_2    plots with multiple crops have inputs and costs divided by harvest weight

#Creating the entire table for Niger 
write.csv(nge_1, file="Niger/Output/nge_1.csv")
write.csv(nge_2, file="Niger/Output/nge_2.csv")

#Create table for entire country
nge_national <- summarise(group_by(nge_2, cropname),
                          n = length(hid),
                          avgarea = mean(area, na.rm=T),
                          harvest = mean(harvest, na.rm=T)/mean(area, na.rm=T),
                          qty_sold = mean(qty_sold, na.rm=T),
                          valu = mean(valu, na.rm=T),
                          lab = mean(lab, na.rm=T)/mean(area, na.rm=T),
                          lab_cost = mean(lab_cost, na.rm=T)/mean(area, na.rm=T),
                          N = mean(N, na.rm=T)/mean(area, na.rm=T),
                          P = mean(P, na.rm=T)/mean(area, na.rm=T),
                          K = mean(K, na.rm=T)/mean(area, na.rm=T),
                          N_cost = mean(N_cost, na.rm=T)/mean(area, na.rm=T),
                          P_cost = mean(P_cost, na.rm=T)/mean(area, na.rm=T),
                          K_cost = mean(K_cost, na.rm=T)/mean(area, na.rm=T),
                          input_cost = mean(input_cost, na.rm=T)/mean(area, na.rm=T))
nge_national <- dplyr::arrange(nge_national, -n) 
nge_national[,3:18] <- round(nge_national[,3:18],2)

#Create table per EA
nge_ea <- summarise(group_by(nge_1, ea, cropname),
                    n = length(hid),
                    avgarea = mean(area, na.rm=T),
                    harvest = mean(harvest, na.rm=T)/mean(area, na.rm=T),
                    qty_sold = mean(qty_sold, na.rm=T),
                    valu = mean(valu, na.rm=T),
                    lab = mean(lab, na.rm=T)/mean(area, na.rm=T),
                    lab_cost = mean(lab_cost, na.rm=T)/mean(area, na.rm=T),
                    N = mean(N, na.rm=T)/mean(area, na.rm=T),
                    P = mean(P, na.rm=T)/mean(area, na.rm=T),
                    K = mean(K, na.rm=T)/mean(area, na.rm=T),
                    N_cost = mean(N_cost, na.rm=T)/mean(area, na.rm=T),
                    P_cost = mean(P_cost, na.rm=T)/mean(area, na.rm=T),
                    K_cost = mean(K_cost, na.rm=T)/mean(area, na.rm=T),
                    input_cost = mean(input_cost, na.rm=T)/mean(area, na.rm=T),
                    lat = mean(lat),
                    lon = mean(lon))
nge_ea <- dplyr::arrange(nge_ea)


#Create table per region 
nge_state <- summarise(group_by(nge_1, region),
                       n = length(hid),
                       avgarea = mean(area, na.rm=T),
                       harvest = mean(harvest, na.rm=T)/mean(area, na.rm=T),
                       qty_sold = mean(qty_sold, na.rm=T),
                       valu = mean(valu, na.rm=T),
                       lab = mean(lab, na.rm=T)/mean(area, na.rm=T),
                       lab_cost = mean(lab_cost, na.rm=T)/mean(area, na.rm=T),
                       N = mean(N, na.rm=T)/mean(area, na.rm=T),
                       P = mean(P, na.rm=T)/mean(area, na.rm=T),
                       K = mean(K, na.rm=T)/mean(area, na.rm=T),
                       N_cost = mean(N_cost, na.rm=T)/mean(area, na.rm=T),
                       P_cost = mean(P_cost, na.rm=T)/mean(area, na.rm=T),
                       K_cost = mean(K_cost, na.rm=T)/mean(area, na.rm=T),
                       input_cost = mean(input_cost, na.rm=T)/mean(area, na.rm=T),
                       lat = mean(lat),
                       lon = mean(lon))
nge_state <- dplyr::arrange(nge_state, -n)
  

write.csv(nge_national, file="Niger/Output/nge_national.csv")
write.csv(nge_state,    file="Niger/Output/nge_state.csv")
write.csv(nge_ea,       file="Niger/Output/nge_ea.csv")



