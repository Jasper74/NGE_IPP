
dataPath <- "Niger/DATA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

nge <- read.csv('Niger/Output/NGE_contre_rawdata.csv')
nge$season = 'contre'

#=========================================================================================================
#labor price
p_lab <- read.csv('Niger/Output/laborprices.csv') %>%
  dplyr::select(ea, p_labor)
nge <- left_join(nge, p_lab) 
nge$p_labor <- ifelse((is.na(nge$p_labor)), mean(nge$p_labor, na.rm=T), nge$p_labor)
nge$household_hiredlabor_cost <- ifelse((nge$household_hiredlabor_cost==0 & nge$household_hiredlabor_days>0), (nge$household_hiredlabor_days * nge$p_labor), nge$household_hiredlabor_cost)
#=========================================================================================================
#Crop prices
#Calculate prices for each crop 
crop_prices <- dplyr::select(nge, ea, region, cropname, sold_volume_kg, sold_value)
crop_prices$price <- crop_prices$sold_value / crop_prices$sold_volume_kg 
crop_prices <- dplyr::filter(crop_prices, price<4)
crop_price_table <- summarise(group_by(crop_prices, cropname),
                              p_crop_perkg = mean(price, na.rm=T))

nge <- left_join(nge, crop_price_table)
#national_pcrop <- read.csv('Niger/Output/NGE_cropprices_national.csv') %>%
#  dplyr::select(cropname, p_crop_perkg=crop_prc)
#nge <- left_join(nge, national_pcrop, by="cropname")

#Calculate quantities sold 
nge$total_sold_kg <- nge$harvest_kg
nge$crop_revenue <- nge$total_sold_kg * nge$p_crop_perkg
nge$crop_revenue[is.na(nge$crop_revenue)] <- 0

#Calculate total rev per plot and per household 
plot_revenue <- summarise(group_by(nge, hid, parcel, field),
                          total_plot_revenue = sum(crop_revenue, na.rm=T))
household_revenue <-  summarise(group_by(nge, hid),
                                total_household_revenue = sum(crop_revenue, na.rm=T))

nge <- left_join(nge, plot_revenue)
nge <- left_join(nge, household_revenue)

#Share of crop revenue in total plot revenue
nge$cropshare_revenue<- nge$crop_revenue / nge$total_plot_revenue
#Share of plot revenue in total household revenue
nge$plotshare_revenue <- nge$total_plot_revenue / nge$total_household_revenue 

#CONVERT HOUSEHOLD COSTS TO COSTS PER PLOT
nge$plot_familylabor_days <- nge$household_familylabor_days * nge$plotshare_revenue
nge$plot_hiredlabor_days <- nge$household_hiredlabor_days * nge$plotshare_revenue
nge$plot_hiredlabor_cost <- nge$household_hiredlabor_cost * nge$plotshare_revenue

nge$plot_urea_kg <- nge$household_urea_kg * nge$plotshare_revenue
nge$plot_dap_kg <- nge$household_dap_kg * nge$plotshare_revenue
nge$plot_npk_kg <- nge$household_npk_kg * nge$plotshare_revenue
nge$plot_fertmix_kg <- nge$household_fertmix_cost * nge$plotshare_revenue

nge$plot_N_kg <- nge$household_N_kg * nge$plotshare_revenue
nge$plot_P_kg <- nge$household_P_kg * nge$plotshare_revenue
nge$plot_K_kg <- nge$household_K_kg * nge$plotshare_revenue

nge$plot_urea_cost <- nge$household_urea_cost * nge$plotshare_revenue
nge$plot_dap_cost <- nge$household_dap_cost * nge$plotshare_revenue
nge$plot_npk_cost <- nge$household_npk_cost * nge$plotshare_revenue
nge$plot_fertmix_cost <- nge$household_fertmix_cost * nge$plotshare_revenue

nge$plot_N_cost <- nge$household_N_cost * nge$plotshare_revenue
nge$plot_P_cost <- nge$household_P_cost * nge$plotshare_revenue
nge$plot_K_cost <- nge$household_K_cost * nge$plotshare_revenue

nge$plot_input_cost <- nge$household_input_cost * nge$plotshare_revenue

#CONVERT PLOT COSTS TO COSTS PER CROP
nge$familylabor_days <- nge$plot_familylabor_days * nge$cropshare_revenue
nge$hiredlabor_days <- nge$plot_hiredlabor_days * nge$cropshare_revenue
nge$hiredlabor_cost <- nge$plot_hiredlabor_cost * nge$cropshare_revenue

nge$urea_kg <- nge$plot_urea_kg * nge$cropshare_revenue
nge$dap_kg <- nge$plot_dap_kg * nge$cropshare_revenue
nge$npk_kg <- nge$plot_npk_kg * nge$cropshare_revenue
nge$fertmix_kg <- nge$plot_fertmix_kg * nge$cropshare_revenue

nge$N_kg <- nge$plot_N_kg * nge$cropshare_revenue
nge$P_kg <- nge$plot_P_kg * nge$cropshare_revenue
nge$K_kg <- nge$plot_K_kg * nge$cropshare_revenue

nge$urea_cost <- nge$plot_urea_cost * nge$cropshare_revenue
nge$dap_cost <- nge$plot_dap_cost * nge$cropshare_revenue
nge$npk_cost <- nge$plot_npk_cost * nge$cropshare_revenue
nge$fertmix_cost <- nge$plot_fertmix_cost * nge$cropshare_revenue

nge$N_cost <- nge$plot_N_cost * nge$cropshare_revenue
nge$P_cost <- nge$plot_P_cost * nge$cropshare_revenue
nge$K_cost <- nge$plot_K_cost * nge$cropshare_revenue

nge$input_cost <- nge$plot_input_cost * nge$cropshare_revenue

nge$seed_cost <- ifelse((is.na(nge$seed_cost) & !is.na(nge$input_cost)), 0, nge$seed_cost)
nge <- within(nge, total_cost <- N_cost + P_cost + K_cost + input_cost + seed_cost)
nge$crop_cost_perkg <- nge$total_cost / nge$total_sold_kg

nge$pricecost_ratio <- nge$crop_cost_perkg / nge$p_crop_perkg

nge2 <- nge
#Getting the goods lined up for some good binding

nge2_v2 <- dplyr::select(nge, hid, ea, field, parcel, region, lat, lon, season, cropname, crop_code, 
                        harvest_kg, harv_area_m2, p_crop_perkg, crop_revenue, total_plot_revenue, 
                        total_household_revenue, plotshare_revenue, cropshare_revenue,
                        plot_familylabor_days, familylabor_days, 
                        plot_hiredlabor_days, hiredlabor_days,
                        plot_hiredlabor_cost, hiredlabor_cost,
                        plot_urea_kg, urea_kg, plot_dap_kg, dap_kg, plot_npk_kg, npk_kg, plot_fertmix_kg, fertmix_kg, 
                        plot_N_kg, N_kg, plot_P_kg, P_kg, plot_K_kg, K_kg,
                        plot_urea_cost, urea_cost, plot_dap_cost, dap_cost, plot_npk_cost, npk_cost, plot_fertmix_cost, fertmix_cost,
                        plot_N_cost, N_cost, plot_P_cost, P_cost, plot_K_cost, K_cost,
                        plot_input_cost, input_cost, seed_cost,
                        water_source, convey_method, water_source2, convey_method2,
                        total_cost, crop_cost_perkg, pricecost_ratio)

write.csv(nge2_v2, 'Niger/Output/nge2_v2') 







