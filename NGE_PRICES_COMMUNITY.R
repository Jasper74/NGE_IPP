#NIGER PRICES
#Set the path for storage of output 
dataPath <- "Niger/DATA"
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

exc_rate = 0.00161422 #29-11-2015, 20:09

price1 <-  read_dta(file.path(dataPath, "ecvmacoms07_p1_en.dta")) %>%
  dplyr::select(grappe, product_code=cs07q01, product_name=cs07q01l, product_variety_code=cs07q02, product_variety_name=cs07q02l,
                price_1=cs07q03, weight_1=cs07q04, unit1=cs07q05, price_2=cs07q06, weight_2=cs07q07, unit2=cs07q08,
                price_3=cs07q09, weight_3=cs07q10, unit3=cs07q11)
#We're only interested in prices for agricultural commodities (7011-7054)
price1 <- dplyr::filter(price1, product_code==701 | product_code==702 | product_code==703 | 
                           product_code==705)

prod_var_names = data.frame(product_variety_code = c(
  
                 c("whitemaize_100kg","yellowmaize_100kg","whitemaize_tia","yellowmaize_tia",
                   "millet_100kg","millet_tia",
                   "thairice_kg","localrice_kg",
                   "whitesorghum_100kg","redsorghum_100kg",
                   "whitesorghum_tia","redsorghum_tia")

#Product varieties are specified, making it difficult to relate crop prices to the harvests, e.g. harvest volumes
#only indicates 'maize' but the local price section indicates 'white' maize and 'yellow' maize 

price1$unit1 <- as.numeric(as.character(price1$unit1))
price1$unit2 <- as.numeric(as.character(price1$unit2))
price1$unit3 <- as.numeric(as.character(price1$unit3))

conv1 <- data.frame(unit1 = c(1,2,3,4,5,6,7,9), conv1=c(1, 0.001, NA, NA, 50, 100, NA, NA))
conv2 <- data.frame(unit2 = c(1,2,3,4,5,6,7,9), conv2=c(1, 0.001, NA, NA, 50, 100, NA, NA))
conv3 <- data.frame(unit3 = c(1,2,3,4,5,6,7,9), conv3=c(1, 0.001, NA, NA, 50, 100, NA, NA))

price1 <- left_join(price1, conv1)
price1 <- left_join(price1, conv2)
price1 <- left_join(price1, conv3)

#convert to $ ***NOT COMPLETED***
price1$p1 <- ((price1$price_1 / (price1$weight_1 * price1$unit1))*1000) * exc_rate
price1$p2 <- ((price1$price_2 / (price1$weight_2 * price1$unit2))*1000) * exc_rate 
price1$p3 <- ((price1$price_3 / (price1$weight_3 * price1$unit3))*1000) * exc_rate

#Obtain mean of prices from three columns
price1$p1[is.na(price1$p1)] <- 0
price1$p2[is.na(price1$p2)] <- 0
price1$p3[is.na(price1$p3)] <- 0

price1$region[price1$grappe>=0 & price1$grappe<=27] <- 1
price1$region[price1$grappe>=28 & price1$grappe<=50] <- 2
price1$region[price1$grappe>=51 & price1$grappe<=76] <- 3
price1$region[price1$grappe>=77 & price1$grappe<=106] <- 4
price1$region[price1$grappe>=107 & price1$grappe<=184] <- 5
price1$region[price1$grappe>=185 & price1$grappe<=213] <- 6
price1$region[price1$grappe>=214 & price1$grappe<=238] <- 7
price1$region[price1$grappe>=239 & price1$grappe<=270] <- 8

price1$totobs <- 0
price1$totobs <- ifelse((price1$p1>0), price1$totobs+1, 0)
price1$totobs <- ifelse((price1$p2>0), price1$totobs+1, price1$totobs)
price1$totobs <- ifelse((price1$p3>0), price1$totobs+1, price1$totobs)

price1$price <- (price1$p1 + price1$p2 + price1$p3)/price1$totobs

price_by_reg <- group_by(price1, region, product_variety_name)
t_price <- summarise(price_by_reg,
                     n = length(price[!is.na(price)]),
                     price = mean(price, na.rm=T))

#Do the same for prices of the second visit
price2 <-  read_dta(file.path(dataPath, "ecvmacoms07_p2_en.dta")) %>%
  dplyr::select(grappe, product_code=cs07q01, product_name=cs07q01l, product_variety_code=cs07q02, product_variety_name=cs07q02l,
                price_1=cs07q03, weight_1=cs07q04, unit1=cs07q05, price_2=cs07q06, weight_2=cs07q07, unit2=cs07q08,
                price_3=cs07q09, weight_3=cs07q10, unit3=cs07q11)
#We're only interested in prices for agricultural commodities (7011-7054)
price2 <- dplyr::filter(price2, product_code==701 | product_code==702 | product_code==703 | 
                          product_code==704 | product_code==705)

#Product varieties are specified, making it difficult to relate crop prices to the harvests, e.g. harvest volumes
#only indicates 'maize' but the local price section indicates 'white' maize and 'yellow' maize 
price2$unit1 <- as.numeric(as.character(price2$unit1))
price2$unit2 <- as.numeric(as.character(price2$unit2))
price2$unit3 <- as.numeric(as.character(price2$unit3))

conv1 <- data.frame(unit1 = c(1,2,3,4,5,6,7,9), conv1=c(1, 0.001, NA, NA, 50, 100, NA, NA))
conv2 <- data.frame(unit2 = c(1,2,3,4,5,6,7,9), conv2=c(1, 0.001, NA, NA, 50, 100, NA, NA))
conv3 <- data.frame(unit3 = c(1,2,3,4,5,6,7,9), conv3=c(1, 0.001, NA, NA, 50, 100, NA, NA))

price2 <- left_join(price2, conv1)
price2 <- left_join(price2, conv2)
price2 <- left_join(price2, conv3)

#convert to $ ***NOT COMPLETED***
price2$price_t1 <- ((price2$price_1 / (price2$weight_1 * price2$unit1))*1000) * exc_rate
price2$price_t2 <- ((price2$price_2 / (price2$weight_2 * price2$unit2))*1000) * exc_rate 
price2$price_t3 <- ((price2$price_3 / (price2$weight_3 * price2$unit3))*1000) * exc_rate

#Obtain mean of prices from three columns
price2$price_t1[is.na(price2$price_t1)] <- 0
price2$price_t2[is.na(price2$price_t2)] <- 0
price2$price_t3[is.na(price2$price_t3)] <- 0

price2$region[price2$grappe>=0 & price2$grappe<=27] <- 1
price2$region[price2$grappe>=28 & price2$grappe<=50] <- 2
price2$region[price2$grappe>=51 & price2$grappe<=76] <- 3
price2$region[price2$grappe>=77 & price2$grappe<=106] <- 4
price2$region[price2$grappe>=107 & price2$grappe<=184] <- 5
price2$region[price2$grappe>=185 & price2$grappe<=213] <- 6
price2$region[price2$grappe>=214 & price2$grappe<=238] <- 7
price2$region[price2$grappe>=239 & price2$grappe<=270] <- 8

price2$totobs <- 0
price2$totobs <- ifelse((price2$price_t1>0), price2$totobs+1, 0)
price2$totobs <- ifelse((price2$price_t2>0), price2$totobs+1, price2$totobs)
price2$totobs <- ifelse((price2$price_t3>0), price2$totobs+1, price2$totobs)

price2$price <- (price2$price_t1 + price2$price_t2 + price2$price_t3)/price2$totobs

price_by_reg <- group_by(price2, region, product_variety_name)
t_price <- summarise(price_by_reg,
                     n = length(price[!is.na(price)]),
                     price = mean(price, na.rm=T))
