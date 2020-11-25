
# if mph > mph thresh and not NA, mph; else if c2rcc < thresh and not NA, c2rcc; else NA

library(openxlsx)

source("C:/Users/WSALLS/Git/Sent2/error_metrics_1800611.R")
#source("/Users/wilsonsalls/Desktop/Git/Sent2/error_metrics_1800611.R")


### load, prep data ###

setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/brockmann")

meris <- read.table("data_current/CRADA_MERIS_2002-2012_MA_1x1_filtered_merged_all_2020-10-27_3.txt", 
                    sep = "\t", header = TRUE)
olci <- read.table("data_current/CRADA_OLCI_2016-2019_MA_1x1_filtered_merged_all.txt", 
                   sep = "\t", header = TRUE)

meris <- meris[, -1]
olci <- olci[, -1]

#write.csv(meris, "meris.csv")
#write.csv(olci, "olci.csv")

# make new tables for each satellite, compatibly formatted so they can be merged
meris_formatted <- data.frame(insitu = meris$RESULTMEAS,
                              c2rcc = meris$zscore_result_mean_c2rcc,
                              mph = meris$zscore_result_mean_mph,
                              chl_merged_pitarch10_50 = meris$chl_merged_pitarch10_50,
                              chl_merged_pitarch15_50 = meris$chl_merged_pitarch15_50,
                              chl_merged_pitarch10_15 = meris$chl_merged_pitarch10_15,
                              satellite = "meris",
                              LATITUDE = meris$LATITUDE,
                              LONGITUDE = meris$LONGITUDE,
                              TIME = meris$TIME)

olci_formatted <- data.frame(insitu = olci$RESULTMEAS,
                             c2rcc = olci$zscore_result_mean_c2rcc,
                             mph = olci$zscore_result_mean_mph,
                             chl_merged_pitarch10_50 = olci$chl_merged_pitarch10_50,
                             chl_merged_pitarch15_50 = olci$chl_merged_pitarch15_50,
                             chl_merged_pitarch10_15 = olci$chl_merged_pitarch10_15,
                             satellite = "olci",
                             LATITUDE = olci$Latitude,
                             LONGITUDE = olci$Longitude,
                             TIME = olci$TIME)

csv <- rbind(olci_formatted, meris_formatted)
  

'  
# rename columns (actually, add new ones to retain old columns)
csv$insitu <- csv$RESULTMEAS

csv$c2rcc <- csv$zscore_result_mean_c2rcc
csv$mph <- csv$zscore_result_mean_mph
'


# remove duplicates
sum(duplicated(meris))
sum(duplicated(olci))
sum(duplicated(meris_formatted))
sum(duplicated(olci_formatted))
sum(duplicated(csv[, which(colnames(csv) %in% c("insitu", "c2rcc", "mph", "satellite", "LATITUDE", "LONGITUDE", "TIME"))]))
csv <- csv[!duplicated(csv), ]

dupdf <- cbind(csv$insitu, csv$c2rcc, csv$mph, csv$satellite, csv$LATITUDE, csv$LONGITUDE, csv$TIME)
table(dupdf)[which(table(dupdf) > 1)]

sum(is.na(csv$insitu)) # 0
#csv <- csv[-which(is.na(csv$insitu)),]

sum(is.na(csv$c2rcc)) # 1743
sum(is.na(csv$mph)) # 1657

'
sum(csv$c2rcc < 50, na.rm = TRUE) # 825 c2rcc
sum(csv$c2rcc >= 50 & csv$mph > 10, na.rm = TRUE) # 280 mph
sum(csv$c2rcc >= 50 & csv$mph <= 10, na.rm = TRUE) # 0 NA
'

# assign algorithm function
assign_alg <- function(data = NULL, mph_min = 10, c2rcc_max = 15) {
  merged_vals <- rep(NA, nrow(data))
  for (r in 1:nrow(data)) {
    if (is.na(data$mph[r])) {
      # passes to C2RCC (nothing happens here)
      
    } else if (data$mph[r] > mph_min) {
      merged_vals[r] <- data$mph[r]
      next
    } else {
      # passes to C2RCC (if MPH <= mph_min)
      
    }
    
    if (is.na(data$c2rcc[r])) {
      merged_vals[r] <- NA
    } else if (data$c2rcc[r] < c2rcc_max) {
      merged_vals[r] <- data$c2rcc[r]
    } else {
      merged_vals[r] <- NA # (if C2RCC > c2rcc_max)
    }
  }
  return(merged_vals)
}



### determine optimal thresholds: iterate through every algorithm value ###

ceiling(max(csv$c2rcc, na.rm = TRUE)) # 163
ceiling(max(csv$mph, na.rm = TRUE)) # 357


opt_df <- data.frame()

c_range <- c(1, 80) #c(0, ceiling(max(csv$c2rcc, na.rm = TRUE)))
m_range <- c(1, 80) #c(0, ceiling(max(csv$c2rcc, na.rm = TRUE)))

for (c in seq(c_range[1], c_range[2], 1)) {
  print(c)
  for (m in seq(m_range[1], m_range[2], 1)) {
    
    # assign merged algorithm values
    chl_merged <- assign_alg(data = csv, mph_min = m, c2rcc_max = c)
    
    # constrain to valid values
    valid_index <- which(!is.na(chl_merged))
    obs <- csv$insitu[valid_index]
    mod <- chl_merged[valid_index]
    
    # calculate error metrics and append to data frame
    mae <- calc_mae(observed = obs, modeled = mod, log_space = TRUE)
    bias <- calc_bias(observed = obs, modeled = mod, log_space = TRUE)
    n_valid <- sum(!is.na(chl_merged))
    
    opt_df <- rbind(opt_df, data.frame(mph_cut = m, 
                                       c2rcc_cut = c,
                                       mae = mae,
                                       bias = bias,
                                       n_valid = n_valid))
  }
}


#opt_df <- opt_df[-which(opt_df$n_valid == 0), ]

## min MAD
opt_df[which(opt_df$mae == min(opt_df$mae)), ]

# plot
library(viridis)

plot(opt_df$mph_cut, opt_df$mae, col = viridis(n = length(unique(opt_df$c2rcc_cut)))[opt_df$c2rcc_cut])
abline(v = 10)

plot(opt_df$c2rcc_cut, opt_df$mae, col = viridis(n = length(unique(opt_df$mph_cut)))[opt_df$mph_cut])
abline(v = 15)

abline(h = 1.87987)

# 3d plot
library(plot3D)
points3D(x = opt_df$c2rcc_cut, y = opt_df$mph_cut, z = opt_df$mae, xlab = "C2RCC max", ylab = "MPH min", zlab = "MAD") #, pch = "."




## min bias
opt_df[which(abs(opt_df$bias - 1) == min(abs(opt_df$bias - 1))), ]

plot(opt_df$mph_cut, opt_df$bias, col = viridis(n = length(unique(opt_df$c2rcc_cut)))[opt_df$c2rcc_cut])
abline(v = 10)

plot(opt_df$c2rcc_cut, opt_df$bias, col = viridis(n = length(unique(opt_df$mph_cut)))[opt_df$mph_cut])
abline(v = 15)

abline(h = 0.9949789)

# 3d plot
points3D(x = opt_df$c2rcc_cut, y = opt_df$mph_cut, z = opt_df$bias, xlab = "C2RCC max", ylab = "MPH min", zlab = "bias") #, pch = "."





## both
plot(1:nrow(opt_df), opt_df$mae, 
     ylim = c(min(opt_df$bias), max(opt_df$mae)),
     xlab = "max C2RCC value allowed",
     ylab = "error value") # , ylim = c(0, 2)
#lines(opt_df$c2rcc_cut, opt_df$mae)
points(opt_df$c2rcc_cut, opt_df$bias)
#lines(opt_df$c2rcc_cut, opt_df$bias)
text(20, 2.2, "MAE")
text(20, 1.2, "bias")
text(20, 25, "MAE")
text(20, 5, "bias")

opt_df[which(is.nan(opt_df$mae)), ]

#

rownames(opt_df) <- NULL

write.csv(opt_df[1:164, ], "/Users/wilsonsalls/Desktop/EPA/Brockman/algorithm_metrics_mult.csv")
write.csv(opt_df[1:164, ], "/Users/wilsonsalls/Desktop/EPA/Brockman/algorithm_metrics_linear.csv")

calc_mae(observed = csv$insitu, modeled = csv$c2rcc, log_space = TRUE)
calc_bias(observed = csv$insitu, modeled = csv$c2rcc, log_space = TRUE)

calc_mae(observed = csv$insitu[which(!is.na(csv$mph))], 
         modeled = csv$mph[which(!is.na(csv$mph))], log_space = TRUE)
calc_bias(observed = csv$insitu[which(!is.na(csv$mph))], 
          modeled = csv$mph[which(!is.na(csv$mph))], log_space = TRUE)





### checking inconsistency in splitting methodology (Brockmann vs. EPA) ####

## assign values for each merged algorithm
csv$c50m15 <- assign_alg(data = csv, c2rcc_max = 50, mph_min = 15)
csv$c50m10 <- assign_alg(data = csv, c2rcc_max = 50, mph_min = 10)
csv$c15m10 <- assign_alg(data = csv, c2rcc_max = 15, mph_min = 10)


# establish difference
csv$diff_c50m15 <-  csv$c50m15 - csv$chl_merged_pitarch15_50
csv$diff_c50m10 <-  csv$c50m10 - csv$chl_merged_pitarch10_50
csv$diff_c15m10 <-  csv$c15m10 - csv$chl_merged_pitarch10_15


## 50 15
csv$flag_c50m15 <- ""
csv$flag_c50m15[which(abs(csv$diff_c50m15) > 0.01)] <- "different value"
csv$flag_c50m15[which(!is.na(csv$c50m15) & is.na(csv$chl_merged_pitarch15_50))] <- "should have value"
csv$flag_c50m15[which(is.na(csv$c50m15) & !is.na(csv$chl_merged_pitarch15_50))] <- "should be NA"


## 50 10
csv$flag_c50m10 <- ""
csv$flag_c50m10[which(abs(csv$diff_c50m10) > 0.01)] <- "different value"
csv$flag_c50m10[which(!is.na(csv$c50m10) & is.na(csv$chl_merged_pitarch10_50))] <- "should have value"
csv$flag_c50m10[which(is.na(csv$c50m10) & !is.na(csv$chl_merged_pitarch10_50))] <- "should be NA"


## 15 10
csv$flag_c15m10 <- ""
csv$flag_c15m10[which(abs(csv$diff_c15m10) > 0.01)] <- "different value"
csv$flag_c15m10[which(!is.na(csv$c15m10) & is.na(csv$chl_merged_pitarch10_15))] <- "should have value"
csv$flag_c15m10[which(is.na(csv$c15m10) & !is.na(csv$chl_merged_pitarch10_15))] <- "should be NA"


## write table
write.csv(csv, sprintf("merging_flags_%s.csv", Sys.Date()))


## investigate

table(csv$flag_c50m15)
table(csv$flag_c50m10)
table(csv$flag_c15m10)

table(csv$diff_c50m15)
table(csv$diff_c50m10)
table(csv$diff_c15m10)


csv[which(csv$flag_c15m10 != ""), ]$mph
csv[which(csv$flag_c15m10 != ""), ]$c2rcc

sum(csv$mph <= 10, na.rm = TRUE)
sum(csv$mph <= 15, na.rm = TRUE)


# should have c2rcc
sum(is.na(with(csv, csv[mph <= 10 & c2rcc <= 15, ])$c15m10))




### scraps

'
sum(csv$c15m10 == csv$chl_merged_pitarch10_15, na.rm = TRUE)
c(sum(is.na(csv$c15m10)), sum(is.na(csv$chl_merged_pitarch10_15)))
bothValid <- sum(!is.na(csv$c15m10) & !is.na(csv$chl_merged_pitarch10_15)) # good
bothNA <- sum(is.na(csv$c15m10) & is.na(csv$chl_merged_pitarch10_15)) # good
epaValid <- sum(!is.na(csv$c15m10) & is.na(csv$chl_merged_pitarch10_15)) # bad
brockValid <- sum(is.na(csv$c15m10) & !is.na(csv$chl_merged_pitarch10_15)) # bad

# confusion matrix
conf_matr <- matrix(c(bothValid, brockValid, epaValid, bothNA), nrow = 2, byrow = FALSE)
rownames(conf_matr) <- c("Valid EPA", "NA EPA")
colnames(conf_matr) <- c("Valid Brock", "NA Brock")
conf_matr

# output data frame of problematic records
problems_c15m10 <- rbind(
  data.frame(csv[which(abs(csv$diff_c15m10) > 0.01), ], problem = "different value"), # [8]
  data.frame(csv[which(!is.na(csv$c15m10) & is.na(csv$chl_merged_pitarch10_15)), ], problem = "should have value"), # are NA but should have value [227]
  data.frame(csv[which(is.na(csv$c15m10) & !is.na(csv$chl_merged_pitarch10_15)), ], problem = "should be NA") # have values but should be NA [10]
)

write.csv(problems_c15m10, "problems_c15m10.csv")
'



## original algorithm assignment
assign_alg_OLD <- function(data = NULL, c2rcc_max = 50, mph_min = 10) {
  merged_vals <- rep(NA, nrow(data))
  for (r in 1:nrow(data)) {
    if (is.na(data$c2rcc[r])) {
      merged_vals[r] <- NA
    } else if (data$c2rcc[r] < c2rcc_max) {
      merged_vals[r] <- data$c2rcc[r]
    } else if (is.na(data$mph[r])) {
      merged_vals[r] <- NA
    } else if (data$mph[r] > mph_min) {
      merged_vals[r] <- data$mph[r]
    } else {
      merged_vals[r] <- NA
    }
  }
  return(merged_vals)
}
