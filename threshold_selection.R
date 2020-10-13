
# if mph > mph thresh and not NA, mph; else if c2rcc < thresh and not NA, c2rcc; else NA

library(openxlsx)

source("C:/Users/WSALLS/Git/Sent2/error_metrics_1800611.R")
#source("/Users/wilsonsalls/Desktop/Git/Sent2/error_metrics_1800611.R")


## load data
'
#setwd("O:/PRIV/NERL_ORD_CYAN/Brockmann_CRADA/AlgorithmDetermination")
setwd("/Users/wilsonsalls/Desktop/EPA/Brockman")
csv <- read.csv("match_ups_filtered_1x1_c2rccv2_mphpitarch_merged_epa.csv",
                stringsAsFactors = FALSE)

colnames(csv)
colnames(csv)[which(colnames(csv) == "in.situ.CHL")] <- "insitu"
colnames(csv)[which(colnames(csv) == "c2rcc_v2_valid_central_pixel")] <- "c2rcc"
colnames(csv)[which(colnames(csv) == "mph_pitarch_valid_central_pixel")] <- "mph"
'

## OR try it with most recent data
'
setwd("O:/PRIV/NERL_ORD_CYAN/Brockmann_CRADA/AlgorithmAssessment")
csv <- read.xlsx("Brockmann_chla_validation.xlsx")

colnames(csv)
colnames(csv)[which(colnames(csv) == "in-situ.CHL")] <- "insitu"
colnames(csv)[which(colnames(csv) == "conc_chl_valid_central_pixel")] <- "c2rcc"
colnames(csv)[which(colnames(csv) == "chl_pitarch_valid_central_pixel")] <- "mph"
'

## even more recent data

setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/brockmann")
meris <- read.xlsx("data_current/CRADA_MERIS_2002-2012_MA_1x1_filtered_merged_all.xlsx")
olci <- read.xlsx("data_current/CRADA_OLCI_2016-2019_MA_1x1_filtered_merged_all.xlsx")

#csv <- meris
csv <- olci

colnames(csv)[which(colnames(csv) == "chl_pitarch_valid_central_pixel")] <- "mph"
colnames(csv)[which(colnames(csv) == "conc_chl_valid_central_pixel")] <- "c2rcc"
colnames(csv)[which(colnames(csv) == "in-situ.CHL")] <- "insitu"


##

## remove duplicates; 
sum(duplicated(csv))
sum(duplicated(csv[which(colnames(csv) %in% c("mph", "c2rcc", "insitu")), ]))
#csv <- csv[!duplicated(csv), ]

sum(is.na(csv$insitu)) # 0
#csv <- csv[-which(is.na(csv$insitu)),]

sum(is.na(csv$c2rcc)) # 36
sum(is.na(csv$mph)) # 512

'
sum(csv$c2rcc < 50, na.rm = TRUE) # 825 c2rcc
sum(csv$c2rcc >= 50 & csv$mph > 10, na.rm = TRUE) # 280 mph
sum(csv$c2rcc >= 50 & csv$mph <= 10, na.rm = TRUE) # 0 NA
'

# assign algorithm function
assign_alg <- function(data = NULL, mph_min = 10, c2rcc_max = 50) {
  merged_vals <- rep(NA, nrow(data))
  for (r in 1:nrow(data)) {
    if (is.na(data$mph[r])) {
      # passes to C2RCC
      #merged_vals[r] <- NA
    } else if (data$mph[r] > mph_min) {
      merged_vals[r] <- data$mph[r]
      next
    } else {
      # passes to C2RCC
      #merged_vals[r] <- NA
    }
    
    if (is.na(data$c2rcc[r])) {
      merged_vals[r] <- NA
    } else if (data$c2rcc[r] < c2rcc_max) {
      merged_vals[r] <- data$c2rcc[r]
    } else {
      merged_vals[r] <- NA
    }
  }
  return(merged_vals)
}


#

ceiling(max(csv$c2rcc, na.rm = TRUE)) # 163
ceiling(max(csv$mph, na.rm = TRUE)) # 492


alg_df <- data.frame()

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
    
    alg_df <- rbind(alg_df, data.frame(mph_cut = m, 
                                       c2rcc_cut = c,
                                       mae = mae,
                                       bias = bias,
                                       n_valid = n_valid))
  }
}


#alg_df <- alg_df[-which(alg_df$n_valid == 0), ]

library(viridis)

## min MAD
alg_df[which(alg_df$mae == min(alg_df$mae)), ]

plot(alg_df$mph_cut, alg_df$mae, col = viridis(n = length(unique(alg_df$c2rcc_cut)))[alg_df$c2rcc_cut])
abline(v = 10)

plot(alg_df$c2rcc_cut, alg_df$mae, col = viridis(n = length(unique(alg_df$mph_cut)))[alg_df$mph_cut])
abline(v = 15)

abline(h = 1.87987)

# 3d plot
library(plot3D)
points3D(x = alg_df$c2rcc_cut, y = alg_df$mph_cut, z = alg_df$mae, xlab = "C2RCC max", ylab = "MPH min", zlab = "MAD", pch = ".")




## min bias
alg_df[which(alg_df$bias == max(alg_df$bias)), ]

plot(alg_df$mph_cut, alg_df$bias, col = viridis(n = length(unique(alg_df$c2rcc_cut)))[alg_df$c2rcc_cut])
abline(v = 10)

plot(alg_df$c2rcc_cut, alg_df$bias, col = viridis(n = length(unique(alg_df$mph_cut)))[alg_df$mph_cut])
abline(v = 15)

abline(h = 0.9949789)

# 3d plot
points3D(x = alg_df$c2rcc_cut, y = alg_df$mph_cut, z = alg_df$bias, xlab = "C2RCC max", ylab = "MPH min", zlab = "bias", pch = ".")





## both
plot(1:nrow(alg_df), alg_df$mae, 
     ylim = c(min(alg_df$bias), max(alg_df$mae)),
     xlab = "max C2RCC value allowed",
     ylab = "error value") # , ylim = c(0, 2)
#lines(alg_df$c2rcc_cut, alg_df$mae)
points(alg_df$c2rcc_cut, alg_df$bias)
#lines(alg_df$c2rcc_cut, alg_df$bias)
text(20, 2.2, "MAE")
text(20, 1.2, "bias")
text(20, 25, "MAE")
text(20, 5, "bias")

alg_df[which(is.nan(alg_df$mae)), ]

#

rownames(alg_df) <- NULL

write.csv(alg_df[1:164, ], "/Users/wilsonsalls/Desktop/EPA/Brockman/algorithm_metrics_mult.csv")
write.csv(alg_df[1:164, ], "/Users/wilsonsalls/Desktop/EPA/Brockman/algorithm_metrics_linear.csv")

calc_mae(observed = csv$insitu, modeled = csv$c2rcc, log_space = TRUE)
calc_bias(observed = csv$insitu, modeled = csv$c2rcc, log_space = TRUE)

calc_mae(observed = csv$insitu[which(!is.na(csv$mph))], 
         modeled = csv$mph[which(!is.na(csv$mph))], log_space = TRUE)
calc_bias(observed = csv$insitu[which(!is.na(csv$mph))], 
          modeled = csv$mph[which(!is.na(csv$mph))], log_space = TRUE)



##### checking inconsistency in splitting methodology (Brockmann vs. EPA)

## assign values for each merged algorithm
csv$c50m10 <- assign_alg(data = csv, c2rcc_max = 50, mph_min = 10)
csv$c50m15 <- assign_alg(data = csv, c2rcc_max = 50, mph_min = 15)
csv$c15m10 <- assign_alg(data = csv, c2rcc_max = 15, mph_min = 10)


# establish difference
csv$diff_c50m10 <-  csv$c50m10 - csv$chl_merged_pitarch10_50
csv$diff_c50m15 <-  csv$c50m15 - csv$chl_merged_pitarch15_50
csv$diff_c15m10 <-  csv$c15m10 - csv$chl_merged_pitarch10_15


## 50 10
csv$flag_c50m10 <- ""
csv$flag_c50m10[which(abs(csv$diff_c50m10) > 0.01)] <- "different value"
csv$flag_c50m10[which(!is.na(csv$c50m10) & is.na(csv$chl_merged_pitarch10_50))] <- "should have value"
csv$flag_c50m10[which(is.na(csv$c50m10) & !is.na(csv$chl_merged_pitarch10_50))] <- "should be NA"


## 50 15
csv$flag_c50m15 <- ""
csv$flag_c50m15[which(abs(csv$diff_c50m15) > 0.01)] <- "different value"
csv$flag_c50m15[which(!is.na(csv$c50m15) & is.na(csv$chl_merged_pitarch15_50))] <- "should have value"
csv$flag_c50m15[which(is.na(csv$c50m15) & !is.na(csv$chl_merged_pitarch15_50))] <- "should be NA"


## 15 10
csv$flag_c15m10 <- ""
csv$flag_c15m10[which(abs(csv$diff_c15m10) > 0.01)] <- "different value"
csv$flag_c15m10[which(!is.na(csv$c15m10) & is.na(csv$chl_merged_pitarch10_15))] <- "should have value"
csv$flag_c15m10[which(is.na(csv$c15m10) & !is.na(csv$chl_merged_pitarch10_15))] <- "should be NA"


write.csv(csv, "merging_flags_OLCI.csv")



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
