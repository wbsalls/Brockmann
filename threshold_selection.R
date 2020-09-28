
# if c2rcc < 50, c2rcc; else if mph > 10, mph; else NA

library(openxlsx)

source("C:/Users/WSALLS/Git/Sent2/error_metrics_1800611.R")
#source("/Users/wilsonsalls/Desktop/Git/Sent2/error_metrics_1800611.R")


## load data
#setwd("O:/PRIV/NERL_ORD_CYAN/Brockmann_CRADA/AlgorithmDetermination")
setwd("/Users/wilsonsalls/Desktop/EPA/Brockman")
csv <- read.csv("match_ups_filtered_1x1_c2rccv2_mphpitarch_merged_epa.csv",
                stringsAsFactors = FALSE)

colnames(csv)
colnames(csv)[which(colnames(csv) == "in.situ.CHL")] <- "insitu"
colnames(csv)[which(colnames(csv) == "c2rcc_v2_valid_central_pixel")] <- "c2rcc"
colnames(csv)[which(colnames(csv) == "mph_pitarch_valid_central_pixel")] <- "mph"

## OR try it with most recent data
setwd("O:/PRIV/NERL_ORD_CYAN/Brockmann_CRADA/AlgorithmAssessment")
csv <- read.xlsx("Brockmann_chla_validation.xlsx")

colnames(csv)
colnames(csv)[which(colnames(csv) == "in-situ.CHL")] <- "insitu"
colnames(csv)[which(colnames(csv) == "conc_chl_valid_central_pixel")] <- "c2rcc"
colnames(csv)[which(colnames(csv) == "chl_pitarch_valid_central_pixel")] <- "mph"

##

sum(duplicated(data.frame(csv$insitu, csv$c2rcc, csv$mph)))

#csv <- csv[-which(is.na(csv$insitu)),]

sum(is.na(csv$insitu)) # 1
sum(is.na(csv$c2rcc)) # 1
sum(is.na(csv$mph)) # 492

sum(csv$c2rcc < 50, na.rm = TRUE) # 825 c2rcc
sum(csv$c2rcc >= 50 & csv$mph > 10, na.rm = TRUE) # 280 mph
sum(csv$c2rcc >= 50 & csv$mph <= 10, na.rm = TRUE) # 0 NA

# assign algorithm function
assign_alg <- function(data = NULL, c2rcc_max = 50, mph_min = 10) {
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


#

alg_df <- data.frame()

for (n in seq(0, 170, 1)) {
  if (n < 15) {
    mph_min <- n
  } else {
    mph_min <- 15
  }
  
  chl_merged <- assign_alg(data = csv, c2rcc_max = n, mph_min = mph_min)
  
  valid_index <- which(!is.na(chl_merged))
  
  obs <- csv$insitu[valid_index]
  mod <- chl_merged[valid_index]
  
  mae <- calc_mae(observed = obs, modeled = mod, log_space = TRUE)
  bias <- calc_bias(observed = obs, modeled = mod, log_space = TRUE)
  n_valid <- sum(!is.na(chl_merged))
  
  alg_df <- rbind(alg_df, data.frame(c2rcc_cut = n,
                                     mae = mae,
                                     bias = bias,
                                     n_valid = n_valid))
}

plot(alg_df$c2rcc_cut, alg_df$mae, 
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

alg_df$c2rcc_cut[which(alg_df$mae == min(alg_df$mae))]
#alg_df$c2rcc_cut[which(alg_df$mae == sort(alg_df$mae))]
alg_df$c2rcc_cut[which(alg_df$bias == min(alg_df$bias))]

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
head(csv)
csv$c50m15 <- assign_alg(data = csv, c2rcc_max = 50, mph_min = 15)
csv$c50m10 <- assign_alg(data = csv, c2rcc_max = 50, mph_min = 10)
csv$c15m10 <- assign_alg(data = csv, c2rcc_max = 15, mph_min = 10)

## SHIT!!!
# 15 10
sum(csv$c15m10 == csv$chl_merged_pitarch10_15, na.rm = TRUE)
c(sum(is.na(csv$c15m10)), sum(is.na(csv$chl_merged_pitarch10_15)))
sum(is.na(csv$c15m10) & is.na(csv$chl_merged_pitarch10_15))
sum(!is.na(csv$c15m10) & !is.na(csv$chl_merged_pitarch10_15))
sum(is.na(csv$c15m10) & !is.na(csv$chl_merged_pitarch10_15))
sum(!is.na(csv$c15m10) & is.na(csv$chl_merged_pitarch10_15))


csv$diff_c50m15 <-  csv$c15m10 - csv$chl_merged_pitarch10_15
summary(csv$diff_c15m10)

# 50 15
sum(csv$c50m15 == csv$chl_merged_pitarch15_50, na.rm = TRUE)
c(sum(is.na(csv$c50m15)), sum(is.na(csv$chl_merged_pitarch15_50)))

csv$diff_c50m15 <-  csv$c50m15 - csv$chl_merged_pitarch15_50
summary(csv$diff_c50m15)


## try to replicate what they did
assign_alg_brock <- function(data = NULL, c2rcc_max = 50, mph_min = 10) {
  merged_vals <- rep(NA, nrow(data))
  for (r in 1:nrow(data)) {
    if (is.na(data$mph[r])) {
      merged_vals[r] <- NA
    } else if (data$mph[r] > mph_min) {
      merged_vals[r] <- data$mph[r]
    } else if (is.na(data$c2rcc[r])) {
      merged_vals[r] <- NA
    } else if (data$c2rcc[r] <= c2rcc_max) {
      merged_vals[r] <- data$c2rcc[r]
    } else {
      merged_vals[r] <- NA
    }
  }
  return(merged_vals)
}

csv$c15m10_brock <- assign_alg_brock(data = csv, c2rcc_max = 15, mph_min = 10)

sum(csv$c15m10_brock == csv$chl_merged_pitarch10_15, na.rm = TRUE)
c(sum(is.na(csv$c15m10_brock)), sum(is.na(csv$chl_merged_pitarch10_15)))
sum(is.na(csv$c15m10_brock) & is.na(csv$chl_merged_pitarch10_15))
sum(!is.na(csv$c15m10_brock) & !is.na(csv$chl_merged_pitarch10_15))
sum(is.na(csv$c15m10_brock) & !is.na(csv$chl_merged_pitarch10_15))
sum(!is.na(csv$c15m10_brock) & is.na(csv$chl_merged_pitarch10_15))


csv$diff_c15m10 <-  csv_c15m10_brock$c15m10_brock - csv_c15m10_brock$chl_merged_pitarch10_15
  