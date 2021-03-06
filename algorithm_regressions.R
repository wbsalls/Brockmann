### makes validation plots for each of five algorithms tested

#library(openxlsx)

source("C:/Users/WSALLS/Git/Sent2/error_metrics_1800611.R")
source("C:/Users/WSALLS/Git/Brockmann/assign_algorithm.R")

#setwd("O:/PRIV/NERL_ORD_CYAN/Brockmann_CRADA/AlgorithmAssessment")
setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/brockmann")

brock_raw <- read.csv("data_current/Brockmann_validation_ready_2020-11-25.csv")

brock <- brock_raw

colnames(brock)[which(colnames(brock) == "c2rcc")] <- "C2RCC"
colnames(brock)[which(colnames(brock) == "mph")] <- "MPH"
colnames(brock)[which(colnames(brock) == "chl_merged_pitarch15_50")] <- "C50_M15"
colnames(brock)[which(colnames(brock) == "chl_merged_pitarch10_50")] <- "C50_M10"
colnames(brock)[which(colnames(brock) == "chl_merged_pitarch10_15")] <- "C15_M10"

# specify relevant columns
insitu_col <- which(colnames(brock) == "insitu")
alg_cols <- c(which(colnames(brock) == "C2RCC"), which(colnames(brock) == "MPH"), 
              which(colnames(brock) == "C50_M15"), which(colnames(brock) == "C50_M10"), 
              which(colnames(brock) == "C15_M10"))


##### plot #####

# find data range so that all plots have the same extent
chl_min <- min(cbind(brock[, insitu_col], brock[, alg_cols]), na.rm = T)
chl_max <- max(cbind(brock[, insitu_col], brock[, alg_cols]), na.rm = T)

# make plot layout matrix
layout_matrix <- matrix(c(1, 1, 2, 2,
                          3, 3, 4, 4,
                          0, 5, 5, 0), 
                        nrow = 3, byrow = TRUE)

# initiate metrics data frame; initiate plotting layout
alg_df <- data.frame()

scalef <- 3 # play with scaling factor and resolution
jpeg(sprintf("val_%s.jpg", Sys.Date()), width = 575 * scalef, height = 900 * scalef, res = 300)
layout(layout_matrix)
par(mar = c(5, 4.5, 1.5, 1))

# loop through algorithms to populate data frame and make plots
for (c in alg_cols) {
  
  print(colnames(brock)[c])
  
  # remove NAs and duplicates
  brock_c <- brock[!is.na(brock[, c]), ]
  brock_c <- brock_c[!duplicated(data.frame(brock_c[, insitu_col], brock_c[, c])), ]
  print(sprintf("%s duplicates removed", sum(duplicated(data.frame(brock_c[, insitu_col], brock_c[, c])))))
  
  # set formatted name for plots and output file name
  rough_name <- colnames(brock_c)[c]
  
  if (rough_name == "C2RCC") {
    formatted_name <- bquote(bold("C2RCC"))
  } else if (rough_name == "MPH") {
    formatted_name <- bquote(bold("MPH"[(P)]))
  } else {
    c_thresh <- substr(rough_name, 2, 3)
    m_thresh <- substr(rough_name, 6, 7)
    formatted_name <- bquote(bold("C"[.(c_thresh)] * "-M"[.(m_thresh)]))
  }
  
  # make plot
  val_metrics <- plot_error_metrics(x = brock_c[, insitu_col], y = brock_c[, c], # export 800 x 860; 600 x 645 for paper
                                    xname = expression(italic("in situ") * " chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                    yname = expression("satellite-derived chl " * italic(a) * " (" * mu * "g " * L^-1 * ")"), 
                                    title = formatted_name, 
                                    equal_axes = TRUE, 
                                    log_axes = "xy", # xy, x, y, ""
                                    log_space = TRUE, # T, F
                                    plot_abline = FALSE,
                                    #text_x = min(brock_c[, insitu_col], brock_c[, c]),
                                    #text_y = ,
                                    mape = FALSE,
                                    rand_error = FALSE,
                                    regr_stats = FALSE,
                                    xlim = c(chl_min, chl_max),
                                    ylim = c(chl_min, chl_max),
                                    show_metrics = TRUE, 
                                    #xaxt="n",
                                    #yaxt="n",
                                    col = alpha("black", 0.4), 
                                    #col = mu_mci$sedimentf,
                                    #col = mu_mci$state_col,
                                    pch = 20)
  plot_min <- chl_min
  plot_max <- chl_max
  text(plot_min, plot_max,
       adj = c(0, 1),
       bquote(MAD[mult] * " = " * .(signif(val_metrics$MAE[2], digits = 3))))
  text(plot_min, plot_max / 2, # 500,
       adj = c(0, 1),
       bquote(bias[mult] * " = " * .(signif(val_metrics$bias[2], digits = 3))))
  text(plot_min, plot_max / 4, # 275,
       adj = c(0, 1),
       paste0("n = ", val_metrics$n[2]))
  text(plot_max, plot_min, 
       adj = c(1, 0),
       bquote(bold((.(letters[c - 2])))))
  
  #dev.off()
  
  alg_df <- rbind(alg_df, cbind(rough_name, val_metrics[2, ], 
                                calc_mae_mult(observed = brock_c[, insitu_col], modeled = brock_c[, c]),
                                calc_bias_mult(observed = brock_c[, insitu_col], modeled = brock_c[, c])))
  
}

layout(1)
dev.off()

###


# add magnitude of bias_mult to data frame
lbias_mag <- function(bias) {
  abs(log10(bias))
}
alg_df$abs_log10_bias <- lbias_mag(alg_df$bias) # directionally symmetrical


# format
rownames(alg_df) <- NULL
alg_df <- alg_df[, c(1, 10, 5, 7, 8, 11, 2:4, 6, 9)]
colnames(alg_df)[c(1, 3, 4)] <- c("algorithm", "MAD", "MAPD")

# write
write.csv(alg_df, sprintf("algorithm_metrics_%s.csv", Sys.Date()))



## ---------------------------------

# star plot
library(graphics)
stars()
symbols(x = 1, y = 1, stars = alg_df[, 2:4])

#
alg_df$colors <- c("red", "yellow", "green", "blue", "purple")
#alg_df_saved <- alg_df
alg_df <- alg_df_saved
alg_df <- alg_df_saved[c(1, 3:5), ]

stars(alg_df[, 2:4], locations = c(0, 0),
      key.loc = c(0, 0), main = "algorithms", 
      col.lines = alg_df$colors,
      labels = alg_df$algorithm,
      lwd = 1)


#


# dup check
for (c in alg_cols) {
  obs <- paste0(brock[, insitu_col], "; ", brock[, c])
  obs_df <- data.frame(table(obs))
  obs_df <- obs_df[order(obs_df$Freq, decreasing = TRUE), ]
  obs_df <- obs_df[1:sum(obs_df$Freq > 1), ]
  obs_df <- obs_df[-which(grepl("NA", obs_df[, 1])), ]
  rownames(obs_df) <- NULL
  print(obs_df)
  print(sum(obs_df$Freq))
}

table(brock$Sensor)
