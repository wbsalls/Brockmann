library(openxlsx)

source("C:/Users/WSALLS/Git/Sent2/error_metrics_1800611.R")

setwd("O:/PRIV/NERL_ORD_CYAN/Brockmann_CRADA/AlgorithmAssessment")

brock_raw <- read.xlsx("Brockmann_chla_validation.xlsx")


# switch order of columns so hybrids are in order, so sensor is at end, 
# and so C2RCC comes before MPH (more intuitive given that we're using C2RCC for lower values)
colnames(brock_raw)
brock <- brock_raw[, c(2, 4, 3, 6, 5, 7, 1)]
colnames(brock)


# specify relevant columns
insitu_col <- 1
alg_cols <- 2:6


# make lookup table with plot titles and file names
name_lu <- data.frame(rbind(c(colnames(brock)[which(colnames(brock) == "conc_chl_valid_central_pixel")], "C2RCC"),
                            c(colnames(brock)[which(colnames(brock) == "chl_pitarch_valid_central_pixel")], "MPH"),
                            c(colnames(brock)[which(colnames(brock) == "chl_merged_pitarch10_15")], "C15_M10"),
                            c(colnames(brock)[which(colnames(brock) == "chl_merged_pitarch10_50")], "C50_M10"),
                            c(colnames(brock)[which(colnames(brock) == "chl_merged_pitarch15_50")], "C50_M15")))
colnames(name_lu) <- c("raw", "formatted")


# filter to a single sensor, if desired
#brock <- brock[which(brock$Sensor == "MERIS"), ] # MERIS, OLCI




# initiate metrics data frame; initiate plotting matrix
alg_df <- data.frame()
par(mfrow = c("???")) #****************


# loop through algorithms to populate data frame and make plots
for (c in alg_cols) {
  
  # remove NAs and duplicates
  brock_c <- brock[!is.na(brock[, c]), ]
  brock_c <- brock_c[!duplicated(data.frame(brock_c[, insitu_col], brock_c[, c])), ]
  
  # set formatted name for plots and output file name 
  formatted_name <- name_lu$formatted[which(name_lu$raw == colnames(brock_c)[c])] #*********
  file_name <- name_lu$formatted[which(name_lu$raw == colnames(brock_c)[c])]
  
  # make plot
  #jpeg(sprintf("val_%s.jpg", file_name), width = 800*4.5, height = 800*4.5, res = 550)
  par(mar = c(5, 5, 2, 2))
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
                                    xlim = c(min(brock_c[, insitu_col], brock_c[, c], na.rm = T), max(brock_c[, insitu_col], brock_c[, c], na.rm = T)),
                                    ylim = c(min(brock_c[, insitu_col], brock_c[, c], na.rm = T), max(brock_c[, insitu_col], brock_c[, c], na.rm = T)),
                                    show_metrics = TRUE, 
                                    #xaxt="n",
                                    #yaxt="n",
                                    col = alpha("black", 0.4), 
                                    #col = mu_mci$sedimentf,
                                    #col = mu_mci$state_col,
                                    pch = 20)
  plot_min <- min(brock_c[, insitu_col], brock_c[, c], na.rm = T)
  plot_max <- max(brock_c[, insitu_col], brock_c[, c], na.rm = T)
  text(plot_min, plot_max,
       adj = c(0, 1),
       bquote(MAE[mult] * " = " * .(signif(val_metrics$MAE[2], digits = 3))))
  text(plot_min, plot_max / 2, # 500,
       adj = c(0, 1),
       bquote(bias[mult] * " = " * .(signif(val_metrics$bias[2], digits = 3))))
  text(plot_min, plot_max / 4, # 275,
       adj = c(0, 1),
       paste0("n = ", val_metrics$n[2]))
  
  #dev.off()
  
  alg_df <- rbind(alg_df, cbind(formatted_name, val_metrics[2, ]))
  
}

# bias magnitude
# magnitude of bias_mult
bias_mag <- function(bias) {
  abs(1 - bias)
}
#alg_df$bias_abs <- bias_mag(alg_df$bias) # not including since not directionally symmetrical

# magnitude of log bias_mult
lbias_mag <- function(bias) {
  abs(log10(bias))
}
alg_df$logbias_abs <- lbias_mag(alg_df$bias) # directionally symmetrical


# format
colnames(alg_df)[1] <- "algorithm"
rownames(alg_df) <- NULL
alg_df <- alg_df[, c(1:3, 10, 4:9)]

# write
write.csv(alg_df, "algorithm_metrics.csv")



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
