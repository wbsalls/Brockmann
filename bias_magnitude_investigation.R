
# get bias in each direction for comparison
bx <- calc_error_metrics(x = brock[, insitu_col], y = brock[, c], log_space = TRUE)$bias
by <- calc_error_metrics(x = brock[, c], y = brock[, insitu_col], log_space = TRUE)$bias

# magnitude of bias_mult
bias_mag <- function(bias) {
  abs(1 - bias)
}

bias_mag(bx)
bias_mag(by)

alg_df$bias_mag <- bias_mag(alg_df$bias)

# magnitude of log bias_mult
lbias_mag <- function(bias) {
  abs(log10(bias))
}

log10(bx)
log10(by)

# yes!
lbias_mag(bx)
lbias_mag(by)

alg_df$lbias_mag <- lbias_mag(alg_df$bias)

# visualize
plot(alg_df$bias_mag)
plot(alg_df$lbias_mag)

