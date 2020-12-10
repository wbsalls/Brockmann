## assign algorithm function
# takes a data frame with columns "c2rcc" and "mph"
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
