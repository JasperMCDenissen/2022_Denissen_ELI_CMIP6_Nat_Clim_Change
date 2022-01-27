
# define functions
# Function to detrend the time series for 30 years of monthly data (30*12=360)
detrend_fun_10yr <- function(x){
  ifelse(sum(!is.na(x)) != 0, 
         detrend <- x - c(1:120) * lm(x ~ c(1:120))$coef[2],
         detrend <- rep(NA, 120))
  return(detrend)
}

detrend_fun_5yr <- function(x){
  ifelse(sum(!is.na(x)) != 0, 
         detrend <- x - c(1:60) * lm(x ~ c(1:60))$coef[2],
         detrend <- rep(NA, 60))
  return(detrend)
}
# detrend_fun_3yr <- function(x){
#   ifelse(sum(!is.na(x)) != 0, 
#          detrend <- x - c(1:36) * lm(x ~ c(1:36))$coef[2],
#          detrend <- rep(NA, 36))
#   return(detrend)
# }
detrend_fun_15yr <- function(x){
  ifelse(sum(!is.na(x)) != 0, 
         detrend <- x - c(1:180) * lm(x ~ c(1:180))$coef[2],
         detrend <- rep(NA, 180))
  return(detrend)
}
detrend_fun_33yr <- function(x){
  ifelse(sum(!is.na(x)) != 0, 
         detrend <- x - c(1:396) * lm(x ~ c(1:396))$coef[2],
         detrend <- rep(NA, 396))
  return(detrend)
}

detrend_fun <- function(x){
  ifelse(sum(!is.na(x)) != 0, 
         detrend <- x - c(1:360) * lm(x ~ c(1:360))$coef[2],
         detrend <- rep(NA, 360))
  return(detrend)
}
get_trend <- function(x){
  ifelse(sum(!is.na(x)) != 0, 
         detrend <- lm(x ~ c(1:120))$coef[2],
         detrend <- NA)
  return(detrend)
}

perc90_fun <- function(x){
  perc90 <- unname(quantile(x, prob = c(.9)))
  # perc90 <- mean(x, na.rm=T)
  return(perc90)
}

perc10_fun <- function(x){
  perc10 <- unname(quantile(x, prob = c(.1)))
  # perc10 <- mean(x, na.rm=T)
  return(perc10)
}

# exc_perc90_fun <- function(x){
#   sum_hot_months <- length(which(x > unname(quantile(x, prob = c(.9)))))
#   return(sum_hot_months)
# }

temp_K_mask_fun <- function(x){
  mask <- x
  mask[x >= 273.15 + 10] <- 1
  mask[x < 273.15 + 10] <- NA
  return(mask)
}

# This function requires to stack the two variables to be correlated
corr_fun_10yr <- function(a){
  ifelse(sum(!is.na(a[1:120])) >= 30 & sum(!is.na(a[121:240])) >= 30,
         corr_a_b <- cor(a[1:120], a[121:240], method = 'kendall', use = "pairwise.complete.obs"),
         corr_a_b <- NaN
  )
  return(corr_a_b)
}

corr_fun_h3m <- function(a){
  ifelse(sum(!is.na(a[1:120])) >= 10 & sum(!is.na(a[121:240])) >= 10,
         corr_a_b <- cor(a[1:120], a[121:240], method = 'kendall', use = "pairwise.complete.obs"),
         corr_a_b <- NaN
  )
  return(corr_a_b)
}

corr_fun_h3m_5yr <- function(a){
  ifelse(sum(!is.na(a[1:60])) >= 10 & sum(!is.na(a[61:120])) >= 10,
         corr_a_b <- cor(a[1:60], a[61:120], method = 'kendall', use = "pairwise.complete.obs"),
         corr_a_b <- NaN
  )
  return(corr_a_b)
}

pcorr_fun_10yr <- function(a){
  ifelse(sum(!is.na(a[1:120])) >= 30 & sum(!is.na(a[121:240])) >= 30 & sum(!is.na(a[241:360])) >= 30,
         pcorr_a_b_c <- pcor.test(a[1:120][which(!is.na(a[1:120]))], a[121:240][which(!is.na(a[1:120]))], a[241:360][which(!is.na(a[1:120]))], 
                                method = 'kendall')$estimate,
         pcorr_a_b_c <- NaN
  )
  return(pcorr_a_b_c)
}

corr_fun_3yr <- function(a){
  ifelse(sum(!is.na(a[1:36])) >= 12 & sum(!is.na(a[37:72])) >= 12,
         corr_a_b <- cor(a[1:36], a[37:72], method = 'kendall', use = "pairwise.complete.obs"),
         corr_a_b <- NaN
  )
  return(corr_a_b)
}
corr_fun_seascycle <- function(a){
  ifelse(sum(!is.na(a[1:10])) >= 10 & sum(!is.na(a[11:20])) >= 10,
         corr_a_b <- cor(a[1:10], a[11:20], method = 'kendall', use = "pairwise.complete.obs"),
         corr_a_b <- NaN
  )
  return(corr_a_b)
}
corr_fun <- function(a){
  ifelse(sum(!is.na(a[1:360])) >= 30 & sum(!is.na(a[361:720])) >= 30,
         corr_a_b <- cor(a[1:360], a[361:720], method = 'kendall', use = "pairwise.complete.obs"),
         corr_a_b <- NaN
  )
  return(corr_a_b)
}

isna_fun <- function(a){
  return(sum(!is.na(a)))
}

# mask function that masks for the same day
mask_fun_1d <- function(x){
  mask <- x
  mask[!is.na(mask)] <- 1
  return(mask)
}

# mask function that masks for 2 days before and two days after
mask_fun_5d <- function(x){
  mask <- x
  mask[!is.na(mask)] <- 1
  return(mask)
}

# If I want the 5 most extreme, run this function 5 times 
max_na_fun1 <- function(x) {
  # max_anom <- max(x, na.rm = T)
  max_anom <- sort(x, decreasing = T)[1]
  x[x != max_anom] <- NA
  return(x)
}

