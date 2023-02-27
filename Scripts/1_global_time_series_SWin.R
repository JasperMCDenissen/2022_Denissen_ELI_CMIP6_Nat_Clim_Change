# Script to make Figure 1 and Supplementary Figures 1-6
# 14-12-2020
# Jasper Denissen
pdf(NULL)

##############################################################################################################################
##############################################################################################################################
######################## !!! Don't forget to reset the working directory to a directory of your choosing!!! ##################
##############################################################################################################################
##############################################################################################################################
setwd('/Net/Groups/BGI/work_3/HydroBioClim/archive/Denissen_etal_2022_Nat_Clim_Change/')
# Get the proper packages
source('Scripts/to_be_loaded_packages.R')
# source functions
source('Scripts/functions/calc_boxes.R')
source('Scripts/functions/plot_discrete_cbar.R')
source('Scripts/functions/align_nine_plots.R')

# from /RData
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
load('RData/202112_dcorr_cmip6_10yr_rsds_evspsblveg.RData')
load("RData/202208_dcorr_cmip6_10yr_SWin.RData")
# # from /testdir
# load('testdir/202106_cmip6_no_evspsblveg_hurs.RData')
# load('testdir/202112_dcorr_cmip6_10yr_rsds_evspsblveg.RData')
# load("testdir/202208_dcorr_cmip6_10yr_SWin.RData")

lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# select the right models and put in array
dcorr_all.array <- 
  corr_wtr_veg_all.array <- 
  corr_rgy_veg_all.array <- 
  dcorr_rsds.array <- 
  corr_wtr_veg_rsds.array <- 
  corr_rgy_veg_rsds.array <- 
  pcorr_rgy_veg_all.array <- 
  dpcorr_all.array <- 
  pcorr_wtr_veg_all.array <- 
  dcorr_t_all.array <- 
  corr_rgy_veg_t_all.array <- 
  corr_wtr_veg_t_all.array <- 
  av_tas_all.array <- 
  av_mrso_all.array <- 
  av_hfls_all.array <- 
  av_lai.array <- 
  av_rlds.array <- 
  av_rlus.array <- 
  av_rsds.array <- 
  av_rsus.array <- 
  av_pr.array <- array(NaN,c(180,90,11*12)) # 11 different source_id and 12*10 years (total: 132)
count_all <- 1
for(i in 1:11){
  dcorr_all.array[,,count_all:(count_all+11)] <- dcorr.list[[i]]
  corr_rgy_veg_all.array[,,count_all:(count_all+11)] <- corr_rgy_veg.list[[i]]
  corr_wtr_veg_all.array[,,count_all:(count_all+11)] <- corr_wtr_veg.list[[i]]
  dcorr_rsds.array[,,count_all:(count_all+11)] <- dcorr_rsds.list[[i]]
  corr_rgy_veg_rsds.array[,,count_all:(count_all+11)] <- corr_rgy_veg_rsds.list[[i]]
  corr_wtr_veg_rsds.array[,,count_all:(count_all+11)] <- corr_wtr_veg_rsds.list[[i]]
  dpcorr_all.array[,,count_all:(count_all+11)] <- dpcorr.list[[i]]
  pcorr_rgy_veg_all.array[,,count_all:(count_all+11)] <- pcorr_rgy_veg.list[[i]]
  pcorr_wtr_veg_all.array[,,count_all:(count_all+11)] <- pcorr_wtr_veg.list[[i]]
  av_tas_all.array[,,count_all:(count_all+11)] <- av_tas.list[[i]]
  av_mrso_all.array[,,count_all:(count_all+11)] <- av_mrso.list[[i]]
  av_hfls_all.array[,,count_all:(count_all+11)] <- av_hfls.list[[i]]
  av_lai.array[,,count_all:(count_all+11)] <- av_lai.list[[i]]
  av_rlds.array[,,count_all:(count_all+11)] <- av_rlds.list[[i]]
  av_rlus.array[,,count_all:(count_all+11)] <- av_rlus.list[[i]]
  av_rsds.array[,,count_all:(count_all+11)] <- av_rsds.list[[i]]
  av_rsus.array[,,count_all:(count_all+11)] <- av_rsus.list[[i]]
  av_pr.array[,,count_all:(count_all+11)] <- av_pr.list[[i]]
  count_all <- count_all + 12
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dcorr_all.array[91:180,,]; test[91:180,,] <- dcorr_all.array[1:90,,]; dcorr_all.array <- test; dcorr_all.array <- -1*dcorr_all.array
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- corr_rgy_veg_all.array[91:180,,]; test[91:180,,] <- corr_rgy_veg_all.array[1:90,,]; corr_rgy_veg_all.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- corr_wtr_veg_all.array[91:180,,]; test[91:180,,] <- corr_wtr_veg_all.array[1:90,,]; corr_wtr_veg_all.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dcorr_rsds.array[91:180,,]; test[91:180,,] <- dcorr_rsds.array[1:90,,]; dcorr_rsds.array <- test; dcorr_rsds.array <- -1*dcorr_rsds.array
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- corr_rgy_veg_rsds.array[91:180,,]; test[91:180,,] <- corr_rgy_veg_rsds.array[1:90,,]; corr_rgy_veg_rsds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- corr_wtr_veg_rsds.array[91:180,,]; test[91:180,,] <- corr_wtr_veg_rsds.array[1:90,,]; corr_wtr_veg_rsds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dpcorr_all.array[91:180,,]; test[91:180,,] <- dpcorr_all.array[1:90,,]; dpcorr_all.array <- test; dpcorr_all.array <- -1*dpcorr_all.array
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- pcorr_rgy_veg_all.array[91:180,,]; test[91:180,,] <- pcorr_rgy_veg_all.array[1:90,,]; pcorr_rgy_veg_all.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- pcorr_wtr_veg_all.array[91:180,,]; test[91:180,,] <- pcorr_wtr_veg_all.array[1:90,,]; pcorr_wtr_veg_all.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_tas_all.array[91:180,,]; test[91:180,,] <- av_tas_all.array[1:90,,]; av_tas_all.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_mrso_all.array[91:180,,]; test[91:180,,] <- av_mrso_all.array[1:90,,]; av_mrso_all.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_hfls_all.array[91:180,,]; test[91:180,,] <- av_hfls_all.array[1:90,,]; av_hfls_all.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rlds.array[91:180,,]; test[91:180,,] <- av_rlds.array[1:90,,]; av_rlds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rlus.array[91:180,,]; test[91:180,,] <- av_rlus.array[1:90,,]; av_rlus.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rsds.array[91:180,,]; test[91:180,,] <- av_rsds.array[1:90,,]; av_rsds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rsus.array[91:180,,]; test[91:180,,] <- av_rsus.array[1:90,,]; av_rsus.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_pr.array[91:180,,]; test[91:180,,] <- av_pr.array[1:90,,]; av_pr.array <- test
av_rnet.array <- ((av_rlds.array - av_rlus.array) + (av_rsds.array - av_rsus.array))/26.15741 # in mm/d
av_rsds.array <- av_rsds.array/26.15741

# rho <- 1000 #kg/m3
# l_v <- 2.26 #MJ/kg
# # W/m2 = 1000 (kg/m3) * 2.26*10^6 (J/kg) * 1 mm/day*(1/(24*60^2))(day/s) * (1/1000)(mm/m)

# conversion factor 1mm = 28.35648 Wm-2
av_hfls_all.array <- av_hfls_all.array/26.15741 # (mm/d)

av_pr.array <- av_pr.array*24*60*60
av_ar.array <- av_rnet.array/(av_pr.array)

# rho <- 1000 #kg/m3
# l_v <- 2.26 #MJ/kg
# conv <- rho*l_v/1000
# av_ar_ERA5.array <- av_snr.array/(av_tp.array*conv)

# make a mask since the observations
mask_obs <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(dcorr_all.array[x,y,])) == 11*12){ # check if all the models have a value there
      mask_obs[x,y] <- 1
    }
  }
}


# maybe now per surface area instead of % of grid cells?
r <- raster()  # by default 1 by 1 degree
res(r) <- 2 # so change the resolution
a <- raster::area(r) # calculate the area of a 2x2 degree grid since N - S, as area varies only by latitude, not longitude
area <- a[,1]
area.array <- array(NaN,c(180,90))
for(x in 1:180){
  area.array[x,] <- area
}

# Calculate area-weighted average dcorr,tas,mrso,hfls
tseries.df <- setNames(data.frame(matrix(ncol = 16, nrow = 0)),
                       c("dcorr", "corr_rgy_veg", "corr_wtr_veg",
                         "dcorr_rsds", "corr_rgy_veg_rsds", "corr_wtr_veg_rsds",
                         "dpcorr", "pcorr_rgy_veg", "pcorr_wtr_veg",
                         "tas","mrso","hfls","lai","ar","pr","rsds"))
for(i in 1:132){
  tseries.df <- rbind(tseries.df, 
                      data.frame("dcorr" = weighted.mean(x = mask_obs*dcorr_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_rgy_veg" = weighted.mean(x = mask_obs*corr_rgy_veg_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_wtr_veg" = weighted.mean(x = mask_obs*corr_wtr_veg_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "dcorr_rsds" = weighted.mean(x = mask_obs*dcorr_rsds.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_rgy_veg_rsds" = weighted.mean(x = mask_obs*corr_rgy_veg_rsds.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_wtr_veg_rsds" = weighted.mean(x = mask_obs*corr_wtr_veg_rsds.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "dpcorr" = weighted.mean(x = mask_obs*dpcorr_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "pcorr_rgy_veg" = weighted.mean(x = mask_obs*pcorr_rgy_veg_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "pcorr_wtr_veg" = weighted.mean(x = mask_obs*pcorr_wtr_veg_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "tas" = weighted.mean(x = mask_obs*av_tas_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "mrso" = weighted.mean(x = mask_obs*av_mrso_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "hfls" = weighted.mean(x = mask_obs*av_hfls_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "lai" = weighted.mean(x = mask_obs*av_lai.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "ar" = weighted.median(x = mask_obs*av_ar.array[,,i], w = area.array*mask_obs, na.rm = T), # only in case of aridity I take the mean!
                                 "pr" = weighted.mean(x = mask_obs*av_pr.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "rsds" = weighted.mean(x = mask_obs*av_rsds.array[,,i], w = area.array*mask_obs, na.rm = T)
                      ))
}
source_id <- setNames(data.frame(matrix(ncol = 1, nrow = 0)),
                      c("source_id"))
for(i in 1:11){
  source_id <- rbind(source_id,
                     data.frame("source_id" = rep(cmip6_data.df$source_id[i],12)))
}
tseries.df$source_id <- source_id$source_id
tseries.df$year <- rep(c(seq(1980,2090,10)),11)

tseries.df$dcorr_abs_1980 <-
  tseries.df$corr_rgy_veg_abs_1980 <-
  tseries.df$corr_wtr_veg_abs_1980 <-
  tseries.df$dcorr_rsds_abs_1980 <-
  tseries.df$corr_rgy_veg_rsds_abs_1980 <-
  tseries.df$corr_wtr_veg_rsds_abs_1980 <-
  tseries.df$dpcorr_abs_1980 <-
  tseries.df$pcorr_rgy_veg_abs_1980 <-
  tseries.df$pcorr_wtr_veg_abs_1980 <-
  tseries.df$tas_abs_1980 <-
  tseries.df$mrso_abs_1980 <-
  tseries.df$hfls_abs_1980 <-
  tseries.df$lai_abs_1980 <-
  tseries.df$ar_abs_1980 <- 
  tseries.df$pr_abs_1980 <- 
  tseries.df$rsds_abs_1980 <-
  NaN

for(i in 1:132){
  tseries.df$dcorr_abs_1980[i] <- (tseries.df$dcorr[i] - tseries.df$dcorr[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_rgy_veg_abs_1980[i] <- (tseries.df$corr_rgy_veg[i] - tseries.df$corr_rgy_veg[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_wtr_veg_abs_1980[i] <- (tseries.df$corr_wtr_veg[i] - tseries.df$corr_wtr_veg[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$dcorr_rsds_abs_1980[i] <- (tseries.df$dcorr_rsds[i] - tseries.df$dcorr_rsds[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_rgy_veg_rsds_abs_1980[i] <- (tseries.df$corr_rgy_veg_rsds[i] - tseries.df$corr_rgy_veg_rsds[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_wtr_veg_rsds_abs_1980[i] <- (tseries.df$corr_wtr_veg_rsds[i] - tseries.df$corr_wtr_veg_rsds[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$dpcorr_abs_1980[i] <- (tseries.df$dpcorr[i] - tseries.df$dpcorr[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$pcorr_rgy_veg_abs_1980[i] <- (tseries.df$pcorr_rgy_veg[i] - tseries.df$pcorr_rgy_veg[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$pcorr_wtr_veg_abs_1980[i] <- (tseries.df$pcorr_wtr_veg[i] - tseries.df$pcorr_wtr_veg[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$tas_abs_1980[i] <- (tseries.df$tas[i] - tseries.df$tas[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$mrso_abs_1980[i] <- (tseries.df$mrso[i] - tseries.df$mrso[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$hfls_abs_1980[i] <- (tseries.df$hfls[i] - tseries.df$hfls[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$lai_abs_1980[i] <- (tseries.df$lai[i] - tseries.df$lai[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$ar_abs_1980[i] <- (tseries.df$ar[i] - tseries.df$ar[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$pr_abs_1980[i] <- (tseries.df$pr[i] - tseries.df$pr[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$rsds_abs_1980[i] <- (tseries.df$rsds[i] - tseries.df$rsds[which(tseries.df$source_id == tseries.df$source_id[i])][1])
}

mmmtseries.df <- data.frame("year" = seq(1980,2090,10))
mmindex <- rep(seq(1,12),11)
for(i in 1:12){
  mmmtseries.df$mmmedian_dcorr[i] <- median(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_dcorr[i] <- quantile(tseries.df$dcorr_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_dcorr[i] <- quantile(tseries.df$dcorr_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_dcorr[i] <- mean(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_dcorr[i] <- mmmtseries.df$mmmean_dcorr[i] - sd(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_dcorr[i] <- mmmtseries.df$mmmean_dcorr[i] + sd(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_dcorr_rsds[i] <- median(tseries.df$dcorr_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_dcorr_rsds[i] <- quantile(tseries.df$dcorr_rsds_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_dcorr_rsds[i] <- quantile(tseries.df$dcorr_rsds_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_dcorr_rsds[i] <- mean(tseries.df$dcorr_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_dcorr_rsds[i] <- mmmtseries.df$mmmean_dcorr_rsds[i] - sd(tseries.df$dcorr_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_dcorr_rsds[i] <- mmmtseries.df$mmmean_dcorr_rsds[i] + sd(tseries.df$dcorr_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_dpcorr[i] <- median(tseries.df$dpcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_dpcorr[i] <- quantile(tseries.df$dpcorr_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_dpcorr[i] <- quantile(tseries.df$dpcorr_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_dpcorr[i] <- mean(tseries.df$dpcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_dpcorr[i] <- mmmtseries.df$mmmean_dpcorr[i] - sd(tseries.df$dpcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_dpcorr[i] <- mmmtseries.df$mmmean_dpcorr[i] + sd(tseries.df$dpcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_rgy_veg[i] <- median(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_rgy_veg[i] <- quantile(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_rgy_veg[i] <- quantile(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_rgy_veg[i] <- mean(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_rgy_veg[i] <- mmmtseries.df$mmmean_corr_rgy_veg[i] - sd(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_rgy_veg[i] <- mmmtseries.df$mmmean_corr_rgy_veg[i] + sd(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_rgy_veg_rsds[i] <- median(tseries.df$corr_rgy_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_rgy_veg_rsds[i] <- quantile(tseries.df$corr_rgy_veg_rsds_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_rgy_veg_rsds[i] <- quantile(tseries.df$corr_rgy_veg_rsds_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_rgy_veg_rsds[i] <- mean(tseries.df$corr_rgy_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_rgy_veg_rsds[i] <- mmmtseries.df$mmmean_corr_rgy_veg_rsds[i] - sd(tseries.df$corr_rgy_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_rgy_veg_rsds[i] <- mmmtseries.df$mmmean_corr_rgy_veg_rsds[i] + sd(tseries.df$corr_rgy_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_pcorr_rgy_veg[i] <- median(tseries.df$pcorr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_pcorr_rgy_veg[i] <- quantile(tseries.df$pcorr_rgy_veg_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_pcorr_rgy_veg[i] <- quantile(tseries.df$pcorr_rgy_veg_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_pcorr_rgy_veg[i] <- mean(tseries.df$pcorr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_pcorr_rgy_veg[i] <- mmmtseries.df$mmmean_pcorr_rgy_veg[i] - sd(tseries.df$pcorr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_pcorr_rgy_veg[i] <- mmmtseries.df$mmmean_pcorr_rgy_veg[i] + sd(tseries.df$pcorr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_wtr_veg[i] <- median(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_wtr_veg[i] <- quantile(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_wtr_veg[i] <- quantile(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_wtr_veg[i] <- mean(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_wtr_veg[i] <- mmmtseries.df$mmmean_corr_wtr_veg[i] - sd(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_wtr_veg[i] <- mmmtseries.df$mmmean_corr_wtr_veg[i] + sd(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_wtr_veg_rsds[i] <- median(tseries.df$corr_wtr_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_wtr_veg_rsds[i] <- quantile(tseries.df$corr_wtr_veg_rsds_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_wtr_veg_rsds[i] <- quantile(tseries.df$corr_wtr_veg_rsds_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_wtr_veg_rsds[i] <- mean(tseries.df$corr_wtr_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_wtr_veg_rsds[i] <- mmmtseries.df$mmmean_corr_wtr_veg_rsds[i] - sd(tseries.df$corr_wtr_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_wtr_veg_rsds[i] <- mmmtseries.df$mmmean_corr_wtr_veg_rsds[i] + sd(tseries.df$corr_wtr_veg_rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_pcorr_wtr_veg[i] <- median(tseries.df$pcorr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_pcorr_wtr_veg[i] <- quantile(tseries.df$pcorr_wtr_veg_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_pcorr_wtr_veg[i] <- quantile(tseries.df$pcorr_wtr_veg_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_pcorr_wtr_veg[i] <- mean(tseries.df$pcorr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_pcorr_wtr_veg[i] <- mmmtseries.df$mmmean_pcorr_wtr_veg[i] - sd(tseries.df$pcorr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_pcorr_wtr_veg[i] <- mmmtseries.df$mmmean_pcorr_wtr_veg[i] + sd(tseries.df$pcorr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_tas[i] <- median(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_tas[i] <- quantile(tseries.df$tas_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_tas[i] <- quantile(tseries.df$tas_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_tas[i] <- mean(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_tas[i] <- mmmtseries.df$mmmean_tas[i] - sd(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_tas[i] <- mmmtseries.df$mmmean_tas[i] + sd(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_mrso[i] <- median(tseries.df$mrso_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_mrso[i] <- quantile(tseries.df$mrso_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_mrso[i] <- quantile(tseries.df$mrso_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_mrso[i] <- mean(tseries.df$mrso_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_mrso[i] <- mmmtseries.df$mmmean_mrso[i] - sd(tseries.df$mrso_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_mrso[i] <- mmmtseries.df$mmmean_mrso[i] + sd(tseries.df$mrso_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_hfls[i] <- median(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_hfls[i] <- quantile(tseries.df$hfls_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_hfls[i] <- quantile(tseries.df$hfls_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_hfls[i] <- mean(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_hfls[i] <- mmmtseries.df$mmmean_hfls[i] - sd(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_hfls[i] <- mmmtseries.df$mmmean_hfls[i] + sd(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_lai[i] <- median(tseries.df$lai_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_lai[i] <- quantile(tseries.df$lai_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_lai[i] <- quantile(tseries.df$lai_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_lai[i] <- mean(tseries.df$lai_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_lai[i] <- mmmtseries.df$mmmean_lai[i] - sd(tseries.df$lai_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_lai[i] <- mmmtseries.df$mmmean_lai[i] + sd(tseries.df$lai_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_ar[i] <- median(tseries.df$ar_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_ar[i] <- quantile(tseries.df$ar_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_ar[i] <- quantile(tseries.df$ar_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_ar[i] <- mean(tseries.df$ar_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_ar[i] <- mmmtseries.df$mmmean_ar[i] - sd(tseries.df$ar_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_ar[i] <- mmmtseries.df$mmmean_ar[i] + sd(tseries.df$ar_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_pr[i] <- median(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_pr[i] <- quantile(tseries.df$pr_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_pr[i] <- quantile(tseries.df$pr_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_pr[i] <- mean(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_pr[i] <- mmmtseries.df$mmmean_pr[i] - sd(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_pr[i] <- mmmtseries.df$mmmean_pr[i] + sd(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_rsds[i] <- median(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_rsds[i] <- quantile(tseries.df$rsds_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_rsds[i] <- quantile(tseries.df$rsds_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_rsds[i] <- mean(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_rsds[i] <- mmmtseries.df$mmmean_rsds[i] - sd(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_rsds[i] <- mmmtseries.df$mmmean_rsds[i] + sd(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
}

# 1) detrend mmmtseries.df, 2) compute standard deviation and 3) normalize mmmedian timeseries with that
mmmtseries.df_norm <- data.frame("median_norm" = c(mmmtseries.df$mmmedian_dcorr / sd(mmmtseries.df$mmmedian_dcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_dcorr)$estimate[2]),
                                                   mmmtseries.df$mmmedian_corr_rgy_veg / sd(mmmtseries.df$mmmedian_corr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_corr_rgy_veg)$estimate[2]),
                                                   mmmtseries.df$mmmedian_corr_wtr_veg / sd(mmmtseries.df$mmmedian_corr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_corr_wtr_veg)$estimate[2]),
                                                   mmmtseries.df$mmmedian_dcorr_rsds / sd(mmmtseries.df$mmmedian_dcorr_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_dcorr_rsds)$estimate[2]),
                                                   mmmtseries.df$mmmedian_corr_rgy_veg_rsds / sd(mmmtseries.df$mmmedian_corr_rgy_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_corr_rgy_veg_rsds)$estimate[2]),
                                                   mmmtseries.df$mmmedian_corr_wtr_veg_rsds / sd(mmmtseries.df$mmmedian_corr_wtr_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_corr_wtr_veg_rsds)$estimate[2]),
                                                   mmmtseries.df$mmmedian_dpcorr / sd(mmmtseries.df$mmmedian_dpcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_dpcorr)$estimate[2]),
                                                   mmmtseries.df$mmmedian_pcorr_rgy_veg / sd(mmmtseries.df$mmmedian_pcorr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_pcorr_rgy_veg)$estimate[2]),
                                                   mmmtseries.df$mmmedian_pcorr_wtr_veg / sd(mmmtseries.df$mmmedian_pcorr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_pcorr_wtr_veg)$estimate[2]),
                                                   mmmtseries.df$mmmedian_tas / sd(mmmtseries.df$mmmedian_tas - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_tas)$estimate[2]),
                                                   mmmtseries.df$mmmedian_mrso / sd(mmmtseries.df$mmmedian_mrso - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_mrso)$estimate[2]),
                                                   mmmtseries.df$mmmedian_hfls / sd(mmmtseries.df$mmmedian_hfls - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_hfls)$estimate[2]),
                                                   mmmtseries.df$mmmedian_lai / sd(mmmtseries.df$mmmedian_lai - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_lai)$estimate[2]),
                                                   mmmtseries.df$mmmedian_ar / sd(mmmtseries.df$mmmedian_ar - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_ar)$estimate[2]),
                                                   mmmtseries.df$mmmedian_pr / sd(mmmtseries.df$mmmedian_pr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_pr)$estimate[2]),
                                                   mmmtseries.df$mmmedian_rsds / sd(mmmtseries.df$mmmedian_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_rsds)$estimate[2])),
                                 "perc25_norm" = c(mmmtseries.df$perc25_dcorr / sd(mmmtseries.df$mmmedian_dcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_dcorr)$estimate[2]),
                                                   mmmtseries.df$perc25_corr_rgy_veg / sd(mmmtseries.df$perc25_corr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc25_corr_rgy_veg)$estimate[2]),
                                                   mmmtseries.df$perc25_corr_wtr_veg / sd(mmmtseries.df$perc25_corr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc25_corr_wtr_veg)$estimate[2]),
                                                   mmmtseries.df$perc25_dcorr_rsds / sd(mmmtseries.df$mmmedian_dcorr_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_dcorr_rsds)$estimate[2]),
                                                   mmmtseries.df$perc25_corr_rgy_veg_rsds / sd(mmmtseries.df$perc25_corr_rgy_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$perc25_corr_rgy_veg_rsds)$estimate[2]),
                                                   mmmtseries.df$perc25_corr_wtr_veg_rsds / sd(mmmtseries.df$perc25_corr_wtr_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$perc25_corr_wtr_veg_rsds)$estimate[2]),
                                                   mmmtseries.df$perc25_dpcorr / sd(mmmtseries.df$perc25_dpcorr - c(1:12)*kendallTrendTest(mmmtseries.df$perc25_dpcorr)$estimate[2]),
                                                   mmmtseries.df$perc25_pcorr_rgy_veg / sd(mmmtseries.df$perc25_pcorr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc25_pcorr_rgy_veg)$estimate[2]),
                                                   mmmtseries.df$perc25_pcorr_wtr_veg / sd(mmmtseries.df$perc25_pcorr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc25_pcorr_wtr_veg)$estimate[2]),
                                                   mmmtseries.df$perc25_tas / sd(mmmtseries.df$mmmedian_tas - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_tas)$estimate[2]),
                                                   mmmtseries.df$perc25_mrso / sd(mmmtseries.df$mmmedian_mrso - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_mrso)$estimate[2]),
                                                   mmmtseries.df$perc25_hfls / sd(mmmtseries.df$mmmedian_hfls - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_hfls)$estimate[2]),
                                                   mmmtseries.df$perc25_lai / sd(mmmtseries.df$mmmedian_lai - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_lai)$estimate[2]),
                                                   mmmtseries.df$perc25_ar / sd(mmmtseries.df$mmmedian_ar - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_ar)$estimate[2]),
                                                   mmmtseries.df$perc25_pr / sd(mmmtseries.df$mmmedian_pr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_pr)$estimate[2]),
                                                   mmmtseries.df$perc25_rsds / sd(mmmtseries.df$mmmedian_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_rsds)$estimate[2])),
                                 "perc75_norm" = c(mmmtseries.df$perc75_dcorr / sd(mmmtseries.df$mmmedian_dcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_dcorr)$estimate[2]),
                                                   mmmtseries.df$perc75_corr_rgy_veg / sd(mmmtseries.df$perc75_corr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc75_corr_rgy_veg)$estimate[2]),
                                                   mmmtseries.df$perc75_corr_wtr_veg / sd(mmmtseries.df$perc75_corr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc75_corr_wtr_veg)$estimate[2]),
                                                   mmmtseries.df$perc75_dcorr_rsds / sd(mmmtseries.df$mmmedian_dcorr_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_dcorr_rsds)$estimate[2]),
                                                   mmmtseries.df$perc75_corr_rgy_veg_rsds / sd(mmmtseries.df$perc75_corr_rgy_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$perc75_corr_rgy_veg_rsds)$estimate[2]),
                                                   mmmtseries.df$perc75_corr_wtr_veg_rsds / sd(mmmtseries.df$perc75_corr_wtr_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$perc75_corr_wtr_veg_rsds)$estimate[2]),
                                                   mmmtseries.df$perc75_dpcorr / sd(mmmtseries.df$perc75_dpcorr - c(1:12)*kendallTrendTest(mmmtseries.df$perc75_dpcorr)$estimate[2]),
                                                   mmmtseries.df$perc75_pcorr_rgy_veg / sd(mmmtseries.df$perc75_pcorr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc75_pcorr_rgy_veg)$estimate[2]),
                                                   mmmtseries.df$perc75_pcorr_wtr_veg / sd(mmmtseries.df$perc75_pcorr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$perc75_pcorr_wtr_veg)$estimate[2]),
                                                   mmmtseries.df$perc75_tas / sd(mmmtseries.df$mmmedian_tas - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_tas)$estimate[2]),
                                                   mmmtseries.df$perc75_mrso / sd(mmmtseries.df$mmmedian_mrso - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_mrso)$estimate[2]),
                                                   mmmtseries.df$perc75_hfls / sd(mmmtseries.df$mmmedian_hfls - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_hfls)$estimate[2]),
                                                   mmmtseries.df$perc75_lai / sd(mmmtseries.df$mmmedian_lai - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_lai)$estimate[2]),
                                                   mmmtseries.df$perc75_ar / sd(mmmtseries.df$mmmedian_ar - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_ar)$estimate[2]),
                                                   mmmtseries.df$perc75_pr / sd(mmmtseries.df$mmmedian_pr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_pr)$estimate[2]),
                                                   mmmtseries.df$perc75_rsds / sd(mmmtseries.df$mmmedian_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmedian_rsds)$estimate[2])),
                                 "mean_norm" = c(mmmtseries.df$mmmean_dcorr / sd(mmmtseries.df$mmmean_dcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_dcorr)$estimate[2]),
                                                 mmmtseries.df$mmmean_corr_rgy_veg / sd(mmmtseries.df$mmmean_corr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_corr_rgy_veg)$estimate[2]),
                                                 mmmtseries.df$mmmean_corr_wtr_veg / sd(mmmtseries.df$mmmean_corr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_corr_wtr_veg)$estimate[2]),
                                                 mmmtseries.df$mmmean_dcorr_rsds / sd(mmmtseries.df$mmmean_dcorr_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_dcorr_rsds)$estimate[2]),
                                                 mmmtseries.df$mmmean_corr_rgy_veg_rsds / sd(mmmtseries.df$mmmean_corr_rgy_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_corr_rgy_veg_rsds)$estimate[2]),
                                                 mmmtseries.df$mmmean_corr_wtr_veg_rsds / sd(mmmtseries.df$mmmean_corr_wtr_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_corr_wtr_veg_rsds)$estimate[2]),
                                                 mmmtseries.df$mmmean_dpcorr / sd(mmmtseries.df$mmmean_dpcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_dpcorr)$estimate[2]),
                                                 mmmtseries.df$mmmean_pcorr_rgy_veg / sd(mmmtseries.df$mmmean_pcorr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_pcorr_rgy_veg)$estimate[2]),
                                                 mmmtseries.df$mmmean_pcorr_wtr_veg / sd(mmmtseries.df$mmmean_pcorr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_pcorr_wtr_veg)$estimate[2]),
                                                 mmmtseries.df$mmmean_tas / sd(mmmtseries.df$mmmean_tas - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_tas)$estimate[2]),
                                                 mmmtseries.df$mmmean_mrso / sd(mmmtseries.df$mmmean_mrso - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_mrso)$estimate[2]),
                                                 mmmtseries.df$mmmean_hfls / sd(mmmtseries.df$mmmean_hfls - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_hfls)$estimate[2]),
                                                 mmmtseries.df$mmmean_lai / sd(mmmtseries.df$mmmean_lai - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_lai)$estimate[2]),
                                                 mmmtseries.df$mmmean_ar / sd(mmmtseries.df$mmmean_ar - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_ar)$estimate[2]),
                                                 mmmtseries.df$mmmean_pr / sd(mmmtseries.df$mmmean_pr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_pr)$estimate[2]),
                                                 mmmtseries.df$mmmean_rsds / sd(mmmtseries.df$mmmean_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_rsds)$estimate[2])),
                                 "mean_minsd_norm" = c(mmmtseries.df$mmmean_minsd_dcorr / sd(mmmtseries.df$mmmean_dcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_dcorr)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_corr_rgy_veg / sd(mmmtseries.df$mmmean_minsd_corr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_minsd_corr_rgy_veg)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_corr_wtr_veg / sd(mmmtseries.df$mmmean_minsd_corr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_minsd_corr_wtr_veg)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_dcorr_rsds / sd(mmmtseries.df$mmmean_dcorr_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_dcorr_rsds)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_corr_rgy_veg_rsds / sd(mmmtseries.df$mmmean_minsd_corr_rgy_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_minsd_corr_rgy_veg_rsds)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_corr_wtr_veg_rsds / sd(mmmtseries.df$mmmean_minsd_corr_wtr_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_minsd_corr_wtr_veg_rsds)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_dpcorr / sd(mmmtseries.df$mmmean_minsd_dpcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_minsd_dpcorr)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_pcorr_rgy_veg / sd(mmmtseries.df$mmmean_minsd_pcorr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_minsd_pcorr_rgy_veg)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_pcorr_wtr_veg / sd(mmmtseries.df$mmmean_minsd_pcorr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_minsd_pcorr_wtr_veg)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_tas / sd(mmmtseries.df$mmmean_tas - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_tas)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_mrso / sd(mmmtseries.df$mmmean_mrso - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_mrso)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_hfls / sd(mmmtseries.df$mmmean_hfls - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_hfls)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_lai / sd(mmmtseries.df$mmmean_lai - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_lai)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_ar / sd(mmmtseries.df$mmmean_ar - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_ar)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_pr / sd(mmmtseries.df$mmmean_pr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_pr)$estimate[2]),
                                                       mmmtseries.df$mmmean_minsd_rsds / sd(mmmtseries.df$mmmean_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_rsds)$estimate[2])),
                                 "mean_plussd_norm" = c(mmmtseries.df$mmmean_plussd_dcorr / sd(mmmtseries.df$mmmean_dcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_dcorr)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_corr_rgy_veg / sd(mmmtseries.df$mmmean_plussd_corr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_plussd_corr_rgy_veg)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_corr_wtr_veg / sd(mmmtseries.df$mmmean_plussd_corr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_plussd_corr_wtr_veg)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_dcorr_rsds / sd(mmmtseries.df$mmmean_dcorr_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_dcorr_rsds)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_corr_rgy_veg_rsds / sd(mmmtseries.df$mmmean_plussd_corr_rgy_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_plussd_corr_rgy_veg_rsds)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_corr_wtr_veg_rsds / sd(mmmtseries.df$mmmean_plussd_corr_wtr_veg_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_plussd_corr_wtr_veg_rsds)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_dpcorr / sd(mmmtseries.df$mmmean_plussd_dpcorr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_plussd_dpcorr)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_pcorr_rgy_veg / sd(mmmtseries.df$mmmean_plussd_pcorr_rgy_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_plussd_pcorr_rgy_veg)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_pcorr_wtr_veg / sd(mmmtseries.df$mmmean_plussd_pcorr_wtr_veg - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_plussd_pcorr_wtr_veg)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_tas / sd(mmmtseries.df$mmmean_tas - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_tas)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_mrso / sd(mmmtseries.df$mmmean_mrso - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_mrso)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_hfls / sd(mmmtseries.df$mmmean_hfls - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_hfls)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_lai / sd(mmmtseries.df$mmmean_lai - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_lai)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_ar / sd(mmmtseries.df$mmmean_ar - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_ar)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_pr / sd(mmmtseries.df$mmmean_pr - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_pr)$estimate[2]),
                                                        mmmtseries.df$mmmean_plussd_rsds / sd(mmmtseries.df$mmmean_rsds - c(1:12)*kendallTrendTest(mmmtseries.df$mmmean_rsds)$estimate[2])),
                                 "var" = c(rep("dcorr",12),rep("corr_rgy_veg",12),rep("corr_wtr_veg",12),rep("dcorr_rsds",12),rep("corr_rgy_veg_rsds",12),rep("corr_wtr_veg_rsds",12),rep("dpcorr",12),rep("pcorr_rgy_veg",12),rep("pcorr_wtr_veg",12),rep("tas",12),rep("mrso",12),rep("hfls",12),rep("lai",12),rep("ar",12),rep("pr",12),rep("rsds",12)),
                                 "year" = rep(c(seq(1980,2090,10)),16))


cols_var <- c(brewer.pal(n=11,'PiYG')[2],brewer.pal(11,"BrBG")[2], brewer.pal(11,"BrBG")[10], brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(8, "Set2")[6])

mmmtseries.df_norm$var_factor <- factor(mmmtseries.df_norm$var, levels = c("dcorr_rsds","corr_wtr_veg","corr_rgy_veg_rsds","rsds","mrso","hfls","lai","ar","pr"))
for_the_plot.df <- mmmtseries.df_norm[which(mmmtseries.df_norm$var != "pr" & mmmtseries.df_norm$var != "dpcorr" &
                                              mmmtseries.df_norm$var != "pcorr_rgy_veg" & mmmtseries.df_norm$var != "pcorr_wtr_veg" &
                                              mmmtseries.df_norm$var != "corr_rgy_veg" & mmmtseries.df_norm$var != "corr_wtr_veg_rsds" &
                                              mmmtseries.df_norm$var != "dcorr" & mmmtseries.df_norm$var != "tas"),]
# abs 1980
m4 <- ggplot(for_the_plot.df, aes(x=year,y=mean_norm,col=var_factor,fill=var_factor,linetype=var_factor,shape=var_factor)) +
  geom_point(size = 1.5) +
  geom_line() +
  geom_hline(yintercept=0) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,20),
                     labels = seq(1980,2090,20), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (in sd.)")), expand = c(0,0)) +
  scale_color_manual("", values = cols_var, labels = c("Ecosystem Limitation Index (ELI)","cor(SM',ET')",expression("cor(SW"["in"]*"',ET')"),expression("Shortwave incoming radiation (SW"["in"]*")"),"Soil moisture (SM)", "Terrestrial evaporation (ET)", "Leaf Area Index", "Aridity Index")) +
  scale_fill_manual("", values = cols_var, labels = c("Ecosystem Limitation Index (ELI)","cor(SM',ET')",expression("cor(SW"["in"]*"',ET')"),expression("Shortwave incoming radiation (SW"["in"]*")"),"Soil moisture (SM)", "Terrestrial evaporation (ET)", "Leaf Area Index", "Aridity Index")) +
  scale_linetype_manual("", values = c(rep(1,3),2:6), labels = c("Ecosystem Limitation Index (ELI)","cor(SM',ET')",expression("cor(SW"["in"]*"',ET')"),expression("Shortwave incoming radiation (SW"["in"]*")"),"Soil moisture (SM)", "Terrestrial evaporation (ET)", "Leaf Area Index", "Aridity Index")) +
  scale_shape_manual("", values = c(16,15,17,rep(16,5)), labels = c("Ecosystem Limitation Index (ELI)","cor(SM',ET')",expression("cor(SW"["in"]*"',ET')"),expression("Shortwave incoming radiation (SW"["in"]*")"),"Soil moisture (SM)", "Terrestrial evaporation (ET)", "Leaf Area Index", "Aridity Index")) +
  guides(color=guide_legend(ncol=2),
         fill=guide_legend(ncol=2),
         linetype=guide_legend(ncol=2),
         shape=guide_legend(ncol=2)) +
  theme(legend.position = c(.25,.875),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
m4

ggsave("testdir/SFig7.png", plot = m4, width = 10*1.25, height = 5*1.25, units = "in")

m4 <- ggplot(tseries.df, aes(x=tas_abs_1980,y=dcorr_rsds_abs_1980,group=source_id)) +
  geom_path(linetype = 'dashed', col = cols_var[1]) +
  geom_hline(yintercept=0) +
  scale_x_continuous(expression("T"[a]*" change since 1980 (K)"),
                     expand = c(0,0)) +
  scale_y_continuous(expression(paste("ELI change since 1980 (-)")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
m4
ggsave("testdir/SFig2.png", plot = m4, width = 10*1.25, height = 5*1.25, units = "in")

cols_var <- c(brewer.pal(n=11,'PiYG')[2],brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(11,"BrBG")[3], brewer.pal(8, "Set2")[6])

arrows.df <- data.frame("x" = c(1979),
                        "y" = c(0),
                        "xend" = c(1979),
                        "yend" = c(.03))

m4 <- ggplot(tseries.df, aes(x=year,y=dcorr_rsds_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = cols_var[1]) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_dcorr_rsds), col = cols_var[1], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_dcorr_rsds), col = cols_var[1], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_dcorr_rsds,ymax=mmmean_plussd_dcorr_rsds), col = cols_var[1], fill = cols_var[1], alpha = 0.5) +
  geom_hline(yintercept=0) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,20),
                     labels = seq(1980,2090,20), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.05,.22)) +
  annotate("segment", x = 1981, xend = 1981, y = 0, yend = 0.03, arrow = arrow(length = unit(0.33, "cm")), col=brewer.pal(11, "BrBG")[2], show.legend = F) +
  annotate("segment", x = 1981, xend = 1981, y = 0, yend = -0.03, arrow = arrow(length = unit(0.33, "cm")), col=brewer.pal(11, "BrBG")[10], show.legend = F) +
  annotate("text", x = 1989, y = .03, label = "Water limitation", angle = 0, col=brewer.pal(11, "BrBG")[2]) +
  annotate("text", x = 1990, y = -.025, label = "Energy limitation", angle = 0, col=brewer.pal(11, "BrBG")[10]) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle("a) Ecosystem Limitation Index (ELI) & components")
m4

m4_wtr <- ggplot(tseries.df, aes(x=year,y=corr_wtr_veg_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = brewer.pal(n=9,'BrBG')[2]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_wtr_veg), col = brewer.pal(n=9,'BrBG')[2], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_wtr_veg), col = brewer.pal(n=9,'BrBG')[2], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=perc25_corr_wtr_veg,ymax=perc75_corr_wtr_veg), col = brewer.pal(n=9,'BrBG')[2], fill = brewer.pal(n=9,'BrBG')[2], alpha = 0.5) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.03,.22)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle("cor(SM',ET')")
m4_wtr

m4_rgy <- ggplot(tseries.df, aes(x=year,y=corr_rgy_veg_rsds_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = brewer.pal(n=9,'BrBG')[8]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_rgy_veg_rsds), col = brewer.pal(n=9,'BrBG')[8], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_rgy_veg_rsds), col = brewer.pal(n=9,'BrBG')[8], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=perc25_corr_rgy_veg_rsds,ymax=perc75_corr_rgy_veg_rsds), col = brewer.pal(n=9,'BrBG')[8], fill = brewer.pal(n=9,'BrBG')[8], alpha = 0.5) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.22,.03)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle(expression("cor(SW"["in"]*"',ET')"))
m4_rgy

n4 <- ggplot(tseries.df, aes(x=year,y=rsds_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = cols_var[2]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_rsds), col = cols_var[2], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_rsds), col = cols_var[2], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=perc25_rsds,ymax=perc75_rsds), col = cols_var[2], fill = cols_var[2], alpha = 0.5) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle(expression("b) Shortwave inc. rad. (SW"["in"]*")"))
n4

o4 <- ggplot(tseries.df, aes(x=year,y=mrso_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = cols_var[3]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_mrso), col = cols_var[3], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_mrso), col = cols_var[3], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_mrso,ymax=mmmean_plussd_mrso), col = cols_var[3], fill = cols_var[3], alpha = 0.5) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (mm)")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("c) Soil moisture (SM)")
o4

# FlUXCOM 7.1 mm/yr/10yr 1982-1997
# 7.1 mm/yr/10yr --> (7.1 * 1.5) mm/yr/15yr --> (7.1 * 1.5) / (60^2*24*365) mm/s/15yr --> (7.1 * 1.5) / (1000*60^2*24*365) m3/m2/s/15yr
# (7.1 * 1.5 * 1000 * 2.26) / (1000*60^2*24*365) MJ/m2/s/15yr --> (7.1 * 1.5 * 2.26 * 10^6) / (60^2*24*365) W/m2/15yr
(7.1*1.5*10^6*2.26)/(60^2*24*365) # FLUXCOM 1982-1997 == 0.76 W/m2/15yr
(0.68*29*10^6*2.26)/(60^2*24*365) # PML 1982-2011 == 1.41 W/m2/29yr
(0.32*29*10^6*2.26)/(60^2*24*365) # MTE 1982-2011 == 0.67 W/m2/29yr
(0.38*29*10^6*2.26)/(60^2*24*365) # GLEAM 1982-2011 == 0.79 W/m2/29yr
(.13*8*10^6*2.26)/(60^2*24*365) # LandFlux-EVAL 1989-1997 == .075 W/m2/8yr
(-.18*7*10^6*2.26)/(60^2*24*365) # LandFlux-EVAL 1997-2005 == -.090 W/m2/8yr

ET_ext.df <- data.frame("year" = c(1982,1997,rep(c(1982,2011),3),1989,1997,2005),
                        "ET" = c(0,0.76,0,1.41,0,0.67,0,.79,0,0.075,-.016)/26.15741,
                        "dataset" = c(rep("M. Jung (2010)",2),rep("PML",2),rep("MTE",2),rep("GLEAM",2),rep("LandFlux-EVAL",3)))

# abs 1980
p4 <- ggplot(tseries.df, aes(x=year,y=hfls_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = cols_var[4]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hfls), col = cols_var[4], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hfls), col = cols_var[4], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_hfls,ymax=mmmean_plussd_hfls), col = cols_var[4], fill = cols_var[4], alpha = 0.5) +
  geom_point(inherit.aes = F, data = ET_ext.df, aes(x=year,y=ET,col=dataset), size = 1.5) +
  geom_line(inherit.aes = F, data = ET_ext.df, aes(x=year,y=ET,col=dataset), linetype = 'dashed') +
  scale_x_continuous("",
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0)) +
  scale_color_manual("data product",values = c(2,3,6,7,'slateblue3')) +
  theme(legend.position = c(.25,.78),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("d) Terrestrial evaporation (ET)")
p4

w4 <- ggplot(tseries.df, aes(x=year,y=lai_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = cols_var[5]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_lai), col = cols_var[5], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_lai), col = cols_var[5], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_lai,ymax=mmmean_plussd_lai), col = cols_var[5], fill = cols_var[5], alpha = 0.5) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (-)")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("e) Leaf Area Index")
w4

u4 <- ggplot(tseries.df, aes(x=year,y=ar_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = cols_var[7]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_ar), col = cols_var[7], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_ar), col = cols_var[7], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_ar,ymax=mmmean_plussd_ar), col = cols_var[7], fill = cols_var[7], alpha = 0.5) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (-)")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("f) Aridity Index")
u4


ELI_plots <- list(m4, m4_wtr, m4_rgy, n4, o4, p4, w4, u4)
plots <- align_cmip6_plots(ELI_plots)

ggsave("testdir/Fig1.eps", plot = plots, width = 18, height = 18, units = "in", device = cairo_ps)

# cols_var <- c(brewer.pal(n=11,'PiYG')[2],brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(11,"BrBG")[3], brewer.pal(8, "Set2")[6])
# 
# arrows.df <- data.frame("x" = c(1979),
#                         "y" = c(0),
#                         "xend" = c(1979),
#                         "yend" = c(.03))
# 
# m4 <- ggplot(tseries.df, aes(x=year,y=dcorr_abs_1980,group=source_id)) +
#   geom_line(linetype = 'dashed', col = cols_var[1]) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_dcorr), col = cols_var[1], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_dcorr), col = cols_var[1], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_dcorr,ymax=mmmean_plussd_dcorr), col = cols_var[1], fill = cols_var[1], alpha = 0.5) +
#   geom_hline(yintercept=0) +
#   scale_x_continuous("",
#                      breaks = seq(1980,2090,20),
#                      labels = seq(1980,2090,20), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.05,.2)) +
#   annotate("segment", x = 1981, xend = 1981, y = 0, yend = 0.045, arrow = arrow(length = unit(0.2, "cm")), col=brewer.pal(11, "BrBG")[2], show.legend = F) +
#   annotate("segment", x = 1981, xend = 1981, y = 0, yend = -0.045, arrow = arrow(length = unit(0.2, "cm")), col=brewer.pal(11, "BrBG")[10], show.legend = F) +
#   annotate("text", x = 1998, y = .05, label = "Water limitation", angle = 0, col=brewer.pal(11, "BrBG")[2]) +
#   annotate("text", x = 1999, y = -.03, label = "Energy limitation", angle = 0, col=brewer.pal(11, "BrBG")[10]) +
#   theme(legend.position = "none",
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7),
#         legend.key.width = unit(1.5, "cm"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"),
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) + ggtitle("a) Ecosystem Limitation Index (ELI) & components")
# m4
# 
# m4_wtr <- ggplot(tseries.df, aes(x=year,y=corr_wtr_veg_abs_1980,group=source_id)) + 
#   geom_line(linetype = 'dashed', col = brewer.pal(n=9,'BrBG')[2]) +
#   geom_hline(yintercept=0) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_wtr_veg), col = brewer.pal(n=9,'BrBG')[2], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_wtr_veg), col = brewer.pal(n=9,'BrBG')[2], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=perc25_corr_wtr_veg,ymax=perc75_corr_wtr_veg), col = brewer.pal(n=9,'BrBG')[2], fill = brewer.pal(n=9,'BrBG')[2], alpha = 0.5) +
#   scale_x_continuous("", 
#                      breaks = seq(1980,2090,30),
#                      labels = seq(1980,2090,30), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.05,.20)) +
#   theme(legend.position = "none",
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7),
#         legend.key.width = unit(1.5, "cm"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"), 
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) + ggtitle("cor(SM',ET')")
# m4_wtr
# 
# m4_rgy <- ggplot(tseries.df, aes(x=year,y=corr_rgy_veg_rsds_abs_1980,group=source_id)) + 
#   geom_line(linetype = 'dashed', col = brewer.pal(n=9,'BrBG')[8]) +
#   geom_hline(yintercept=0) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_rgy_veg_rsds), col = brewer.pal(n=9,'BrBG')[8], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_rgy_veg_rsds), col = brewer.pal(n=9,'BrBG')[8], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=perc25_corr_rgy_veg_rsds,ymax=perc75_corr_rgy_veg_rsds), col = brewer.pal(n=9,'BrBG')[8], fill = brewer.pal(n=9,'BrBG')[8], alpha = 0.5) +
#   scale_x_continuous("", 
#                      breaks = seq(1980,2090,30),
#                      labels = seq(1980,2090,30), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.2,.05)) +
#   theme(legend.position = "none",
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7),
#         legend.key.width = unit(1.5, "cm"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"), 
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) + ggtitle(expression("cor(SW"["in"]*"',ET')"))
# m4_rgy
# 
# n4 <- ggplot(tseries.df, aes(x=year,y=rsds_abs_1980,group=source_id)) + 
#   geom_line(linetype = 'dashed', col = cols_var[2]) +
#   geom_hline(yintercept=0) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_rsds), col = cols_var[2], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_rsds), col = cols_var[2], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=perc25_rsds,ymax=perc75_rsds), col = cols_var[2], fill = cols_var[2], alpha = 0.5) +
#   scale_x_continuous("", 
#                      breaks = seq(1980,2090,30),
#                      labels = seq(1980,2090,30), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0)) +
#   # scale_y_continuous(expression(paste("change since 1980 (K)")), expand = c(0,0)) +
#   theme(legend.position = "none",
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7),
#         legend.key.width = unit(1.5, "cm"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"), 
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) +
#   ggtitle(expression("b) Surface net radiation (R"[n]*")"))
# n4
# 
# o4 <- ggplot(tseries.df, aes(x=year,y=mrso_abs_1980,group=source_id)) + 
#   geom_line(linetype = 'dashed', col = cols_var[3]) +
#   geom_hline(yintercept=0) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_mrso), col = cols_var[3], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_mrso), col = cols_var[3], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_mrso,ymax=mmmean_plussd_mrso), col = cols_var[3], fill = cols_var[3], alpha = 0.5) +
#   scale_x_continuous("", 
#                      breaks = seq(1980,2090,30),
#                      labels = seq(1980,2090,30), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980 (mm)")), expand = c(0,0)) +
#   theme(legend.position = "none",
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7),
#         legend.key.width = unit(1.5, "cm"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"), 
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) +
#   ggtitle("c) Soil moisture (SM)")
# o4
# 
# # FlUXCOM 7.1 mm/yr/10yr 1982-1997
# # 7.1 mm/yr/10yr --> (7.1 * 1.5) mm/yr/15yr --> (7.1 * 1.5) / (60^2*24*365) mm/s/15yr --> (7.1 * 1.5) / (1000*60^2*24*365) m3/m2/s/15yr
# # (7.1 * 1.5 * 1000 * 2.26) / (1000*60^2*24*365) MJ/m2/s/15yr --> (7.1 * 1.5 * 2.26 * 10^6) / (60^2*24*365) W/m2/15yr
# (7.1*1.5*10^6*2.26)/(60^2*24*365) # FLUXCOM 1982-1997 == 0.76 W/m2/15yr
# (0.68*29*10^6*2.26)/(60^2*24*365) # PML 1982-2011 == 1.41 W/m2/29yr
# (0.32*29*10^6*2.26)/(60^2*24*365) # MTE 1982-2011 == 0.67 W/m2/29yr
# (0.38*29*10^6*2.26)/(60^2*24*365) # GLEAM 1982-2011 == 0.79 W/m2/29yr
# (.13*8*10^6*2.26)/(60^2*24*365) # LandFlux-EVAL 1989-1997 == .075 W/m2/8yr
# (-.18*7*10^6*2.26)/(60^2*24*365) # LandFlux-EVAL 1997-2005 == -.090 W/m2/8yr
# 
# ET_ext.df <- data.frame("year" = c(1982,1997,rep(c(1982,2011),3),1989,1997,2005),
#                         "ET" = c(0,0.76,0,1.41,0,0.67,0,.79,0,0.075,-.016)/26.15741,
#                         "dataset" = c(rep("M. Jung (2010)",2),rep("PML",2),rep("MTE",2),rep("GLEAM",2),rep("LandFlux-EVAL",3)))
# 
# # abs 1980
# p4 <- ggplot(tseries.df, aes(x=year,y=hfls_abs_1980,group=source_id)) + 
#   geom_line(linetype = 'dashed', col = cols_var[4]) +
#   geom_hline(yintercept=0) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hfls), col = cols_var[4], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hfls), col = cols_var[4], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_hfls,ymax=mmmean_plussd_hfls), col = cols_var[4], fill = cols_var[4], alpha = 0.5) +
#   geom_point(inherit.aes = F, data = ET_ext.df, aes(x=year,y=ET,col=dataset), size = 1.25) +
#   geom_line(inherit.aes = F, data = ET_ext.df, aes(x=year,y=ET,col=dataset), linetype = 'dashed') +
#   scale_x_continuous("", 
#                      breaks = seq(1980,2090,30),
#                      labels = seq(1980,2090,30), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0)) +
#   scale_color_manual("data product",values = c(2,3,6,7,'slateblue3')) +
#   guides(col = guide_legend(byrow = T)) +
#   theme(legend.position = c(.3,.7),
#         legend.spacing.y = unit(.001, 'mm'),
#         legend.key.size = unit(.33, "cm"),
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         legend.title = element_blank(),
#         legend.key = element_rect(colour = "transparent", fill = "transparent"),
#         legend.background = element_rect(colour = "transparent", fill = "transparent"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"), 
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) +
#   ggtitle("d) Terrestrial evaporation (ET)")
# p4
# 
# w4 <- ggplot(tseries.df, aes(x=year,y=lai_abs_1980,group=source_id)) + 
#   geom_line(linetype = 'dashed', col = cols_var[5]) +
#   geom_hline(yintercept=0) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_lai), col = cols_var[5], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_lai), col = cols_var[5], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_lai,ymax=mmmean_plussd_lai), col = cols_var[5], fill = cols_var[5], alpha = 0.5) +
#   scale_x_continuous("", 
#                      breaks = seq(1980,2090,30),
#                      labels = seq(1980,2090,30), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980 (-)")), expand = c(0,0)) +
#   theme(legend.position = "none",
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7),
#         legend.key.width = unit(1.5, "cm"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"), 
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) +
#   ggtitle("e) Leaf Area Index")
# w4
# 
# u4 <- ggplot(tseries.df, aes(x=year,y=ar_abs_1980,group=source_id)) + 
#   geom_line(linetype = 'dashed', col = cols_var[7]) +
#   geom_hline(yintercept=0) +
#   geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_ar), col = cols_var[7], size = 1.75) +
#   geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_ar), col = cols_var[7], linetype = 'solid', size = .8) +
#   geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_ar,ymax=mmmean_plussd_ar), col = cols_var[7], fill = cols_var[7], alpha = 0.5) +
#   scale_x_continuous("", 
#                      breaks = seq(1980,2090,30),
#                      labels = seq(1980,2090,30), expand = c(0,0)) +
#   scale_y_continuous(expression(paste("change since 1980 (-)")), expand = c(0,0)) +
#   theme(legend.position = "none",
#         text = element_text(size = 7),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7),
#         legend.key.width = unit(1.5, "cm"),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         # axis.text = element_text(size=7),
#         axis.title.x = element_blank(),
#         # axis.title = element_text(size=7),
#         # plot.title = element_text(size=7),
#         axis.line.y.right = element_line(color = "grey80"), 
#         axis.ticks.y.right = element_line(color = "grey80"),
#         axis.text.y.right = element_text(color = "grey80"),
#         axis.title.y.right = element_text(color = "grey80")
#         
#   ) +
#   ggtitle("f) Aridity Index")
# u4
# 
# 
# ELI_plots <- list(m4, m4_wtr, m4_rgy, n4, o4, p4, w4, u4)
# plots <- align_cmip6_plots(ELI_plots)
# 
# ggsave("testdir/Fig1.eps", plot = plots, width = 180, height = 180, units = "mm", device = cairo_ps)

m4 <- ggplot(tseries.df, aes(x=year,y=dcorr_abs_1980,group=source_id)) +
  geom_line(linetype = 'dashed', col = cols_var[1]) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_dcorr), col = cols_var[1], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_dcorr), col = cols_var[1], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_dcorr,ymax=mmmean_plussd_dcorr), col = cols_var[1], fill = cols_var[1], alpha = 0.5) +
  geom_hline(yintercept=0) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,20),
                     labels = seq(1980,2090,20), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.03,.23)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle("a) Ecosystem Limitation Index (ELI)")
m4

m4_rgy <- ggplot(tseries.df, aes(x=year,y=corr_rgy_veg_abs_1980,group=source_id)) + 
  geom_line(linetype = 'dashed', col = brewer.pal(n=9,'BrBG')[8]) +
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_rgy_veg), col = brewer.pal(n=9,'BrBG')[8], size = 2.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_corr_rgy_veg), col = brewer.pal(n=9,'BrBG')[8], linetype = 'solid', size = 1.2) +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=perc25_corr_rgy_veg,ymax=perc75_corr_rgy_veg), col = brewer.pal(n=9,'BrBG')[8], fill = brewer.pal(n=9,'BrBG')[8], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0), limits = c(-.18,.03)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle(expression("b) cor(T"[a]*"',ET')"))
m4_rgy


plot_tas <- plot_grid(m4, m4_rgy, ncol = 2)

ggsave("testdir/SFig1.png", plot = plot_tas, width = 10*1.25, height = 5*1.25, units = "in")

mmmtseries_pcorr.df <- data.frame("value" = c(mmmtseries.df$mmmean_dcorr_rsds, mmmtseries.df$mmmean_dpcorr,
                                              mmmtseries.df$mmmean_corr_rgy_veg_rsds, mmmtseries.df$mmmean_pcorr_rgy_veg,
                                              mmmtseries.df$mmmean_corr_wtr_veg_rsds, mmmtseries.df$mmmean_pcorr_wtr_veg),
                                  "year" = rep(seq(1980,2090,10),6),
                                  "var" = c(rep("ELI",12),rep("pELI",12),rep("corr_rgy_veg",12),rep("pcorr_rgy_veg",12),rep("corr_wtr_veg",12),rep("pcorr_wtr_veg",12)))

mmmtseries_pcorr.df$var_factor <- factor(mmmtseries_pcorr.df$var, levels = c("ELI","pELI","corr_rgy_veg","pcorr_rgy_veg","corr_wtr_veg","pcorr_wtr_veg"))

cols_pcorr <- c("ELI" = 'black',
                "pELI" = 'slategrey',
                "corr_rgy_veg" = brewer.pal(9,"Reds")[8],
                "pcorr_rgy_veg" = brewer.pal(9,"Reds")[4],
                "corr_wtr_veg" = brewer.pal(9,"Blues")[8],
                "pcorr_wtr_veg" = brewer.pal(9,"Blues")[4])

linetypes_pcorr <- c("ELI" = 'solid',
                     "pELI" = 'dotted',
                     "corr_rgy_veg" = 'solid',
                     "pcorr_rgy_veg" = 'dotted',
                     "corr_wtr_veg" = 'solid',
                     "pcorr_wtr_veg" = 'dotted')

m4 <- ggplot(mmmtseries_pcorr.df, aes(x=year,y=value,col=var_factor,linetype=var_factor)) +
  geom_point(size = 1.5) +
  geom_line() +
  geom_hline(yintercept=0) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,20),
                     labels = seq(1980,2090,20), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0)) +
  scale_color_manual("variable", 
                     values = cols_pcorr,
                     labels = c("ELI (cor)", "ELI (pcor)", expression("cor(SW"["in"]*"',ET')"), expression("cor(SW"["in"]*"',ET'|SM')"), "cor(SM',ET')", expression("cor(SM',ET'|SW"["in"]*"')"))) +
  scale_linetype_manual("variable",
                        values = linetypes_pcorr,
                        labels = c("ELI (cor)", "ELI (pcor)", expression("cor(SW"["in"]*"',ET')"), expression("cor(SW"["in"]*"',ET'|SM')"), "cor(SM',ET')", expression("cor(SM',ET'|SW"["in"]*"')"))) +
  theme(legend.position = c(.1,.85),
        legend.text = element_text(size=12),
        # legend.title = element_text(size=20),
        legend.title = element_blank(),
        # legend.key.width = unit(1.5, "cm"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
m4
ggsave("testdir/SFig6.png", plot = m4, width = 10*1.25, height = 5*1.25, units = "in")

# /RData
load('RData/202112_dcorr_cmip6_10yr.RData')
load('RData/202112_dcorr_cmip6_10yr_rsds_evspsblveg.RData')
# # /testdir
# load('testdir/202112_dcorr_cmip6_10yr_rsds_evspsblveg.RData')

# select the right models and put in array
dcorr_t_all.array <- 
  corr_rgy_veg_t_all.array <- 
  corr_wtr_veg_t_all.array <- 
  av_hurs.array <- 
  av_evspsblveg.array <- array(NaN,c(180,90,7*12)) # 7 different source_id and 12*10 years (total: 84)
count_all <- 1
for(i in 1:7){
  dcorr_t_all.array[,,count_all:(count_all+11)] <- dcorr_t.list[[i]]
  corr_rgy_veg_t_all.array[,,count_all:(count_all+11)] <- corr_rgy_veg_t.list[[i]]
  corr_wtr_veg_t_all.array[,,count_all:(count_all+11)] <- corr_wtr_veg_t.list[[i]]
  av_hurs.array[,,count_all:(count_all+11)] <- av_hurs.list[[i]]
  av_evspsblveg.array[,,count_all:(count_all+11)] <- av_evspsblveg.list[[i]]
  count_all <- count_all + 12
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,7*12)); test[1:90,,] <- dcorr_t_all.array[91:180,,]; test[91:180,,] <- dcorr_t_all.array[1:90,,]; dcorr_t_all.array <- test; dcorr_t_all.array <- -1*dcorr_t_all.array
test <- array(NaN,c(180,90,7*12)); test[1:90,,] <- corr_rgy_veg_t_all.array[91:180,,]; test[91:180,,] <- corr_rgy_veg_t_all.array[1:90,,]; corr_rgy_veg_t_all.array <- test; corr_rgy_veg_t_all.array <- corr_rgy_veg_t_all.array
test <- array(NaN,c(180,90,7*12)); test[1:90,,] <- corr_wtr_veg_t_all.array[91:180,,]; test[91:180,,] <- corr_wtr_veg_t_all.array[1:90,,]; corr_wtr_veg_t_all.array <- test; corr_wtr_veg_t_all.array <- corr_wtr_veg_t_all.array
test <- array(NaN,c(180,90,7*12)); test[1:90,,] <- av_hurs.array[91:180,,]; test[91:180,,] <- av_hurs.array[1:90,,]; av_hurs.array <- test
test <- array(NaN,c(180,90,7*12)); test[1:90,,] <- av_evspsblveg.array[91:180,,]; test[91:180,,] <- av_evspsblveg.array[1:90,,]; av_evspsblveg.array <- test

# get 7 models radiation from av_rnet.array, av_hfls.array, av_pr.array, av_tas_all.array c(1,2,4,5,6,8,10)
av_rsds.array <- av_rnet.array[,,c(1:12,13:24,37:48,49:60,61:72,85:96,109:120)]
av_hfls_all.array <- av_hfls_all.array[,,c(1:12,13:24,37:48,49:60,61:72,85:96,109:120)]
av_pr.array <- av_pr.array[,,c(1:12,13:24,37:48,49:60,61:72,85:96,109:120)]
av_tas_all.array <- av_tas_all.array[,,c(1:12,13:24,37:48,49:60,61:72,85:96,109:120)]

# VPsat
VPsat <- 610.7*10^((7.5*(av_tas_all.array-273.15))/(237.3+(av_tas_all.array-273.15)))/1000
VPair <- (610.7*10^((7.5*(av_tas_all.array-273.15))/(237.3+(av_tas_all.array-273.15)))/1000)*av_hurs.array/100

av_VPD.array <- VPsat - VPair
# convert av_evspsblveg.array to W/m2
av_evspsblveg.array <- av_evspsblveg.array/26.15741*2.26*10^6
av_T_ET.array <- (av_evspsblveg.array)/av_hfls_all.array * 100



av_baresoilevap.array <- av_hfls_all.array - av_evspsblveg.array

# Calculate area-weighted average dcorr,tas,mrso,hfls
tseries.df <- setNames(data.frame(matrix(ncol = 15, nrow = 0)),
                       c("dcorr","corr_rgy_veg", "corr_wtr_veg", "dcorr_t", "corr_rgy_veg_t", "corr_wtr_veg_t", 
                         "hurs","VPD","evspsblveg", "hfls", "T_ET", "pr","rsds","baresoilevap","tas"))
for(i in 1:84){
  tseries.df <- rbind(tseries.df, 
                      data.frame("dcorr" = weighted.mean(x = mask_obs*dcorr_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_rgy_veg" = weighted.mean(x = mask_obs*corr_rgy_veg_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_wtr_veg" = weighted.mean(x = mask_obs*corr_wtr_veg_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "dcorr_t" = weighted.mean(x = mask_obs*dcorr_t_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_rgy_veg_t" = weighted.mean(x = mask_obs*corr_rgy_veg_t_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "corr_wtr_veg_t" = weighted.mean(x = mask_obs*corr_wtr_veg_t_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "hurs" = weighted.mean(x = mask_obs*av_hurs.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "VPD" = weighted.mean(x = mask_obs*av_VPD.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "evspsblveg" = weighted.mean(x = mask_obs*av_evspsblveg.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "hfls" = weighted.mean(x = mask_obs*av_hfls_all.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "T_ET" = weighted.mean(x = mask_obs*av_T_ET.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "pr" = weighted.mean(x = mask_obs*av_pr.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "rsds" = weighted.mean(x = mask_obs*av_rsds.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "baresoilevap" = weighted.mean(x = mask_obs*av_baresoilevap.array[,,i], w = area.array*mask_obs, na.rm = T),
                                 "tas" = weighted.mean(x = mask_obs*av_tas_all.array[,,i], w = area.array*mask_obs, na.rm = T)))
}
source_id <- setNames(data.frame(matrix(ncol = 1, nrow = 0)),
                      c("source_id"))
for(i in c(1,2,4,5,6,8,10)){
  source_id <- rbind(source_id,
                     data.frame("source_id" = rep(cmip6_data.df$source_id[i],12)))
}
tseries.df$source_id <- source_id$source_id
tseries.df$year <- rep(c(seq(1980,2090,10)),7)

tseries.df$dcorr_abs_1980 <-
  tseries.df$corr_rgy_veg_abs_1980 <- 
  tseries.df$corr_wtr_veg_abs_1980 <- 
  tseries.df$dcorr_t_abs_1980 <-
  tseries.df$corr_rgy_veg_t_abs_1980 <- 
  tseries.df$corr_wtr_veg_t_abs_1980 <- 
  tseries.df$hurs_abs_1980 <-
  tseries.df$VPD_abs_1980 <-
  tseries.df$evspsblveg_abs_1980 <- 
  tseries.df$hfls_abs_1980 <- 
  tseries.df$T_ET_abs_1980 <- 
  tseries.df$pr_abs_1980 <- 
  tseries.df$rsds_abs_1980 <- 
  tseries.df$baresoilevap_abs_1980 <- 
  tseries.df$tas_abs_1980 <- NaN

for(i in 1:84){
  tseries.df$dcorr_abs_1980[i] <- (tseries.df$dcorr[i] - tseries.df$dcorr[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_rgy_veg_abs_1980[i] <- (tseries.df$corr_rgy_veg[i] - tseries.df$corr_rgy_veg[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_wtr_veg_abs_1980[i] <- (tseries.df$corr_wtr_veg[i] - tseries.df$corr_wtr_veg[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$dcorr_t_abs_1980[i] <- (tseries.df$dcorr_t[i] - tseries.df$dcorr_t[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_rgy_veg_t_abs_1980[i] <- (tseries.df$corr_rgy_veg_t[i] - tseries.df$corr_rgy_veg_t[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$corr_wtr_veg_t_abs_1980[i] <- (tseries.df$corr_wtr_veg_t[i] - tseries.df$corr_wtr_veg_t[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$hurs_abs_1980[i] <- (tseries.df$hurs[i] - tseries.df$hurs[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$VPD_abs_1980[i] <- (tseries.df$VPD[i] - tseries.df$VPD[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$hfls_abs_1980[i] <- (tseries.df$hfls[i] - tseries.df$hfls[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$evspsblveg_abs_1980[i] <- (tseries.df$evspsblveg[i] - tseries.df$evspsblveg[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$T_ET_abs_1980[i] <- (tseries.df$T_ET[i] - tseries.df$T_ET[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$pr_abs_1980[i] <- (tseries.df$pr[i] - tseries.df$pr[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$rsds_abs_1980[i] <- (tseries.df$rsds[i] - tseries.df$rsds[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$baresoilevap_abs_1980[i] <- (tseries.df$baresoilevap[i] - tseries.df$baresoilevap[which(tseries.df$source_id == tseries.df$source_id[i])][1])
  tseries.df$tas_abs_1980[i] <- (tseries.df$tas[i] - tseries.df$tas[which(tseries.df$source_id == tseries.df$source_id[i])][1])
}

mmmtseries.df <- data.frame("year" = seq(1980,2090,10))
mmindex <- rep(seq(1,12),7)
for(i in 1:12){
  mmmtseries.df$mmmedian_dcorr[i] <- median(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_dcorr[i] <- quantile(tseries.df$dcorr_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_dcorr[i] <- quantile(tseries.df$dcorr_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_dcorr[i] <- mean(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_dcorr[i] <- mmmtseries.df$mmmean_dcorr[i] - sd(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_dcorr[i] <- mmmtseries.df$mmmean_dcorr[i] + sd(tseries.df$dcorr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_rgy_veg[i] <- median(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_rgy_veg[i] <- quantile(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_rgy_veg[i] <- quantile(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_rgy_veg[i] <- mean(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_rgy_veg[i] <- mmmtseries.df$mmmean_corr_rgy_veg[i] - sd(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_rgy_veg[i] <- mmmtseries.df$mmmean_corr_rgy_veg[i] + sd(tseries.df$corr_rgy_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_wtr_veg[i] <- median(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_wtr_veg[i] <- quantile(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_wtr_veg[i] <- quantile(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_wtr_veg[i] <- mean(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_wtr_veg[i] <- mmmtseries.df$mmmean_corr_wtr_veg[i] - sd(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_wtr_veg[i] <- mmmtseries.df$mmmean_corr_wtr_veg[i] + sd(tseries.df$corr_wtr_veg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_dcorr_t[i] <- median(tseries.df$dcorr_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_dcorr_t[i] <- quantile(tseries.df$dcorr_t_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_dcorr_t[i] <- quantile(tseries.df$dcorr_t_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_dcorr_t[i] <- mean(tseries.df$dcorr_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_dcorr_t[i] <- mmmtseries.df$mmmean_dcorr_t[i] - sd(tseries.df$dcorr_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_dcorr_t[i] <- mmmtseries.df$mmmean_dcorr_t[i] + sd(tseries.df$dcorr_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_rgy_veg_t[i] <- median(tseries.df$corr_rgy_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_rgy_veg_t[i] <- quantile(tseries.df$corr_rgy_veg_t_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_rgy_veg_t[i] <- quantile(tseries.df$corr_rgy_veg_t_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_rgy_veg_t[i] <- mean(tseries.df$corr_rgy_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_rgy_veg_t[i] <- mmmtseries.df$mmmean_corr_rgy_veg_t[i] - sd(tseries.df$corr_rgy_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_rgy_veg_t[i] <- mmmtseries.df$mmmean_corr_rgy_veg_t[i] + sd(tseries.df$corr_rgy_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_corr_wtr_veg_t[i] <- median(tseries.df$corr_wtr_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_corr_wtr_veg_t[i] <- quantile(tseries.df$corr_wtr_veg_t_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_corr_wtr_veg_t[i] <- quantile(tseries.df$corr_wtr_veg_t_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_corr_wtr_veg_t[i] <- mean(tseries.df$corr_wtr_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_corr_wtr_veg_t[i] <- mmmtseries.df$mmmean_corr_wtr_veg_t[i] - sd(tseries.df$corr_wtr_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_corr_wtr_veg_t[i] <- mmmtseries.df$mmmean_corr_wtr_veg_t[i] + sd(tseries.df$corr_wtr_veg_t_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_hurs[i] <- median(tseries.df$hurs_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_hurs[i] <- quantile(tseries.df$hurs_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_hurs[i] <- quantile(tseries.df$hurs_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_hurs[i] <- mean(tseries.df$hurs_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_hurs[i] <- mmmtseries.df$mmmean_hurs[i] - sd(tseries.df$hurs_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_hurs[i] <- mmmtseries.df$mmmean_hurs[i] + sd(tseries.df$hurs_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_VPD[i] <- median(tseries.df$VPD_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_VPD[i] <- quantile(tseries.df$VPD_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_VPD[i] <- quantile(tseries.df$VPD_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_VPD[i] <- mean(tseries.df$VPD_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_VPD[i] <- mmmtseries.df$mmmean_VPD[i] - sd(tseries.df$VPD_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_VPD[i] <- mmmtseries.df$mmmean_VPD[i] + sd(tseries.df$VPD_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_hfls[i] <- median(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_hfls[i] <- quantile(tseries.df$hfls_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_hfls[i] <- quantile(tseries.df$hfls_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_hfls[i] <- mean(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_hfls[i] <- mmmtseries.df$mmmean_hfls[i] - sd(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_hfls[i] <- mmmtseries.df$mmmean_hfls[i] + sd(tseries.df$hfls_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_evspsblveg[i] <- median(tseries.df$evspsblveg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_evspsblveg[i] <- quantile(tseries.df$evspsblveg_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_evspsblveg[i] <- quantile(tseries.df$evspsblveg_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_evspsblveg[i] <- mean(tseries.df$evspsblveg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_evspsblveg[i] <- mmmtseries.df$mmmean_evspsblveg[i] - sd(tseries.df$evspsblveg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_evspsblveg[i] <- mmmtseries.df$mmmean_evspsblveg[i] + sd(tseries.df$evspsblveg_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_T_ET[i] <- median(tseries.df$T_ET_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_T_ET[i] <- quantile(tseries.df$T_ET_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_T_ET[i] <- quantile(tseries.df$T_ET_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_T_ET[i] <- mean(tseries.df$T_ET_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_T_ET[i] <- mmmtseries.df$mmmean_T_ET[i] - sd(tseries.df$T_ET_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_T_ET[i] <- mmmtseries.df$mmmean_T_ET[i] + sd(tseries.df$T_ET_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_pr[i] <- median(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_pr[i] <- quantile(tseries.df$pr_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_pr[i] <- quantile(tseries.df$pr_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_pr[i] <- mean(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_pr[i] <- mmmtseries.df$mmmean_pr[i] - sd(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_pr[i] <- mmmtseries.df$mmmean_pr[i] + sd(tseries.df$pr_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_rsds[i] <- median(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_rsds[i] <- quantile(tseries.df$rsds_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_rsds[i] <- quantile(tseries.df$rsds_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_rsds[i] <- mean(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_rsds[i] <- mmmtseries.df$mmmean_rsds[i] - sd(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_rsds[i] <- mmmtseries.df$mmmean_rsds[i] + sd(tseries.df$rsds_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_baresoilevap[i] <- median(tseries.df$baresoilevap_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_baresoilevap[i] <- quantile(tseries.df$baresoilevap_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_baresoilevap[i] <- quantile(tseries.df$baresoilevap_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_baresoilevap[i] <- mean(tseries.df$baresoilevap_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_baresoilevap[i] <- mmmtseries.df$mmmean_baresoilevap[i] - sd(tseries.df$baresoilevap_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_baresoilevap[i] <- mmmtseries.df$mmmean_baresoilevap[i] + sd(tseries.df$baresoilevap_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmedian_tas[i] <- median(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$perc25_tas[i] <- quantile(tseries.df$tas_abs_1980[which(mmindex == i)], prob = c(.25), na.rm = T)
  mmmtseries.df$perc75_tas[i] <- quantile(tseries.df$tas_abs_1980[which(mmindex == i)], prob = c(.75), na.rm = T)
  mmmtseries.df$mmmean_tas[i] <- mean(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_minsd_tas[i] <- mmmtseries.df$mmmean_tas[i] - sd(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
  mmmtseries.df$mmmean_plussd_tas[i] <- mmmtseries.df$mmmean_tas[i] + sd(tseries.df$tas_abs_1980[which(mmindex == i)], na.rm = T)
}

qa4 <- ggplot(tseries.df, aes(x=year,y=hfls_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hfls), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hfls), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_hfls,ymax=mmmean_plussd_hfls), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0), limits = c(-.025,.25)) +
  scale_color_manual(values = rep(brewer.pal(n=9,'Oranges')[6],12)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("a) Terrestrial evaporation (ET)")
qa4

qc4 <- ggplot(tseries.df, aes(x=year,y=baresoilevap_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_baresoilevap), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_baresoilevap), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_baresoilevap,ymax=mmmean_plussd_baresoilevap), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0), limits = c(-.025,.25)) +
  scale_color_manual(values = rep(brewer.pal(n=9,'Oranges')[6],12)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("c) ET - T")
qc4

qb4 <- ggplot(tseries.df, aes(x=year,y=evspsblveg_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_evspsblveg), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_evspsblveg), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_evspsblveg,ymax=mmmean_plussd_evspsblveg), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0), limits = c(-.025,.25)) +
  scale_color_manual(values = rep(brewer.pal(n=9,'Oranges')[6],12)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("b) Plant transpiration (T)")
qb4

qd4 <- ggplot(tseries.df, aes(x=year,y=T_ET_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_T_ET), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_T_ET), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_T_ET,ymax=mmmean_plussd_T_ET), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (%)")), expand = c(0,0)) +
  scale_color_manual(values = rep(brewer.pal(n=9,'Oranges')[6],12)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("d) T/ET")
qd4

plots <- plot_grid(qa4, qb4, qc4, qd4, rel_heights = c(1/2,1/2), align = 'v')

ggsave("testdir/SFig4.png", plot = plots, width = 10*1.5, height = 10*1.5, units = "in")


s4 <- ggplot(tseries.df, aes(x=year,y=hurs_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hurs), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_hurs), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_hurs,ymax=mmmean_plussd_hurs), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (%)")), expand = c(0,0)) +
  scale_color_manual(values = rep(brewer.pal(n=9,'Oranges')[6],12)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("a) Relative humidity")
s4

t4 <- ggplot(tseries.df, aes(x=year,y=VPD_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_VPD), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_VPD), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_VPD,ymax=mmmean_plussd_VPD), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (kPa)")), expand = c(0,0)) +
  scale_color_manual(values = rep(brewer.pal(n=9,'Oranges')[6],12)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("b) Vapour pressure deficit")
t4

v4 <- ggplot(tseries.df, aes(x=year,y=pr_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_pr), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_pr), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_pr,ymax=mmmean_plussd_pr), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (mm/d)")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("d) Precipitation")
v4

w4 <- ggplot(tseries.df, aes(x=year,y=tas_abs_1980,col=source_id)) + 
  geom_hline(yintercept=0) +
  geom_point(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_tas), col = 1, size = 1.5) +
  geom_line(inherit.aes = F, data = mmmtseries.df, aes(x=year,y=mmmean_tas), col = 1, linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = mmmtseries.df, aes(x=year,ymin=mmmean_minsd_tas,ymax=mmmean_plussd_tas), col = brewer.pal(n=9,'Greys')[5], alpha = 0.5) +
  scale_x_continuous("", 
                     breaks = seq(1980,2090,30),
                     labels = seq(1980,2090,30), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980 (K)")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) +
  ggtitle("c) Air temperature")
w4

plots <- plot_grid(s4, t4, w4, v4, rel_heights = c(1/2,1/2), align = 'v')

ggsave("testdir/SFig3.png", plot = plots, width = 10*1.5, height = 10*1.5, units = "in")


mmmtseries_t.df <- data.frame("value" = c(mmmtseries.df$mmmean_dcorr, mmmtseries.df$mmmean_dcorr_t,
                                          mmmtseries.df$mmmean_corr_rgy_veg,mmmtseries.df$mmmean_corr_rgy_veg_t,
                                          mmmtseries.df$mmmean_corr_wtr_veg,mmmtseries.df$mmmean_corr_wtr_veg_t),
                              "year" = rep(seq(1980,2090,10),6),
                              "var" = c(rep("ELI (w/ evaporation)",12),rep("ELI (/w transpiration)",12),rep("corr_rgy_veg",12),rep("corr_rgy_veg_t",12),rep("corr_wtr_veg",12),rep("corr_wtr_veg_t",12)))

mmmtseries_t.df$var_factor <- factor(mmmtseries_t.df$var, levels = c("ELI (w/ evaporation)","ELI (/w transpiration)","corr_rgy_veg","corr_rgy_veg_t","corr_wtr_veg","corr_wtr_veg_t"))

cols_t <- c('ELI (w/ evaporation)' = 'black',
            'ELI (/w transpiration)' = brewer.pal(9,"Greens")[8],
            'corr_rgy_veg' = brewer.pal(9,"Reds")[8],
            'corr_rgy_veg_t' = brewer.pal(9,"YlGn")[3],
            'corr_wtr_veg' = brewer.pal(9,"Blues")[8],
            'corr_wtr_veg_t' = brewer.pal(9,"GnBu")[5])

linetypes_t <- c('ELI (w/ evaporation)' = 'solid',
                 'ELI (/w transpiration)' = 'dashed',
                 'corr_rgy_veg' = 'solid',
                 'corr_rgy_veg_t' = 'dashed',
                 'corr_wtr_veg' = 'solid',
                 'corr_wtr_veg_t' = 'dashed')

m4 <- ggplot(mmmtseries_t.df, aes(x=year,y=value,col=var_factor,linetype=var_factor)) +
  geom_point(size = 1.5) +
  geom_line() +
  geom_hline(yintercept=0) +
  scale_x_continuous("",
                     breaks = seq(1980,2090,20),
                     labels = seq(1980,2090,20), expand = c(0,0)) +
  scale_y_continuous(expression(paste("change since 1980")), expand = c(0,0)) +
  scale_color_manual("variable", 
                     values = cols_t,
                     labels = c(expression("ELI"), expression("ELI"[T]), expression("cor(SW"["in"]*"',ET')"), expression("cor(SW"["in"]*"',T')"), "cor(SM',ET')", expression("cor(SM',T')"))) +
  scale_linetype_manual("variable", 
                        values = linetypes_t,
                        labels = c(expression("ELI"), expression("ELI"[T]), expression("cor(SW"["in"]*"',ET')"), expression("cor(SW"["in"]*"',T')"), "cor(SM',ET')", expression("cor(SM',T')"))) +
  theme(legend.position = c(.1,.85),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
m4
ggsave("testdir/SFig5.png", plot = m4, width = 10*1.25, height = 5*1.25, units = "in")


