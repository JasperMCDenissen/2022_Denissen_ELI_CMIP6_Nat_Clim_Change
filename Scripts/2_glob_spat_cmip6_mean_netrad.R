# Script to make Figure 2 and Supplementary Figures XXX
# 04-01-2021
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
# function
source('Scripts/functions/plot_discrete_cbar.R')

##########################################################################################
##########################################################################################
################################## barplot treeFrac (start) ##############################
##########################################################################################
##########################################################################################

# from /RData
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
load('RData/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
load('RData/total_land_area.RData')
load('RData/202106_cmip6_frac.RData')
# # from /testdir
# load('testdir/202106_cmip6_no_evspsblveg_hurs.RData')
# load('testdir/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs.RData')
# load('testdir/total_land_area.RData')
# load("testdir/202106_cmip6_frac.RData")


dcorr.list <- list(dcorr.list[[9]], dcorr.list[[8]], dcorr.list[[10]], dcorr.list[[2]],
                   dcorr.list[[4]], dcorr.list[[6]])
dcorr_netrad.list <- list(dcorr_netrad.list[[9]], dcorr_netrad.list[[8]], dcorr_netrad.list[[10]], dcorr_netrad.list[[2]],
                   dcorr_netrad.list[[4]], dcorr_netrad.list[[6]])
av_tas.list <- list(av_tas.list[[9]], av_tas.list[[8]], av_tas.list[[10]], av_tas.list[[2]],
                    av_tas.list[[4]], av_tas.list[[6]])
av_mrso.list <- list(av_mrso.list[[9]], av_mrso.list[[8]], av_mrso.list[[10]], av_mrso.list[[2]],
                     av_mrso.list[[4]], av_mrso.list[[6]])
av_hfls.list <- list(av_hfls.list[[9]], av_hfls.list[[8]], av_hfls.list[[10]], av_hfls.list[[2]],
                     av_hfls.list[[4]], av_hfls.list[[6]])
av_lai.list <- list(av_lai.list[[9]], av_lai.list[[8]], av_lai.list[[10]], av_lai.list[[2]],
                    av_lai.list[[4]], av_lai.list[[6]])
av_rsds.list <- list(av_rsds.list[[9]], av_rsds.list[[8]], av_rsds.list[[10]], av_rsds.list[[2]],
                     av_rsds.list[[4]], av_rsds.list[[6]])
av_rsus.list <- list(av_rsus.list[[9]], av_rsus.list[[8]], av_rsus.list[[10]], av_rsus.list[[2]],
                     av_rsus.list[[4]], av_rsus.list[[6]])
av_rlds.list <- list(av_rlds.list[[9]], av_rlds.list[[8]], av_rlds.list[[10]], av_rlds.list[[2]],
                     av_rlds.list[[4]], av_rlds.list[[6]])
av_rlus.list <- list(av_rlus.list[[9]], av_rlus.list[[8]], av_rlus.list[[10]], av_rlus.list[[2]],
                     av_rlus.list[[4]], av_rlus.list[[6]])
av_pr.list <- list(av_pr.list[[9]], av_pr.list[[8]], av_pr.list[[10]], av_pr.list[[2]],
                   av_pr.list[[4]], av_pr.list[[6]])

lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# select the right models and put in array
dcorr_all.array <- 
  dcorr_netrad.array <- 
  av_tas.array <- 
  av_mrso.array <- 
  av_lai.array <- 
  av_hfls.array <- 
  av_rlds.array <- 
  av_rlus.array <- 
  av_rsds.array <- 
  av_rsus.array <- 
  av_cropFrac.array <-
  av_treeFrac.array <-
  av_pr.array <- array(NaN,c(180,90,6*12)) # 6 different source_id and 12*10 years (total: 72)
count_all <- 1
for(i in 1:6){
  dcorr_all.array[,,count_all:(count_all+11)] <- dcorr.list[[i]]
  dcorr_netrad.array[,,count_all:(count_all+11)] <- dcorr_netrad.list[[i]]
  av_tas.array[,,count_all:(count_all+11)] <- av_tas.list[[i]]
  av_mrso.array[,,count_all:(count_all+11)] <- av_mrso.list[[i]]
  av_hfls.array[,,count_all:(count_all+11)] <- av_hfls.list[[i]]
  av_lai.array[,,count_all:(count_all+11)] <- av_lai.list[[i]]
  av_rlds.array[,,count_all:(count_all+11)] <- av_rlds.list[[i]]
  av_rlus.array[,,count_all:(count_all+11)] <- av_rlus.list[[i]]
  av_rsds.array[,,count_all:(count_all+11)] <- av_rsds.list[[i]]
  av_rsus.array[,,count_all:(count_all+11)] <- av_rsus.list[[i]]
  av_pr.array[,,count_all:(count_all+11)] <- av_pr.list[[i]]
  av_cropFrac.array[,,count_all:(count_all+11)] <- av_cropFrac.list[[i]]
  av_treeFrac.array[,,count_all:(count_all+11)] <- av_treeFrac.list[[i]]
  count_all <- count_all + 12
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- dcorr_all.array[91:180,,]; test[91:180,,] <- dcorr_all.array[1:90,,]; dcorr_all.array <- test; dcorr_all.array <- -1*dcorr_all.array
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- dcorr_netrad.array[91:180,,]; test[91:180,,] <- dcorr_netrad.array[1:90,,]; dcorr_netrad.array <- test; dcorr_netrad.array <- -1*dcorr_netrad.array
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_tas.array[91:180,,]; test[91:180,,] <- av_tas.array[1:90,,]; av_tas.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_mrso.array[91:180,,]; test[91:180,,] <- av_mrso.array[1:90,,]; av_mrso.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_hfls.array[91:180,,]; test[91:180,,] <- av_hfls.array[1:90,,]; av_hfls.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_rlds.array[91:180,,]; test[91:180,,] <- av_rlds.array[1:90,,]; av_rlds.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_rlus.array[91:180,,]; test[91:180,,] <- av_rlus.array[1:90,,]; av_rlus.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_rsds.array[91:180,,]; test[91:180,,] <- av_rsds.array[1:90,,]; av_rsds.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_rsus.array[91:180,,]; test[91:180,,] <- av_rsus.array[1:90,,]; av_rsus.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_pr.array[91:180,,]; test[91:180,,] <- av_pr.array[1:90,,]; av_pr.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_cropFrac.array[91:180,,]; test[91:180,,] <- av_cropFrac.array[1:90,,]; av_cropFrac.array <- test
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- av_treeFrac.array[91:180,,]; test[91:180,,] <- av_treeFrac.array[1:90,,]; av_treeFrac.array <- test
av_rnet.array <- ((av_rlds.array - av_rlus.array) + (av_rsds.array - av_rsus.array))/26.15741 # in mm
# rho <- 1000 #kg/m3
# l_v <- 2.26 #MJ/kg
# # W/m2 = 1000 (kg/m3) * 2.26*10^6 (J/kg) * 1 mm/day*(1/(24*60^2))(day/s) * (1/1000)(mm/m)

# conversion factor 1mm = 28.35648 Wm-2
av_ar.array <- av_rnet.array/(av_pr.array*24*60*60)

# dredge with crop/treeFrac as explanatory variables
# maybe now per surface area instead of % of grid cells?
r <- raster()  # by default 1 by 1 degree
res(r) <- 2 # so change the resolution
a <- raster::area(r) # calculate the area of a 2x2 degree grid from N - S, as area varies only by latitude, not longitude
area <- a[,1]
area.array <- array(NaN,c(180,90))
for(x in 1:180){
  area.array[x,] <- area
}

# Now define spatial correlations between local trends
# 2) Calculate the local trend per model.
# calculate trends in i) dcorr and ii) T/SM/LE for all 10yr blocks
kendall_dcorr <-
  kendall_dcorr_netrad <-
  array(NaN,c(180,90,6,2)) # 7: models, 2: slope (all) & p.value (all)
count_i <- 1
for(i in seq(1,(6*12),12)){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(dcorr_all.array[x,y,(i:(i+11))])) == 12){ # only calculate if the model has full time series
        kendall_dcorr[x,y,count_i,] <- c(unname(kendallTrendTest(dcorr_all.array[x,y,(i:(i+11))])$estimate[2]), unname(kendallTrendTest(dcorr_all.array[x,y,(i:(i+11))])$p.value))
        kendall_dcorr_netrad[x,y,count_i,] <- c(unname(kendallTrendTest(dcorr_netrad.array[x,y,(i:(i+11))])$estimate[2]), unname(kendallTrendTest(dcorr_netrad.array[x,y,(i:(i+11))])$p.value))
      }
    }
  }
  print(i)
  count_i <- count_i + 1
}

mmmean_dcorr_per_10yr <- mmmean_dcorr_netrad_per_10yr <- mmmean_tas_per_10yr <- 
  mmmean_mrso_per_10yr <- mmmean_hfls_per_10yr <- 
  mmmean_lai_per_10yr <- mmmean_ar_per_10yr <- 
  mmmean_cropFrac_per_10yr <- mmmean_treeFrac_per_10yr <- 
  array(NaN,c(180,90,12))
index_per_10yr <- seq(0,71,12)
for(i in 1:12){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(models_with_full_timeseries[x,y])){
        if(models_with_full_timeseries[x,y] > 4){
          mmmean_dcorr_per_10yr[x,y,i] <- mean(dcorr_all.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_dcorr_netrad_per_10yr[x,y,i] <- mean(dcorr_netrad.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_tas_per_10yr[x,y,i] <- mean(av_tas.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_mrso_per_10yr[x,y,i] <- mean(av_mrso.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_hfls_per_10yr[x,y,i] <- mean(av_hfls.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_lai_per_10yr[x,y,i] <- mean(av_lai.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_ar_per_10yr[x,y,i] <- mean(av_ar.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_cropFrac_per_10yr[x,y,i] <- mean(av_cropFrac.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_treeFrac_per_10yr[x,y,i] <- mean(av_treeFrac.array[x,y,(index_per_10yr+i)],na.rm=T)
        }
      }
    }
  }
}

mmmean_kendall_dcorr <-mmmean_kendall_dcorr_netrad <- mmmean_kendall_tas <- mmmean_kendall_hfls <-
  mmmean_kendall_mrso <- mmmean_kendall_lai <- mmmean_kendall_ar <- array(NaN,c(180,90,2)) # 2: slope (all) & p.value (all)
sum_blocks <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_per_10yr[x,y,])) == 12){
      sum_blocks[x,y] <- sum(!is.na(mmmean_dcorr_per_10yr[x,y,]))
      mmmean_kendall_dcorr[x,y,] <- c(unname(kendallTrendTest(mmmean_dcorr_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_dcorr_per_10yr[x,y,])$p.value))
      mmmean_kendall_dcorr_netrad[x,y,] <- c(unname(kendallTrendTest(mmmean_dcorr_netrad_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_dcorr_netrad_per_10yr[x,y,])$p.value))
      mmmean_kendall_tas[x,y,] <- c(unname(kendallTrendTest(mmmean_tas_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_tas_per_10yr[x,y,])$p.value))
      mmmean_kendall_mrso[x,y,] <- c(unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$p.value))
      mmmean_kendall_hfls[x,y,] <- c(unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$p.value))
      mmmean_kendall_lai[x,y,] <- c(unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$p.value))
      mmmean_kendall_ar[x,y,] <- c(unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$p.value))
    }
  }
}

mmmean_av_treeFrac <- 
  array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(models_with_full_timeseries[x,y])){
      if(models_with_full_timeseries[x,y] > 4){
        mmmean_av_treeFrac[x,y] <- mean(av_treeFrac.array[x,y,], na.rm = T)
      }
    }
  }
}

intervals <- seq(0,100,20)
mask_treeFrac <- array(NaN,c(180,90,5))
for(x in 1:180){
  for(y in 1:90){
    for(i in 1:5){
      if(!is.na(mmmean_av_treeFrac[x,y]) & mmmean_av_treeFrac[x,y] > intervals[i] & mmmean_av_treeFrac[x,y] < intervals[i+1]){
        mask_treeFrac[x,y,i] <- 1
      }
    }
  }
}

q_classes <- c("<20", "20-40", "40-60", "60-80",">80")
dcorr_treeFrac.df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                              c("mmmean_kendall_dcorr","label"))
for(i in 1:5){
  dcorr_treeFrac.df <- rbind(dcorr_treeFrac.df, 
                             data.frame("mmmean_kendall_dcorr" = c(weighted.mean(x = mask_treeFrac[,,i]*mmmean_kendall_dcorr[,,1], w = area.array*mask_treeFrac[,,i], na.rm = T)),
                                        "mmmean_kendall_dcorr_netrad" = c(weighted.mean(x = mask_treeFrac[,,i]*mmmean_kendall_dcorr_netrad[,,1], w = area.array*mask_treeFrac[,,i], na.rm = T)),
                                        "label" = q_classes[i]))
}
dcorr_treeFrac.df$label <- factor(dcorr_treeFrac.df$label, levels = q_classes)

cols_trnd_dcorr <- rev(brewer.pal(11,"RdYlBu")[2:8])


bar_treeFractrends_rev <- ggplot(dcorr_treeFrac.df, aes(y = label, x = mmmean_kendall_dcorr, fill = label)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "dodge") +
  scale_y_discrete("Tree fraction (%)") +
  scale_x_continuous("Ecosystem Limitation\nIndex trend (-/10yr)", expand = c(0,0), breaks = seq(0,.02,.01)) +
  scale_fill_manual(values = c(cols_trnd_dcorr[3],cols_trnd_dcorr[3], cols_trnd_dcorr[4],cols_trnd_dcorr[4],cols_trnd_dcorr[5]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  ) + ggtitle("*")
bar_treeFractrends_rev
bar_treeFractrends_rev <- ggplotGrob(bar_treeFractrends_rev)

bar_treeFractrends_netrad_rev <- ggplot(dcorr_treeFrac.df, aes(y = label, x = mmmean_kendall_dcorr_netrad, fill = label)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "dodge") +
  scale_y_discrete("Tree fraction (%)") +
  scale_x_continuous("Ecosystem Limitation\nIndex trend (-/10yr)", expand = c(0,0)) +
  scale_fill_manual(values = c(cols_trnd_dcorr[3],cols_trnd_dcorr[3], cols_trnd_dcorr[4],cols_trnd_dcorr[4],cols_trnd_dcorr[5]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  ) + ggtitle("*")
bar_treeFractrends_netrad_rev
bar_treeFractrends_netrad_rev <- ggplotGrob(bar_treeFractrends_netrad_rev)

##########################################################################################
##########################################################################################
################################## barplot treeFrac (end) ################################
##########################################################################################
##########################################################################################


# from /RData
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
load('RData/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
load('RData/total_land_area.RData')
load('RData/202106_cmip6_frac.RData')
# # from /testdir
# load('testdir/202106_cmip6_no_evspsblveg_hurs.RData')
# load('testdir/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs.RData')
# load('testdir/total_land_area.RData')
# load("testdir/202106_cmip6_frac.RData")


lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# select the right models and put in array
dcorr_all.array <- 
  corr_rgy_veg_all.array <-
  corr_wtr_veg_all.array <-
  dcorr_netrad.array <- 
  corr_wtr_veg_netrad.array <- 
  corr_rgy_veg_netrad.array <- 
  av_tas.array <- 
  av_mrso.array <- 
  av_lai.array <- 
  av_hfls.array <- 
  av_rlds.array <- 
  av_rlus.array <- 
  av_rsds.array <- 
  av_rsus.array <- 
  av_pr.array <- array(NaN,c(180,90,11*12)) # 7 different source_id and 12*10 years (total: 84)
count_all <- 1
for(i in 1:11){
  dcorr_all.array[,,count_all:(count_all+11)] <- dcorr.list[[i]]
  corr_rgy_veg_all.array[,,count_all:(count_all+11)] <- corr_rgy_veg.list[[i]]
  corr_wtr_veg_all.array[,,count_all:(count_all+11)] <- corr_wtr_veg.list[[i]]
  dcorr_netrad.array[,,count_all:(count_all+11)] <- dcorr_netrad.list[[i]]
  corr_rgy_veg_netrad.array[,,count_all:(count_all+11)] <- corr_rgy_veg_netrad.list[[i]]
  corr_wtr_veg_netrad.array[,,count_all:(count_all+11)] <- corr_wtr_veg_netrad.list[[i]]
  av_tas.array[,,count_all:(count_all+11)] <- av_tas.list[[i]]
  av_mrso.array[,,count_all:(count_all+11)] <- av_mrso.list[[i]]
  av_hfls.array[,,count_all:(count_all+11)] <- av_hfls.list[[i]]
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
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dcorr_netrad.array[91:180,,]; test[91:180,,] <- dcorr_netrad.array[1:90,,]; dcorr_netrad.array <- test; dcorr_netrad.array <- -1*dcorr_netrad.array
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- corr_rgy_veg_netrad.array[91:180,,]; test[91:180,,] <- corr_rgy_veg_netrad.array[1:90,,]; corr_rgy_veg_netrad.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- corr_wtr_veg_netrad.array[91:180,,]; test[91:180,,] <- corr_wtr_veg_netrad.array[1:90,,]; corr_wtr_veg_netrad.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_tas.array[91:180,,]; test[91:180,,] <- av_tas.array[1:90,,]; av_tas.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_mrso.array[91:180,,]; test[91:180,,] <- av_mrso.array[1:90,,]; av_mrso.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_hfls.array[91:180,,]; test[91:180,,] <- av_hfls.array[1:90,,]; av_hfls.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rlds.array[91:180,,]; test[91:180,,] <- av_rlds.array[1:90,,]; av_rlds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rlus.array[91:180,,]; test[91:180,,] <- av_rlus.array[1:90,,]; av_rlus.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rsds.array[91:180,,]; test[91:180,,] <- av_rsds.array[1:90,,]; av_rsds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rsus.array[91:180,,]; test[91:180,,] <- av_rsus.array[1:90,,]; av_rsus.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_pr.array[91:180,,]; test[91:180,,] <- av_pr.array[1:90,,]; av_pr.array <- test
av_rnet.array <- ((av_rlds.array - av_rlus.array) + (av_rsds.array - av_rsus.array))/26.15741 # in mm
# rho <- 1000 #kg/m3
# l_v <- 2.26 #MJ/kg
# # W/m2 = 1000 (kg/m3) * 2.26*10^6 (J/kg) * 1 mm/day*(1/(24*60^2))(day/s) * (1/1000)(mm/m)

av_hfls.array <- av_hfls.array/26.15741

# conversion factor 1mm = 28.35648 Wm-2
av_ar.array <- av_rnet.array/(av_pr.array*24*60*60)





wtr_lim_both_pos <- rgy_lim_both_pos <- wtr_pos_rgy_neg <- wtr_neg_rgy_pos <- c()
for(i in 1:132){
  wtr_pos_rgy_neg[i] <- sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] > 0 & corr_wtr_veg_all.array[,,i] > 0 & corr_rgy_veg_netrad.array[,,i] < 0)], na.rm = T)/sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] > 0)], na.rm = T)
  wtr_lim_both_pos[i] <- sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] > 0 & corr_wtr_veg_all.array[,,i] > 0 & corr_rgy_veg_netrad.array[,,i] > 0)], na.rm = T)/sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] > 0)], na.rm = T)
  wtr_neg_rgy_pos[i] <- sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] < 0 & corr_wtr_veg_all.array[,,i] < 0 & corr_rgy_veg_netrad.array[,,i] > 0)], na.rm = T)/sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] < 0)], na.rm = T)
  rgy_lim_both_pos[i] <- sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] < 0 & corr_wtr_veg_all.array[,,i] > 0 & corr_rgy_veg_netrad.array[,,i] > 0)], na.rm = T)/sum(area.array[which(!is.na(dcorr_netrad.array[,,i]) & dcorr_netrad.array[,,i] < 0)], na.rm = T)
}

median(wtr_pos_rgy_neg)
sd(wtr_pos_rgy_neg)
median(wtr_lim_both_pos)
sd(wtr_lim_both_pos)
median(wtr_neg_rgy_pos)
sd(wtr_neg_rgy_pos)
median(rgy_lim_both_pos)
sd(rgy_lim_both_pos)

image.plot(dcorr_netrad.array[,,1])



# Now define spatial correlations between local trends
# 2) Calculate the local trend per model.
# calculate trends in i) dcorr and ii) T/SM/LE for all 10yr blocks
kendall_dcorr <-
  kendall_dcorr_netrad <-
  array(NaN,c(180,90,11,2)) # 11: models, 2: slope (all) & p.value (all)
count_i <- 1
for(i in seq(1,(11*12),12)){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(dcorr_all.array[x,y,(i:(i+11))])) == 12){ # only calculate if the model has full time series
        kendall_dcorr[x,y,count_i,] <- c(unname(kendallTrendTest(dcorr_all.array[x,y,(i:(i+11))])$estimate[2]), unname(kendallTrendTest(dcorr_all.array[x,y,(i:(i+11))])$p.value))
        kendall_dcorr_netrad[x,y,count_i,] <- c(unname(kendallTrendTest(dcorr_netrad.array[x,y,(i:(i+11))])$estimate[2]), unname(kendallTrendTest(dcorr_netrad.array[x,y,(i:(i+11))])$p.value))
      }
    }
  }
  print(i)
  count_i <- count_i + 1
}

ensemble_mean_dcorr_all <- 
  ensemble_mean_dcorr_netrad <- 
  count_warm <- # a 
  array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(models_with_full_timeseries[x,y])){
      if(models_with_full_timeseries[x,y] > 4){
        ensemble_mean_dcorr_all[x,y] <- mean(dcorr_all.array[x,y,], na.rm = T)
        ensemble_mean_dcorr_netrad[x,y] <- mean(dcorr_netrad.array[x,y,], na.rm = T)
      }
    }
    count_warm[x,y] <- length(which(!is.na(dcorr_all.array[x,y,])))
  }
}

count <- 1
ensemble_mean_dcorr_all_vec <- 
  ensemble_mean_dcorr_netrad_vec <- 
  count_warm_vec <-
  area_vec <-
  NaN
for(x in 1:180){
  for(y in 1:90){
    ensemble_mean_dcorr_all_vec[count] <- ensemble_mean_dcorr_all[x,y]
    ensemble_mean_dcorr_netrad_vec[count] <- ensemble_mean_dcorr_netrad[x,y]
    count_warm_vec[count] <- count_warm[x,y]
    area_vec[count] <- area.array[x,y]
    count <- count + 1
  }
  print(paste(round(count/(180*90)*100,2),"%"))
}

# make the data.frames
dcorr.df <- data.frame("ensemble_mean_dcorr_all" = ensemble_mean_dcorr_all_vec,
                       "ensemble_mean_dcorr_netrad" = ensemble_mean_dcorr_netrad_vec,
                       "count_warm" = count_warm_vec,
                       "area" = area_vec,
                       "lon" = rep(lon, each = 90),
                       "lat" = rep(lat, 180))
dcorr.df$count_warm[which(dcorr.df$count_warm == 0)] <- NaN


dcorrcol <- rev(brewer.pal(9,"BrBG"))

myvalues_ensemble_mean_dcorr <- c(-Inf,-.66,-.44,-.22,-.0001,.0001,.22,.44,.66,Inf)
dcorr.df$cuts_ensemble_mean_dcorr_all <- cut(dcorr.df$ensemble_mean_dcorr_all, myvalues_ensemble_mean_dcorr, include.lowest = T)
dcorr.df$cuts_ensemble_mean_dcorr_netrad <- cut(dcorr.df$ensemble_mean_dcorr_netrad, myvalues_ensemble_mean_dcorr, include.lowest = T)

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

# define hot spot regions
hotspot_regs.df <- data.frame("x" = c(lon[52], lon[62], lon[52], lon[52], lon[30], lon[58], lon[30], lon[30], lon[92], lon[100], lon[92], lon[92], lon[103], lon[138], lon[103], lon[103], lon[138], lon[152], lon[138], lon[138]),
                              "y" = c(lat[38], lat[38], lat[38], lat[48], lat[65], lat[65], lat[65], lat[77], lat[67], lat[67], lat[67], lat[72], lat[72], lat[72], lat[72], lat[77], lat[63], lat[63], lat[63], lat[57]),
                              "xend" = c(lon[52], lon[62], lon[62], lon[62], lon[30], lon[58], lon[58], lon[58], lon[92], lon[100], lon[100], lon[100], lon[103], lon[138], lon[138], lon[138], lon[138], lon[152], lon[152], lon[152]),
                              "yend" = c(lat[48], lat[48], lat[38], lat[48], lat[77], lat[77], lat[65], lat[77], lat[72], lat[72], lat[67], lat[72], lat[77], lat[77], lat[72], lat[77], lat[57], lat[57], lat[63], lat[57]))

mmmean_dcorr_per_10yr <- mmmean_corr_rgy_veg_per_10yr <- mmmean_corr_wtr_veg_per_10yr <- mmmean_tas_per_10yr <- mmmean_netrad_per_10yr <- 
  mmmean_dcorr_netrad_per_10yr <- mmmean_corr_rgy_veg_netrad_per_10yr <- 
  mmmean_mrso_per_10yr <- mmmean_hfls_per_10yr <- 
  mmmean_lai_per_10yr <- mmmean_ar_per_10yr <- 
  array(NaN,c(180,90,12))
index_per_10yr <- seq(0,131,12)
for(i in 1:12){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(models_with_full_timeseries[x,y])){
        if(models_with_full_timeseries[x,y] > 4){
          mmmean_dcorr_per_10yr[x,y,i] <- mean(dcorr_all.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_corr_rgy_veg_per_10yr[x,y,i] <- mean(corr_rgy_veg_all.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_dcorr_netrad_per_10yr[x,y,i] <- mean(dcorr_netrad.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_corr_rgy_veg_netrad_per_10yr[x,y,i] <- mean(corr_rgy_veg_netrad.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_corr_wtr_veg_per_10yr[x,y,i] <- mean(corr_wtr_veg_all.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_tas_per_10yr[x,y,i] <- mean(av_tas.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_netrad_per_10yr[x,y,i] <- mean(av_rnet.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_mrso_per_10yr[x,y,i] <- mean(av_mrso.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_hfls_per_10yr[x,y,i] <- mean(av_hfls.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_lai_per_10yr[x,y,i] <- mean(av_lai.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_ar_per_10yr[x,y,i] <- mean(av_ar.array[x,y,(index_per_10yr+i)],na.rm=T)
        }
      }
    }
  }
}

area_per_10yr.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                             c("year","area","area_netrad","region"))
year_id <- seq(1980,2090,10)
for(i in 1:12){
  area_per_10yr.df <- rbind(area_per_10yr.df,
                            data.frame("year" = year_id[i],
                                       "area" = 100*c(sum(area.array[which(mmmean_dcorr_per_10yr[,,i] > 0)])/total_land_area),
                                       "area_netrad" = 100*c(sum(area.array[which(mmmean_dcorr_netrad_per_10yr[,,i] > 0)])/total_land_area),
                                       "reg" = "Global"))
}

reg <- c("SAM","NAM", "CEU", "NEA", "EAS")
count_j <- 1
for(j in seq(1,20,4)){
  lonmin <- min(hotspot_regs.df$x[j:(j+3)])
  lonmax <- max(hotspot_regs.df$x[j:(j+3)])
  latmin <- min(hotspot_regs.df$y[j:(j+3)])
  latmax <- max(hotspot_regs.df$y[j:(j+3)])
  total_land_area_reg <- sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                        which(lat >= latmin & lat <= latmax)][which(models_with_full_timeseries[which(lon >= lonmin & lon <= lonmax),
                                                                                                                which(lat >= latmin & lat <= latmax)] > 4)])
  for(i in 1:12){
    area_per_10yr.df <- rbind(area_per_10yr.df,
                              data.frame("year" = year_id[i],
                                         "area" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                      which(lat >= latmin & lat <= latmax)][which(mmmean_dcorr_per_10yr[,,i][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                             which(lat >= latmin & lat <= latmax)] > 0)])/total_land_area_reg),
                                         
                                         "area_netrad" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                             which(lat >= latmin & lat <= latmax)][which(mmmean_dcorr_netrad_per_10yr[,,i][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                           which(lat >= latmin & lat <= latmax)] > 0)])/total_land_area_reg),
                                         "reg" = reg[count_j]))
  }
  count_j <- count_j + 1
}

# the mean dcorr hist plot
a <- ggplot(dcorr.df[which(!is.na(dcorr.df$ensemble_mean_dcorr_all)),], aes(x=lon,y=lat,fill=cuts_ensemble_mean_dcorr_all)) + 
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = c(dcorrcol[2:4],dcorrcol[6],dcorrcol[7:9])) + # watch how many unique col classes there are in dcorr.df$cuts
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("c)")
a

point_mmmean_dcorr_per_10yr <- ggplot(area_per_10yr.df[which(area_per_10yr.df$reg == 'Global'),], aes(x = year, y = area)) +
  geom_line(col = dcorrcol[9], size = 1) + 
  scale_x_continuous("",
                     breaks = year_id[seq(1,11,2)], expand = c(0,0)) +
  scale_y_continuous(expression(paste("water-limited land area-%")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
point_mmmean_dcorr_per_10yr
point_mmmean_dcorr_per_10yr <- ggplotGrob(point_mmmean_dcorr_per_10yr)


# plot the discrete colorbar for mean dcorr
dbar <- plot_discrete_cbar(breaks = c(myvalues_ensemble_mean_dcorr[1:4],0,myvalues_ensemble_mean_dcorr[7:10]),
                           colors = c(dcorrcol[1:4],dcorrcol[6:9]),
                           legend_title = expression("mean Ecosystem Limitation Index (with T"[a]*")"),
                           spacing = "constant",
                           font_size = 6,
                           spacing_scaling = 2,
                           width = .2,
                           triangle_size = .175)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(a,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_dcorr <- plot

col_reg <- c("Global" = "black",
             "SAM" = brewer.pal(8,"Dark2")[1],
             "NAM" = brewer.pal(8,"Dark2")[2],
             "CEU" = brewer.pal(8,"Dark2")[3],
             "NEA" = brewer.pal(8,"Dark2")[4],
             "EAS" = brewer.pal(8,"Dark2")[5])

point_mmmean_dcorr_per_10yr_reg <- ggplot(area_per_10yr.df, aes(x = year, y = area_netrad, col = reg)) +
  geom_line(size = 1) +
  scale_x_continuous("",
                     breaks = year_id[seq(1,11,2)], expand = c(0,0)) +
  scale_y_continuous(expression(paste("water-limited land area-%")), expand = c(0,0)) +
  scale_color_manual("region",
                     values = col_reg) +
  theme(legend.position = "right",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key = element_rect(colour = NA, fill = NA),
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
        axis.title.y.right = element_text(color = "grey80"))
point_mmmean_dcorr_per_10yr_reg


ggsave("testdir/SFig12.png", plot = point_mmmean_dcorr_per_10yr_reg, width = 11*1.25, height = 5*1.25, units = "in")

area_per_10yr.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                             c("year","count","area","evap_regime"))
year_id <- seq(1980,2090,10)
for(i in 1:12){
  area_per_10yr.df <- rbind(area_per_10yr.df,
                            data.frame("year" = rep(year_id[i],2),
                                       "count" = c(length(which(mmmean_dcorr_netrad_per_10yr[,,i] > 0)), length(which(mmmean_dcorr_netrad_per_10yr[,,i] < 0))),
                                       "area" = 100*c(sum(area.array[which(mmmean_dcorr_netrad_per_10yr[,,i] > 0)])/total_land_area, sum(area.array[which(mmmean_dcorr_netrad_per_10yr[,,i] < 0)])/total_land_area),
                                       "evap_regime" = c("water-lim.","energy-lim.")))
}
a <- ggplot(dcorr.df[which(!is.na(dcorr.df$ensemble_mean_dcorr_netrad)),], aes(x=lon,y=lat,fill=cuts_ensemble_mean_dcorr_netrad)) + 
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = c(dcorrcol)) + # watch how many unique col classes there are in dcorr.df$cuts
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        # text = element_text(size = 7),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("b)")
a

point_mmmean_dcorr_netrad_per_10yr <- ggplot(area_per_10yr.df[which(area_per_10yr.df$evap_regime == 'water-lim.'),], aes(x = year, y = area)) +
  geom_line(col = dcorrcol[9], size = 1) + 
  scale_x_continuous("",
                     breaks = year_id[seq(1,11,2)], expand = c(0,0)) +
  scale_y_continuous(expression(paste("water-limited land area-%")), expand = c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
point_mmmean_dcorr_netrad_per_10yr
point_mmmean_dcorr_netrad_per_10yr <- ggplotGrob(point_mmmean_dcorr_netrad_per_10yr)

# plot the discrete colorbar for mean dcorr
dbar <- plot_discrete_cbar(breaks = c(myvalues_ensemble_mean_dcorr[1:4],0,myvalues_ensemble_mean_dcorr[7:10]),
                           colors = c(dcorrcol[1:4],dcorrcol[6:9]),
                           legend_title = expression("mean Ecosystem Limitation Index"),
                           spacing = "constant",
                           font_size = 6,
                           spacing_scaling = 2,
                           width = .2,
                           triangle_size = .175)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(a,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_dcorr_netrad <- plot

# plot standard deviation between individual slopes
sd_kendall_dcorr_netrad <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(models_with_full_timeseries[x,y] > 4){
      sd_kendall_dcorr_netrad[x,y] <- sd(kendall_dcorr_netrad[x,y,,1], na.rm = T)
    }
  }
}

mmmean_kendall_dcorr <- mmmean_kendall_corr_rgy_veg <- mmmean_kendall_corr_wtr_veg <- 
  mmmean_kendall_dcorr_netrad <- mmmean_kendall_corr_rgy_veg_netrad <-
  mmmean_kendall_tas <- mmmean_kendall_netrad <- mmmean_kendall_hfls <-
  mmmean_kendall_mrso <- mmmean_kendall_lai <- mmmean_kendall_ar <- 
  array(NaN,c(180,90,2)) # 2: slope (all) & p.value (all)
sum_blocks <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_netrad_per_10yr[x,y,])) == 12){ # full time series
      sum_blocks[x,y] <- sum(!is.na(mmmean_dcorr_per_10yr[x,y,]))
      mmmean_kendall_dcorr[x,y,] <- c(unname(kendallTrendTest(mmmean_dcorr_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_dcorr_per_10yr[x,y,])$p.value))
      mmmean_kendall_corr_rgy_veg[x,y,] <- c(unname(kendallTrendTest(mmmean_corr_rgy_veg_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_corr_rgy_veg_per_10yr[x,y,])$p.value))
      mmmean_kendall_corr_wtr_veg[x,y,] <- c(unname(kendallTrendTest(mmmean_corr_wtr_veg_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_corr_wtr_veg_per_10yr[x,y,])$p.value))
      mmmean_kendall_dcorr_netrad[x,y,] <- c(unname(kendallTrendTest(mmmean_dcorr_netrad_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_dcorr_netrad_per_10yr[x,y,])$p.value))
      mmmean_kendall_corr_rgy_veg_netrad[x,y,] <- c(unname(kendallTrendTest(mmmean_corr_rgy_veg_netrad_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_corr_rgy_veg_netrad_per_10yr[x,y,])$p.value))
      mmmean_kendall_tas[x,y,] <- c(unname(kendallTrendTest(mmmean_tas_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_tas_per_10yr[x,y,])$p.value))
      mmmean_kendall_netrad[x,y,] <- c(unname(kendallTrendTest(mmmean_netrad_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_netrad_per_10yr[x,y,])$p.value))
      mmmean_kendall_mrso[x,y,] <- c(unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$p.value))
      mmmean_kendall_hfls[x,y,] <- c(unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$p.value))
      mmmean_kendall_lai[x,y,] <- c(unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$p.value))
      mmmean_kendall_ar[x,y,] <- c(unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$p.value))
    }
  }
}

sum(area.array[which(mmmean_kendall_dcorr[,,1] > 0)])/total_land_area # increasing water limitation
sum(area.array[which(mmmean_kendall_dcorr_netrad[,,1] > 0)])/total_land_area # increasing water limitation with net radiation
# sum(area.array[which(mmmean_kendall_dcorr[,,1] > 0 & mmmean_dcorr_per_10yr[,,1] > 0 | 
#                        mmmean_kendall_dcorr[,,1] < 0 & mmmean_dcorr_per_10yr[,,1] < 0)])/total_land_area # DDWW
# sum(area.array[which(mmmean_kendall_dcorr[,,1] > 0 & mmmean_dcorr_per_10yr[,,1] > 0 | 
#                        mmmean_kendall_dcorr[,,1] < 0 & mmmean_dcorr_per_10yr[,,1] < 0 &
#                        mmmean_kendall_dcorr[,,2] < 0.05)])/total_land_area # significant DDWW

mmmean_kendall_dcorr_vec <- 
  mmmean_sign_kendall_dcorr_vec <- 
  mmmean_kendall_corr_rgy_veg_vec <- 
  mmmean_sign_kendall_corr_rgy_veg_vec <- 
  mmmean_kendall_dcorr_netrad_vec <- 
  mmmean_sign_kendall_dcorr_netrad_vec <- 
  mmmean_kendall_corr_rgy_veg_netrad_vec <- 
  mmmean_sign_kendall_corr_rgy_veg_netrad_vec <- 
  mmmean_kendall_corr_wtr_veg_vec <- 
  mmmean_sign_kendall_corr_wtr_veg_vec <- 
  mmmean_kendall_tas_vec <- 
  mmmean_sign_kendall_tas_vec <- 
  mmmean_kendall_netrad_vec <- 
  mmmean_sign_kendall_netrad_vec <- 
  mmmean_kendall_mrso_vec <- 
  mmmean_sign_kendall_mrso_vec <- 
  mmmean_kendall_hfls_vec <- 
  mmmean_sign_kendall_hfls_vec <- 
  mmmean_kendall_lai_vec <- 
  mmmean_sign_kendall_lai_vec <- 
  mmmean_kendall_ar_vec <- 
  mmmean_sign_kendall_ar_vec <- 
  sd_kendall_dcorr_netrad_vec <- 
c()
count <- 1
for(x in 1:180){
  for(y in 1:90){
    mmmean_kendall_dcorr_vec[count] <- mmmean_kendall_dcorr[x,y,1]
    mmmean_sign_kendall_dcorr_vec[count] <- mmmean_kendall_dcorr[x,y,2]
    mmmean_kendall_corr_rgy_veg_vec[count] <- mmmean_kendall_corr_rgy_veg[x,y,1]
    mmmean_sign_kendall_corr_rgy_veg_vec[count] <- mmmean_kendall_corr_rgy_veg[x,y,2]
    mmmean_kendall_dcorr_netrad_vec[count] <- mmmean_kendall_dcorr_netrad[x,y,1]
    mmmean_sign_kendall_dcorr_netrad_vec[count] <- mmmean_kendall_dcorr_netrad[x,y,2]
    mmmean_kendall_corr_rgy_veg_netrad_vec[count] <- mmmean_kendall_corr_rgy_veg_netrad[x,y,1]
    mmmean_sign_kendall_corr_rgy_veg_netrad_vec[count] <- mmmean_kendall_corr_rgy_veg_netrad[x,y,2]
    mmmean_kendall_corr_wtr_veg_vec[count] <- mmmean_kendall_corr_wtr_veg[x,y,1]
    mmmean_sign_kendall_corr_wtr_veg_vec[count] <- mmmean_kendall_corr_wtr_veg[x,y,2]
    mmmean_kendall_tas_vec[count] <- mmmean_kendall_tas[x,y,1]
    mmmean_sign_kendall_tas_vec[count] <- mmmean_kendall_tas[x,y,2]
    mmmean_kendall_netrad_vec[count] <- mmmean_kendall_netrad[x,y,1]
    mmmean_sign_kendall_netrad_vec[count] <- mmmean_kendall_netrad[x,y,2]
    mmmean_kendall_mrso_vec[count] <- mmmean_kendall_mrso[x,y,1]
    mmmean_sign_kendall_mrso_vec[count] <- mmmean_kendall_mrso[x,y,2]
    mmmean_kendall_hfls_vec[count] <- mmmean_kendall_hfls[x,y,1]
    mmmean_sign_kendall_hfls_vec[count] <- mmmean_kendall_hfls[x,y,2]
    mmmean_kendall_lai_vec[count] <- mmmean_kendall_lai[x,y,1]
    mmmean_sign_kendall_lai_vec[count] <- mmmean_kendall_lai[x,y,2]
    mmmean_kendall_ar_vec[count] <- mmmean_kendall_ar[x,y,1]
    mmmean_sign_kendall_ar_vec[count] <- mmmean_kendall_ar[x,y,2]
    sd_kendall_dcorr_netrad_vec[count] <- sd_kendall_dcorr_netrad[x,y]
    count <- count + 1
  }
}

mmtrends.df <- data.frame('trend_dcorr' = c(mmmean_kendall_dcorr_vec),
                          'trend_corr_rgy_veg' = c(mmmean_kendall_corr_rgy_veg_vec),
                          'trend_dcorr_netrad' = c(mmmean_kendall_dcorr_netrad_vec),
                          'trend_corr_rgy_veg_netrad' = c(mmmean_kendall_corr_rgy_veg_netrad_vec),
                          'trend_corr_wtr_veg' = c(mmmean_kendall_corr_wtr_veg_vec),
                          'trend_tas' = c(mmmean_kendall_tas_vec),
                          'trend_netrad' = c(mmmean_kendall_netrad_vec),
                          'trend_mrso' = c(mmmean_kendall_mrso_vec),
                          'trend_hfls' = c(mmmean_kendall_hfls_vec),
                          'trend_lai' = c(mmmean_kendall_lai_vec),
                          'trend_ar' = c(mmmean_kendall_ar_vec),
                          'sd_kendall_dcorr_netrad' = c(sd_kendall_dcorr_netrad_vec),
                          "lon" = rep(lon, each = 90),
                          "lat" = rep(lat, 180))
sign_dcorr.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_dcorr_vec < .05)],
                            "lat" = rep(lat, 180)[which(mmmean_sign_kendall_dcorr_vec < .05)])
sign_corr_rgy_veg.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_corr_rgy_veg_vec < .05)],
                            "lat" = rep(lat, 180)[which(mmmean_sign_kendall_corr_rgy_veg_vec < .05)])
sign_dcorr_netrad.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_dcorr_netrad_vec < .05)],
                                   "lat" = rep(lat, 180)[which(mmmean_sign_kendall_dcorr_netrad_vec < .05)])
sign_corr_rgy_veg_netrad.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_corr_rgy_veg_netrad_vec < .05)],
                                          "lat" = rep(lat, 180)[which(mmmean_sign_kendall_corr_rgy_veg_netrad_vec < .05)])
sign_corr_wtr_veg.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_corr_wtr_veg_vec < .05)],
                            "lat" = rep(lat, 180)[which(mmmean_sign_kendall_corr_wtr_veg_vec < .05)])
sign_tas.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_tas_vec < .05)],
                          "lat" = rep(lat, 180)[which(mmmean_sign_kendall_tas_vec < .05)])
sign_netrad.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_netrad_vec < .05)],
                          "lat" = rep(lat, 180)[which(mmmean_sign_kendall_netrad_vec < .05)])
sign_mrso.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_mrso_vec < .05)],
                           "lat" = rep(lat, 180)[which(mmmean_sign_kendall_mrso_vec < .05)])
sign_hfls.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_hfls_vec < .05)],
                           "lat" = rep(lat, 180)[which(mmmean_sign_kendall_hfls_vec < .05)])
sign_lai.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_lai_vec < .05)],
                          "lat" = rep(lat, 180)[which(mmmean_sign_kendall_lai_vec < .05)])
sign_ar.df <- data.frame("lon" = rep(lon, each = 90)[which(mmmean_sign_kendall_ar_vec < .05)],
                         "lat" = rep(lat, 180)[which(mmmean_sign_kendall_ar_vec < .05)])


cols_trnd_dcorr <- rev(brewer.pal(11,"RdYlBu")[2:8])
myvalues_trnd_dcorr <- c(-Inf,seq(-.01,.04,.01),Inf)
dbar <- plot_discrete_cbar(myvalues_trnd_dcorr,
                           colors = cols_trnd_dcorr,
                           legend_title = expression("ELI"[T[a]]*" trend (-/10yr)"),
                           spacing = "constant",
                           font_size = 6,
                           spacing_scaling = 2,
                           width = .2,
                           triangle_size = .175)

dbar_dcorr_netrad <- plot_discrete_cbar(myvalues_trnd_dcorr,
                                  colors = cols_trnd_dcorr,
                                  legend_title = "Ecosystem Limitation Index trend (-/10yr)",
                                  spacing = "constant",
                                  font_size = 6,
                                  spacing_scaling = 2,
                                  width = .2,
                                  triangle_size = .175)

cols_trnd_corr_rgy_veg <- c(brewer.pal(11,"RdYlBu")[1:5],brewer.pal(11,"RdYlBu")[7:11])
myvalues_trnd_corr_rgy_veg <- c(-Inf,seq(-.04,.04,.01),Inf)
dbar_corr_rgy_veg <- plot_discrete_cbar(myvalues_trnd_corr_rgy_veg,
                                        colors = cols_trnd_corr_rgy_veg,
                                        legend_title = expression("cor(R"[n]*"',ET') trend (-/10yr)"),
                                        spacing = "constant",
                                        font_size = 6,
                                        spacing_scaling = 2,
                                        width = .2,
                                        triangle_size = .175)


cols_trnd_corr_wtr_veg <- rev(c(brewer.pal(11,"RdYlBu")[1:5],brewer.pal(11,"RdYlBu")[7:11]))
myvalues_trnd_corr_wtr_veg <- c(-Inf,seq(-.04,.04,.01),Inf)
dbar_corr_wtr_veg <- plot_discrete_cbar(myvalues_trnd_corr_wtr_veg,
                           colors = cols_trnd_corr_wtr_veg,
                           legend_title = expression("cor(SM',ET') trend (-/10yr)"),
                           spacing = "constant",
                           font_size = 6,
                           spacing_scaling = 2,
                           width = .2,
                           triangle_size = .175)

cols_trnd_tas <- brewer.pal(9,"YlOrRd")[3:9]
myvalues_trnd_tas <- c(-Inf,seq(.2,.7,.1),Inf)
dbar_tas <- plot_discrete_cbar(myvalues_trnd_tas,
                               colors = cols_trnd_tas,
                               legend_title = expression(paste("Temperature trend (K/10yr)")),
                               spacing = "constant",
                               font_size = 6,
                               spacing_scaling = 2,
                               width = .2,
                               triangle_size = .175)

cols_trnd_netrad <- rev(brewer.pal(9,"RdYlBu")[1:7])
myvalues_trnd_netrad <- c(-Inf,seq(-.02,.08,.02),Inf)
dbar_netrad <- plot_discrete_cbar(myvalues_trnd_netrad,
                                  colors = cols_trnd_netrad,
                                  legend_title = expression(paste("Surface net radiation trend (mm/10yr)")),
                                  spacing = "constant",
                                  font_size = 6,
                                  spacing_scaling = 2,
                                  width = .2,
                                  triangle_size = .175)

cols_trnd_mrso <- brewer.pal(9,"RdYlBu")[3:8]
myvalues_trnd_mrso <- c(-Inf,seq(-5,5,2.5),Inf)
dbar_mrso <- plot_discrete_cbar(myvalues_trnd_mrso,
                                colors = cols_trnd_mrso,
                                legend_title = expression(paste("Soil moisture trend (mm/10yr)")),
                                spacing = "constant",
                                font_size = 6,
                                spacing_scaling = 2,
                                width = .2,
                                triangle_size = .175)

cols_trnd_hfls <- c(brewer.pal(9,"PiYG")[2:4],brewer.pal(11,"PiYG")[7:11])
myvalues_trnd_hfls <- c(-Inf,seq(-.04,.08,.02),Inf)
dbar_hfls <- plot_discrete_cbar(myvalues_trnd_hfls,
                                colors = cols_trnd_hfls,
                                legend_title = expression(paste("Terrestrial evaporation trend (mm/d/10yr)")),
                                spacing = "constant",
                                font_size = 6,
                                spacing_scaling = 2,
                                width = .2,
                                triangle_size = .175)

cols_trnd_lai <- c(brewer.pal(11,"RdYlBu")[6],brewer.pal(9,"Greens")[5:9])
myvalues_trnd_lai <- c(-Inf,seq(0,.08,.02),Inf)
dbar_lai <- plot_discrete_cbar(myvalues_trnd_lai,
                               colors = cols_trnd_lai,
                               legend_title = expression(paste("Leaf area trend (-/10yr)")),
                               spacing = "constant",
                               font_size = 6,
                               spacing_scaling = 2,
                               width = .2,
                               triangle_size = .175)

cols_trnd_ar <- rev(c(brewer.pal(11,"RdYlBu")[5:10]))
myvalues_trnd_ar <- c(-Inf,round(seq(-.3,.1,.1),2),Inf)
dbar_ar <- plot_discrete_cbar(myvalues_trnd_ar,
                              colors = cols_trnd_ar,
                              legend_title = expression(paste("Aridity Index trend (-/10yr)")),
                              spacing = "constant",
                              font_size = 6,
                              spacing_scaling = 2,
                              width = .2,
                              triangle_size = .175)

cols_trnd_sd_dcorr <- c(brewer.pal(6,"Purples"))
myvalues_trnd_sd_dcorr <- c(-Inf,seq(.01,.03,.005),Inf)
dbar_sd_dcorr <- plot_discrete_cbar(myvalues_trnd_sd_dcorr,
                                    colors = cols_trnd_sd_dcorr,
                                    legend_title = expression(paste(sigma[ELI]," between individual slopes")),
                                    spacing = "constant",
                                    font_size = 6,
                                    spacing_scaling = 2,
                                    width = .2,
                                    triangle_size = .175)

cols_trnd_agreement <- c('red','orange','yellow','olivedrab1','limegreen')
myvalues_trnd_agreement <- c(seq(-1,1,.2)*100)[6:11]
dbar_agreement <- plot_discrete_cbar(c(myvalues_trnd_agreement),
                                    colors = cols_trnd_agreement, 
                                    legend_title = expression("Agreement on multi-model mean ELI trend sign (%)"),
                                    spacing = "constant",
                                    font_size = 6,
                                    spacing_scaling = 2,
                                    width = .2,
                                    triangle_size = .175)

mmtrends.df$cuts_trend_dcorr <- cut(mmtrends.df$trend_dcorr, myvalues_trnd_dcorr, include.lowest = T)
mmtrends.df$cuts_trend_corr_rgy_veg <- cut(mmtrends.df$trend_corr_rgy_veg, myvalues_trnd_corr_rgy_veg, include.lowest = T)
mmtrends.df$cuts_trend_dcorr_netrad <- cut(mmtrends.df$trend_dcorr_netrad, myvalues_trnd_dcorr, include.lowest = T)
mmtrends.df$cuts_trend_corr_rgy_veg_netrad <- cut(mmtrends.df$trend_corr_rgy_veg_netrad, myvalues_trnd_corr_rgy_veg, include.lowest = T)
mmtrends.df$cuts_trend_corr_wtr_veg <- cut(mmtrends.df$trend_corr_wtr_veg, myvalues_trnd_corr_wtr_veg, include.lowest = T)
mmtrends.df$cuts_trend_tas <- cut(mmtrends.df$trend_tas, myvalues_trnd_tas, include.lowest = T)
mmtrends.df$cuts_trend_netrad <- cut(mmtrends.df$trend_netrad, myvalues_trnd_netrad, include.lowest = T)
mmtrends.df$cuts_trend_mrso <- cut(mmtrends.df$trend_mrso, myvalues_trnd_mrso, include.lowest = T)
mmtrends.df$cuts_trend_hfls <- cut(mmtrends.df$trend_hfls, myvalues_trnd_hfls, include.lowest = T)
mmtrends.df$cuts_trend_lai <- cut(mmtrends.df$trend_lai, myvalues_trnd_lai, include.lowest = T)
mmtrends.df$cuts_trend_ar <- cut(mmtrends.df$trend_ar, myvalues_trnd_ar, include.lowest = T)
mmtrends.df$cuts_sd_kendall_dcorr_netrad <- cut(mmtrends.df$sd_kendall_dcorr_netrad, myvalues_trnd_sd_dcorr, include.lowest = T)

count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_dcorr[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_dcorr[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_dcorr[,,1] > 0 & mmmean_kendall_dcorr[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_dcorr[,,1] < 0 & mmmean_kendall_dcorr[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_dcorr[,,1] > 0 & mmmean_kendall_dcorr[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_dcorr[,,1] < 0 & mmmean_kendall_dcorr[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("drying","wettening"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)

names.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                     c("lon","lat","label"))
jump_four <- seq(1,20,4)
regs <- c("SAM","NAM","CEU","NEA","EAS")
for(i in 1:5){
  names.df <- rbind(names.df, data.frame("lon" = (min(hotspot_regs.df$x[jump_four[i]:(jump_four[i]+3)]) + max(hotspot_regs.df$x[jump_four[i]:(jump_four[i]+3)]))/2,
                                         "lat" = max(hotspot_regs.df$y[jump_four[i]:(jump_four[i]+3)]),
                                         "label" = regs[i]))
}

f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_dcorr)),], aes(x=lon,y=lat,fill=cuts_trend_dcorr)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_dcorr.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_text(inherit.aes = F, data = names.df, aes(x=lon,y=lat+3.5,label=label), col = 1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("a)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign =='all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), position = "right") +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_treeFractrends_rev, t = 30, l = 3, b = 19, r = 10)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 21, l = 3, b = 13, r = 8)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_dcorr <- plot

count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_dcorr_netrad[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_dcorr_netrad[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_dcorr_netrad[,,1] > 0 & mmmean_kendall_dcorr_netrad[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_dcorr_netrad[,,1] < 0 & mmmean_kendall_dcorr_netrad[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_dcorr_netrad[,,1] > 0 & mmmean_kendall_dcorr_netrad[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_dcorr_netrad[,,1] < 0 & mmmean_kendall_dcorr_netrad[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("drying","wettening"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)

f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_dcorr_netrad)),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_dcorr_netrad.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_text(inherit.aes = F, data = names.df, aes(x=lon,y=lat+3.5,label=label), size = 5, col = 1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        # text = element_text(size=7),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("a)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign =='all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), position = "right") +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)


gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_treeFractrends_netrad_rev, t = 30, l = 3, b = 19, r = 10)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 21, l = 3, b = 13, r = 8)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_dcorr_netrad <- dbar_dcorr_netrad + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_dcorr_netrad, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_dcorr_netrad <- plot










# the mean tas plot

count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_tas[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_tas[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_tas[,,1] > 0 & mmmean_kendall_tas[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_tas[,,1] < 0 & mmmean_kendall_tas[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_tas[,,1] > 0 & mmmean_kendall_tas[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_tas[,,1] < 0 & mmmean_kendall_tas[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("warming","cooling"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)

f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_tas)),], aes(x=lon,y=lat,fill=cuts_trend_tas)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_tas.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_tas,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("a)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("warming_sign" = cols_trnd_tas[7],
                               "warming_insign" = alpha(cols_trnd_tas[7], .25),
                               "cooling_insign" = alpha(cols_trnd_tas[1], .25),
                               "cooling_sign" = cols_trnd_tas[1]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_tas <- dbar_tas + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_tas, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_tas <- plot

# the mean netrad plot

count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_netrad[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_netrad[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_netrad[,,1] > 0 & mmmean_kendall_netrad[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_netrad[,,1] < 0 & mmmean_kendall_netrad[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_netrad[,,1] > 0 & mmmean_kendall_netrad[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_netrad[,,1] < 0 & mmmean_kendall_netrad[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("warming","cooling"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)

f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_netrad)),], aes(x=lon,y=lat,fill=cuts_trend_netrad)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_netrad.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_netrad,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("a)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("warming_sign" = cols_trnd_netrad[7],
                               "warming_insign" = alpha(cols_trnd_netrad[7], .25),
                               "cooling_insign" = alpha(cols_trnd_netrad[1], .25),
                               "cooling_sign" = cols_trnd_netrad[1]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_netrad <- dbar_netrad + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_netrad, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_netrad <- plot

# the mean mrso plot
count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_mrso[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_mrso[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_mrso[,,1] > 0 & mmmean_kendall_mrso[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_mrso[,,1] < 0 & mmmean_kendall_mrso[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_mrso[,,1] > 0 & mmmean_kendall_mrso[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_mrso[,,1] < 0 & mmmean_kendall_mrso[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("wettening","drying"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)
f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_mrso)),], aes(x=lon,y=lat,fill=cuts_trend_mrso)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_mrso.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_mrso,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("b)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = cols_trnd_mrso[1],
                               "drying_insign" = alpha(cols_trnd_mrso[1], .25),
                               "wettening_insign" = alpha(cols_trnd_mrso[6], .25),
                               "wettening_sign" = cols_trnd_mrso[6]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_mrso <- dbar_mrso + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_mrso, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_mrso <- plot

# the mean hfls plot
count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_hfls[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_hfls[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_hfls[,,1] > 0 & mmmean_kendall_hfls[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_hfls[,,1] < 0 & mmmean_kendall_hfls[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_hfls[,,1] > 0 & mmmean_kendall_hfls[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_hfls[,,1] < 0 & mmmean_kendall_hfls[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("wettening","drying"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)
f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_hfls)),], aes(x=lon,y=lat,fill=cuts_trend_hfls)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_hfls.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_hfls,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("c)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = cols_trnd_hfls[1],
                               "drying_insign" = alpha(cols_trnd_hfls[1], .25),
                               "wettening_insign" = alpha(cols_trnd_hfls[8], .25),
                               "wettening_sign" = cols_trnd_hfls[8]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_hfls <- dbar_hfls + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_hfls, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_hfls <- plot

# the mean lai plot
count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_lai[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_lai[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_lai[,,1] > 0 & mmmean_kendall_lai[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_lai[,,1] < 0 & mmmean_kendall_lai[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_lai[,,1] > 0 & mmmean_kendall_lai[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_lai[,,1] < 0 & mmmean_kendall_lai[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("greening","browning"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)
f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_lai)),], aes(x=lon,y=lat,fill=cuts_trend_lai)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_lai.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_lai,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("d)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("browning_sign" = cols_trnd_lai[1],
                               "browning_insign" = alpha(cols_trnd_lai[1], .25),
                               "greening_insign" = alpha(cols_trnd_lai[6], .25),
                               "greening_sign" = cols_trnd_lai[6]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_lai <- dbar_lai + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_lai, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_lai <- plot

# the mean ar plot
count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_ar[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_ar[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_ar[,,1] > 0 & mmmean_kendall_ar[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_ar[,,1] < 0 & mmmean_kendall_ar[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_ar[,,1] > 0 & mmmean_kendall_ar[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_ar[,,1] < 0 & mmmean_kendall_ar[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("drying","wettening"),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)
f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_ar)),], aes(x=lon,y=lat,fill=cuts_trend_ar)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_ar.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_ar,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("e)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = cols_trnd_ar[1],
                               "drying_insign" = alpha(cols_trnd_ar[1], .25),
                               "wettening_insign" = alpha(cols_trnd_ar[6], .25),
                               "wettening_sign" = cols_trnd_ar[6]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_ar <- dbar_ar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_ar, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_ar <- plot

plots1 <- grid.arrange(plot_mean_kendall_netrad, plot_mean_kendall_mrso, plot_mean_kendall_hfls, ncol = 3, widths = c(1/3,1/3,1/3))
plots2 <- grid.arrange(empty, as_ggplot(plot_mean_kendall_lai), as_ggplot(plot_mean_kendall_ar), empty, ncol = 4,widths = c(1/8,3/8,3/8,1/8))
plots3 <- grid.arrange(grobs = list(plots1,plots2), nrow = 2, heigths = c(5/7,2/7))
ggsave("testdir/SFig11.png", plot = plots3, width = 11*1.9, height = 5.5*1.9, units = "in")

# the mean corr_rgy_veg plot
count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_corr_rgy_veg_netrad[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_corr_rgy_veg_netrad[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_corr_rgy_veg_netrad[,,1] > 0 & mmmean_kendall_corr_rgy_veg_netrad[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_corr_rgy_veg_netrad[,,1] < 0 & mmmean_kendall_corr_rgy_veg_netrad[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_corr_rgy_veg_netrad[,,1] > 0 & mmmean_kendall_corr_rgy_veg_netrad[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_corr_rgy_veg_netrad[,,1] < 0 & mmmean_kendall_corr_rgy_veg_netrad[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("+ energy lim.","- energy lim."),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)

f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_corr_rgy_veg)),], aes(x=lon,y=lat,fill=cuts_trend_corr_rgy_veg)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_corr_rgy_veg.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_corr_rgy_veg,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("b)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("- energy lim._sign" = cols_trnd_corr_rgy_veg[1],
                               "- energy lim._insign" = alpha(cols_trnd_corr_rgy_veg[1], .25),
                               "+ energy lim._insign" = alpha(cols_trnd_corr_rgy_veg[10], .25),
                               "+ energy lim._sign" = cols_trnd_corr_rgy_veg[10]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_corr_rgy_veg <- dbar_corr_rgy_veg + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_corr_rgy_veg, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_corr_rgy_veg <- plot

# the mean corr_wtr_veg plot
count_mmtrends.df <- data.frame("area" = c(100*c(sum(area.array[which(mmmean_kendall_corr_wtr_veg[,,1] > 0)])/total_land_area, sum(area.array[which(mmmean_kendall_corr_wtr_veg[,,1] < 0)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_corr_wtr_veg[,,1] > 0 & mmmean_kendall_corr_wtr_veg[,,2] < .05)])/total_land_area, sum(area.array[which(mmmean_kendall_corr_wtr_veg[,,1] < 0 & mmmean_kendall_corr_wtr_veg[,,2] < .05)])/total_land_area),
                                           100*c(sum(area.array[which(mmmean_kendall_corr_wtr_veg[,,1] > 0 & mmmean_kendall_corr_wtr_veg[,,2] > .05)])/total_land_area, sum(area.array[which(mmmean_kendall_corr_wtr_veg[,,1] < 0 & mmmean_kendall_corr_wtr_veg[,,2] > .05)])/total_land_area)),
                                "trend" = rep(c("+ water lim.","- water lim."),3),
                                "sign" = rep(c("all","sign","insign"),each=2))
count_mmtrends.df$trend_sign <- paste0(count_mmtrends.df$trend, "_",count_mmtrends.df$sign)

f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_corr_wtr_veg)),], aes(x=lon,y=lat,fill=cuts_trend_corr_wtr_veg)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_corr_wtr_veg.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_corr_wtr_veg,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("a)")
f
f <- ggplotGrob(f)

bar_mmtrends <- ggplot(count_mmtrends.df[which(count_mmtrends.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  scale_x_discrete("") +
  geom_text(inherit.aes = F, data = count_mmtrends.df[which(count_mmtrends.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 2.5) + 
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("- water lim._sign" = cols_trnd_corr_wtr_veg[1],
                               "- water lim._insign" = alpha(cols_trnd_corr_wtr_veg[1], .25),
                               "+ water lim._insign" = alpha(cols_trnd_corr_wtr_veg[10], .25),
                               "+ water lim._sign" = cols_trnd_corr_wtr_veg[10]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mmtrends
bar_mmtrends <- ggplotGrob(bar_mmtrends)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mmtrends, t = 31, l = 4, b = 18, r = 10)
grid.draw(gt1)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_corr_wtr_veg <- dbar_corr_wtr_veg + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_corr_wtr_veg, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt1,dbar_smaller, nrow = 2, heights = c(.8,.2))
plot_mean_kendall_corr_wtr_veg <- plot

plots1 <- grid.arrange(plot_mean_kendall_corr_wtr_veg, plot_mean_kendall_corr_rgy_veg, nrow = 2, heights = c(1/2,1/2))
ggsave("testdir/SFig10.png", plot = plots1, width = 9*1.2, height = 7*2*1.2, units = "in")


# the mean sd_kendall_dcorr_netrad plot
f <- ggplot(mmtrends.df[which(!is.na(mmtrends.df$trend_dcorr_netrad)),], aes(x=lon,y=lat,fill=cuts_sd_kendall_dcorr_netrad)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_sd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + 
  ggtitle("b)")
f

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_sd_dcorr <- dbar_sd_dcorr + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_sd_dcorr, empty , ncol=3, widths = c(1,4,1))

plot1 <- grid.arrange(f,dbar_smaller, nrow = 2, heights = c(.8,.2))

# agreement on sign of ELI
# this is the agreement between models and across times
agreement.array <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(models_with_full_timeseries[x,y])){
      if(models_with_full_timeseries[x,y] > 4){
        agreement.array[x,y] <- length(which(sign(kendall_dcorr_netrad[x,y,,1]) == sign(mmmean_kendall_dcorr_netrad[x,y,1]))) / sum(!is.na(kendall_dcorr_netrad[x,y,,1]))*100
      }
    }
  }
}
agreement.array[which(agreement.array == 0)] <- NaN

agreement.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                         c("agreement","lon","lat"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(agreement.array[x,y])){
      agreement.df <- rbind(agreement.df,
                            data.frame("agreement" = agreement.array[x,y],
                                       "lon" = lon[x],
                                       "lat" = lat[y]))
    }
  }
}
agreement.df$cuts_agreement <- cut(agreement.df$agreement, myvalues_trnd_agreement, include.lowest = T)

# the mean sd_kendall_dcorr plot
f <- ggplot(agreement.df, aes(x=lon,y=lat,fill=cuts_agreement)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_agreement,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + 
  ggtitle("a)")
f

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_agreement <- dbar_agreement + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_agreement, empty , ncol=3, widths = c(1,4,1))

plot2 <- grid.arrange(f,dbar_smaller, nrow = 2, heights = c(.8,.2))

plots <- grid.arrange(plot2,plot1,nrow=2,heights=c(1,1))

ggsave("testdir/SFig9.png", plot = plots, width = 9, height = 14, units = "in")

# make spatial plots per model for the dcorr_netrad trend
kendall_dcorr_netrad_per_model.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                       c("trend_dcorr_netrad","lon","lat","source_id"))
for(i in 1:11){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(kendall_dcorr_netrad[x,y,i,1])){
        kendall_dcorr_netrad_per_model.df <- rbind(kendall_dcorr_netrad_per_model.df,
                                            data.frame("trend_dcorr_netrad" = kendall_dcorr_netrad[x,y,i,1],
                                                       "p" = kendall_dcorr_netrad[x,y,i,2],
                                                       "lon" = lon[x],
                                                       "lat" = lat[y],
                                                       "source_id" = cmip6_data.df$source_id[i]))
      }
    }
  }
}
kendall_dcorr_netrad_per_model.df$cuts_trend_dcorr_netrad <- cut(kendall_dcorr_netrad_per_model.df$trend_dcorr_netrad, myvalues_trnd_dcorr, include.lowest = T)

sign_kendall_dcorr_netrad_per_model.df <- kendall_dcorr_netrad_per_model.df[,c(3,4,5)][which(kendall_dcorr_netrad_per_model.df$p < .05),]

# calculate total_land_area_sourceid
total_land_area_sourceid <- c()
for(i in 1:11){
  total_land_area_sourceid[i] <- sum(area.array[which(!is.na(kendall_dcorr_netrad[,,i,1]))])
}



# the dcorr_netrad trend per source_id
count_kendall_dcorr_netrad_per_model.df <- data.frame("area" = c(100*c(sum(area.array[which(kendall_dcorr_netrad[,,1,1] > 0)])/total_land_area_sourceid[1], sum(area.array[which(kendall_dcorr_netrad[,,1,1] < 0)])/total_land_area_sourceid[1],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,1,1] > 0 & kendall_dcorr_netrad[,,1,2] < .05)])/total_land_area_sourceid[1], sum(area.array[which(kendall_dcorr_netrad[,,1,1] < 0 & kendall_dcorr_netrad[,,1,2] < .05)])/total_land_area_sourceid[1],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,1,1] > 0 & kendall_dcorr_netrad[,,1,2] > .05)])/total_land_area_sourceid[1], sum(area.array[which(kendall_dcorr_netrad[,,1,1] < 0 & kendall_dcorr_netrad[,,1,2] > .05)])/total_land_area_sourceid[1]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,2,1] > 0)])/total_land_area_sourceid[2], sum(area.array[which(kendall_dcorr_netrad[,,2,1] < 0)])/total_land_area_sourceid[2],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,2,1] > 0 & kendall_dcorr_netrad[,,2,2] < .05)])/total_land_area_sourceid[2], sum(area.array[which(kendall_dcorr_netrad[,,2,1] < 0 & kendall_dcorr_netrad[,,2,2] < .05)])/total_land_area_sourceid[2],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,2,1] > 0 & kendall_dcorr_netrad[,,2,2] > .05)])/total_land_area_sourceid[2], sum(area.array[which(kendall_dcorr_netrad[,,2,1] < 0 & kendall_dcorr_netrad[,,2,2] > .05)])/total_land_area_sourceid[2]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,3,1] > 0)])/total_land_area_sourceid[3], sum(area.array[which(kendall_dcorr_netrad[,,3,1] < 0)])/total_land_area_sourceid[3],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,3,1] > 0 & kendall_dcorr_netrad[,,3,2] < .05)])/total_land_area_sourceid[3], sum(area.array[which(kendall_dcorr_netrad[,,3,1] < 0 & kendall_dcorr_netrad[,,3,2] < .05)])/total_land_area_sourceid[3],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,3,1] > 0 & kendall_dcorr_netrad[,,3,2] > .05)])/total_land_area_sourceid[3], sum(area.array[which(kendall_dcorr_netrad[,,3,1] < 0 & kendall_dcorr_netrad[,,3,2] > .05)])/total_land_area_sourceid[3]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,4,1] > 0)])/total_land_area_sourceid[4], sum(area.array[which(kendall_dcorr_netrad[,,4,1] < 0)])/total_land_area_sourceid[4],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,4,1] > 0 & kendall_dcorr_netrad[,,4,2] < .05)])/total_land_area_sourceid[4], sum(area.array[which(kendall_dcorr_netrad[,,4,1] < 0 & kendall_dcorr_netrad[,,4,2] < .05)])/total_land_area_sourceid[4],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,4,1] > 0 & kendall_dcorr_netrad[,,4,2] > .05)])/total_land_area_sourceid[4], sum(area.array[which(kendall_dcorr_netrad[,,4,1] < 0 & kendall_dcorr_netrad[,,4,2] > .05)])/total_land_area_sourceid[4]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,5,1] > 0)])/total_land_area_sourceid[5], sum(area.array[which(kendall_dcorr_netrad[,,5,1] < 0)])/total_land_area_sourceid[5],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,5,1] > 0 & kendall_dcorr_netrad[,,5,2] < .05)])/total_land_area_sourceid[5], sum(area.array[which(kendall_dcorr_netrad[,,5,1] < 0 & kendall_dcorr_netrad[,,5,2] < .05)])/total_land_area_sourceid[5],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,5,1] > 0 & kendall_dcorr_netrad[,,5,2] > .05)])/total_land_area_sourceid[5], sum(area.array[which(kendall_dcorr_netrad[,,5,1] < 0 & kendall_dcorr_netrad[,,5,2] > .05)])/total_land_area_sourceid[5]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,6,1] > 0)])/total_land_area_sourceid[6], sum(area.array[which(kendall_dcorr_netrad[,,6,1] < 0)])/total_land_area_sourceid[6],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,6,1] > 0 & kendall_dcorr_netrad[,,6,2] < .05)])/total_land_area_sourceid[6], sum(area.array[which(kendall_dcorr_netrad[,,6,1] < 0 & kendall_dcorr_netrad[,,6,2] < .05)])/total_land_area_sourceid[6],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,6,1] > 0 & kendall_dcorr_netrad[,,6,2] > .05)])/total_land_area_sourceid[6], sum(area.array[which(kendall_dcorr_netrad[,,6,1] < 0 & kendall_dcorr_netrad[,,6,2] > .05)])/total_land_area_sourceid[6]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,7,1] > 0)])/total_land_area_sourceid[7], sum(area.array[which(kendall_dcorr_netrad[,,7,1] < 0)])/total_land_area_sourceid[7],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,7,1] > 0 & kendall_dcorr_netrad[,,7,2] < .05)])/total_land_area_sourceid[7], sum(area.array[which(kendall_dcorr_netrad[,,7,1] < 0 & kendall_dcorr_netrad[,,7,2] < .05)])/total_land_area_sourceid[7],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,7,1] > 0 & kendall_dcorr_netrad[,,7,2] > .05)])/total_land_area_sourceid[7], sum(area.array[which(kendall_dcorr_netrad[,,7,1] < 0 & kendall_dcorr_netrad[,,7,2] > .05)])/total_land_area_sourceid[7]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,8,1] > 0)])/total_land_area_sourceid[8], sum(area.array[which(kendall_dcorr_netrad[,,8,1] < 0)])/total_land_area_sourceid[8],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,8,1] > 0 & kendall_dcorr_netrad[,,8,2] < .05)])/total_land_area_sourceid[8], sum(area.array[which(kendall_dcorr_netrad[,,8,1] < 0 & kendall_dcorr_netrad[,,8,2] < .05)])/total_land_area_sourceid[8],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,8,1] > 0 & kendall_dcorr_netrad[,,8,2] > .05)])/total_land_area_sourceid[8], sum(area.array[which(kendall_dcorr_netrad[,,8,1] < 0 & kendall_dcorr_netrad[,,8,2] > .05)])/total_land_area_sourceid[8]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,9,1] > 0)])/total_land_area_sourceid[9], sum(area.array[which(kendall_dcorr_netrad[,,9,1] < 0)])/total_land_area_sourceid[9],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,9,1] > 0 & kendall_dcorr_netrad[,,9,2] < .05)])/total_land_area_sourceid[9], sum(area.array[which(kendall_dcorr_netrad[,,9,1] < 0 & kendall_dcorr_netrad[,,9,2] < .05)])/total_land_area_sourceid[9],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,9,1] > 0 & kendall_dcorr_netrad[,,9,2] > .05)])/total_land_area_sourceid[9], sum(area.array[which(kendall_dcorr_netrad[,,9,1] < 0 & kendall_dcorr_netrad[,,9,2] > .05)])/total_land_area_sourceid[9]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,10,1] > 0)])/total_land_area_sourceid[10], sum(area.array[which(kendall_dcorr_netrad[,,10,1] < 0)])/total_land_area_sourceid[10],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,10,1] > 0 & kendall_dcorr_netrad[,,10,2] < .05)])/total_land_area_sourceid[10], sum(area.array[which(kendall_dcorr_netrad[,,10,1] < 0 & kendall_dcorr_netrad[,,10,2] < .05)])/total_land_area_sourceid[10],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,10,1] > 0 & kendall_dcorr_netrad[,,10,2] > .05)])/total_land_area_sourceid[10], sum(area.array[which(kendall_dcorr_netrad[,,10,1] < 0 & kendall_dcorr_netrad[,,10,2] > .05)])/total_land_area_sourceid[10]),
                                                          100*c(sum(area.array[which(kendall_dcorr_netrad[,,11,1] > 0)])/total_land_area_sourceid[11], sum(area.array[which(kendall_dcorr_netrad[,,11,1] < 0)])/total_land_area_sourceid[11],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,11,1] > 0 & kendall_dcorr_netrad[,,11,2] < .05)])/total_land_area_sourceid[11], sum(area.array[which(kendall_dcorr_netrad[,,11,1] < 0 & kendall_dcorr_netrad[,,11,2] < .05)])/total_land_area_sourceid[11],
                                                                sum(area.array[which(kendall_dcorr_netrad[,,11,1] > 0 & kendall_dcorr_netrad[,,11,2] > .05)])/total_land_area_sourceid[11], sum(area.array[which(kendall_dcorr_netrad[,,11,1] < 0 & kendall_dcorr_netrad[,,11,2] > .05)])/total_land_area_sourceid[11])),
                                               "trend" = rep(rep(c("drying","wettening"),3), 11),
                                               "sign" = rep(rep(c("all","sign","insign"),each=2),11),
                                               "source_id" = rep(cmip6_data.df$source_id, each = 6))
count_kendall_dcorr_netrad_per_model.df$trend_sign <- paste0(count_kendall_dcorr_netrad_per_model.df$trend, "_",count_kendall_dcorr_netrad_per_model.df$sign)

bar_kendall_dcorr_netrad_per_model1 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[1] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[1] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model1
bar_kendall_dcorr_netrad_per_model1 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model1)

f1 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[1]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[1]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[1])
f1
f1 <- ggplotGrob(f1)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, f1, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_kendall_dcorr_netrad_per_model1, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt1)


bar_kendall_dcorr_netrad_per_model2 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[2] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[2] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model2
bar_kendall_dcorr_netrad_per_model2 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model2)

f2 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[2]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[2]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[2])
f2
f2 <- ggplotGrob(f2)

gt2 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt2 <- gtable_add_grob(gt2, f2, t=1, b=32, l=1, r=32)
gt2 <- gtable_add_grob(gt2, bar_kendall_dcorr_netrad_per_model2, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt2)

bar_kendall_dcorr_netrad_per_model3 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[3] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[3] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model3
bar_kendall_dcorr_netrad_per_model3 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model3)

f3 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[3]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[3]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[3])
f3
f3 <- ggplotGrob(f3)

gt3 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt3 <- gtable_add_grob(gt3, f3, t=1, b=32, l=1, r=32)
gt3 <- gtable_add_grob(gt3, bar_kendall_dcorr_netrad_per_model3, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt3)

bar_kendall_dcorr_netrad_per_model4 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[4] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[4] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model4
bar_kendall_dcorr_netrad_per_model4 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model4)

f4 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[4]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[4]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[4])
f4
f4 <- ggplotGrob(f4)

gt4 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt4 <- gtable_add_grob(gt4, f4, t=1, b=32, l=1, r=32)
gt4 <- gtable_add_grob(gt4, bar_kendall_dcorr_netrad_per_model4, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt4)

bar_kendall_dcorr_netrad_per_model5 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[5] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[5] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model5
bar_kendall_dcorr_netrad_per_model5 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model5)

f5 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[5]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[5]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[5])
f5
f5 <- ggplotGrob(f5)

gt5 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt5 <- gtable_add_grob(gt5, f5, t=1, b=32, l=1, r=32)
gt5 <- gtable_add_grob(gt5, bar_kendall_dcorr_netrad_per_model5, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt5)

bar_kendall_dcorr_netrad_per_model6 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[6] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[6] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model6
bar_kendall_dcorr_netrad_per_model6 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model6)

f6 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[6]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[6]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[6])
f6
f6 <- ggplotGrob(f6)

gt6 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt6 <- gtable_add_grob(gt6, f6, t=1, b=32, l=1, r=32)
gt6 <- gtable_add_grob(gt6, bar_kendall_dcorr_netrad_per_model6, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt6)

bar_kendall_dcorr_netrad_per_model7 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[7] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[7] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model7
bar_kendall_dcorr_netrad_per_model7 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model7)

f7 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[7]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[7]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[7])
f7
f7 <- ggplotGrob(f7)

gt7 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt7 <- gtable_add_grob(gt7, f7, t=1, b=32, l=1, r=32)
gt7 <- gtable_add_grob(gt7, bar_kendall_dcorr_netrad_per_model7, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt7)

bar_kendall_dcorr_netrad_per_model8 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[8] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[8] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model8
bar_kendall_dcorr_netrad_per_model8 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model8)

f8 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[8]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[8]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[8])
f8
f8 <- ggplotGrob(f8)

gt8 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt8 <- gtable_add_grob(gt8, f8, t=1, b=32, l=1, r=32)
gt8 <- gtable_add_grob(gt8, bar_kendall_dcorr_netrad_per_model8, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt8)

bar_kendall_dcorr_netrad_per_model9 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[9] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[9] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model9
bar_kendall_dcorr_netrad_per_model9 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model9)

f9 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[9]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[9]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[9])
f9
f9 <- ggplotGrob(f9)

gt9 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt9 <- gtable_add_grob(gt9, f9, t=1, b=32, l=1, r=32)
gt9 <- gtable_add_grob(gt9, bar_kendall_dcorr_netrad_per_model9, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt9)

bar_kendall_dcorr_netrad_per_model10 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[10] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[10] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model10
bar_kendall_dcorr_netrad_per_model10 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model10)

f10 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[10]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[10]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[10])
f10
f10 <- ggplotGrob(f10)

gt10 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt10 <- gtable_add_grob(gt10, f10, t=1, b=32, l=1, r=32)
gt10 <- gtable_add_grob(gt10, bar_kendall_dcorr_netrad_per_model10, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt10)

bar_kendall_dcorr_netrad_per_model11 <- ggplot(count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[11] & count_kendall_dcorr_netrad_per_model.df$sign != 'all'),], aes(x = trend, y = area, group = trend, fill = trend_sign)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = count_kendall_dcorr_netrad_per_model.df[which(count_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[11] & count_kendall_dcorr_netrad_per_model.df$sign == 'all'),], aes(x=trend,y=area-5,label=round(area,2)), size = 1.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0)) +
  scale_fill_manual(values = c("drying_sign" = brewer.pal(9,"Reds")[7],
                               "drying_insign" = alpha(brewer.pal(9,"Reds")[7], .25),
                               "wettening_insign" = alpha(brewer.pal(9,"Blues")[7], .25),
                               "wettening_sign" = brewer.pal(9,"Blues")[7]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_kendall_dcorr_netrad_per_model11
bar_kendall_dcorr_netrad_per_model11 <- ggplotGrob(bar_kendall_dcorr_netrad_per_model11)

f11 <- ggplot(kendall_dcorr_netrad_per_model.df[which(kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[11]),], aes(x=lon,y=lat,fill=cuts_trend_dcorr_netrad)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_kendall_dcorr_netrad_per_model.df[which(sign_kendall_dcorr_netrad_per_model.df$source_id == cmip6_data.df$source_id[11]),], aes(x=lon,y=lat), size = .005) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual(values = cols_trnd_dcorr,
                    drop = F) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  ) +
  ggtitle(cmip6_data.df$source_id[11])
f11
f11 <- ggplotGrob(f11)

gt11 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt11 <- gtable_add_grob(gt11, f11, t=1, b=32, l=1, r=32)
gt11 <- gtable_add_grob(gt11, bar_kendall_dcorr_netrad_per_model11, t = 31, l = 5, b = 18, r = 11)
grid.draw(gt11)

plots <- grid.arrange(gt1, gt2, gt3, gt4, gt5, gt6, gt7, gt8, gt9, gt10, gt11, dbar, nrow = 3)


ggsave("testdir/SFig8.png", plot = plots, width = 13/.66/.9, height = 11/.9, units = "in")

trans_zone <- array(NaN,c(180,90,12))
year_id <- seq(1980,2090,10)
#################################################################################################################################################
#####################################################                       #####################################################################
##################################################### TEMPORAL DEFINITION!  #####################################################################
#####################################################                       #####################################################################
#################################################################################################################################################
for(i in 2:12){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(mmmean_dcorr_per_10yr[x,y,((i-1):i)])) == 2){
        if(sign(mmmean_dcorr_per_10yr[x,y,(i-1)]) != sign(mmmean_dcorr_per_10yr[x,y,(i)])){
          trans_zone[x,y,i] <- year_id[i-1]
        }
      }
    }
  }
}

how_many_trans_blocks <- array(NaN,c(180,90))
# just to see how many of the 10 year blocks a grid cell is transitional
for(x in 1:180){
  for(y in 1:90){
    how_many_trans_blocks[x,y] <- sum(!is.na(trans_zone[x,y,]))
  }
}

# I'll define trans_zone as the first year that the transitional zone has been there
first_yr_trans_zone <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(length(which(!is.na(trans_zone[x,y,])) > 0)){
      first_yr_trans_zone[x,y] <- min(trans_zone[x,y,], na.rm=T)
    }
  }
}

# trans_zone_vec <- c()
first_yr_trans_zone.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                   c("trans","lon","lat","year"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(first_yr_trans_zone[x,y])){
      first_yr_trans_zone.df <- rbind(first_yr_trans_zone.df,
                                      data.frame("trans" = first_yr_trans_zone[x,y],
                                                 "lon" = lon[x],
                                                 "lat" = lat[y]))
    }
  }
}

first_yr_trans_zone.df$trans_discrete <- factor(first_yr_trans_zone.df$trans, levels = seq(1980,2090,10))

c <- ggplot(first_yr_trans_zone.df, aes(x=lon,y=lat,fill=trans_discrete)) + 
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_viridis("First decade of transition", option = 'plasma', discrete = T) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, label.position = "bottom", nrow=1, byrow=TRUE,
                             label.theme = element_text(angle = 0))) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        legend.key.width = unit(2, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("b)")
c
c <- ggplotGrob(c)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, c, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, point_mmmean_dcorr_per_10yr, t = 30, l = 4, b = 22, r = 10)
grid.draw(gt1)

cols_trans_zone <- viridis(12, option = "plasma")
myvalues_trans_zone <- seq(1980,2100,10)
dbar_trans_zone <- plot_discrete_cbar(myvalues_trans_zone,
                                      colors = c(cols_trans_zone[1],cols_trans_zone[10:12],cols_trans_zone[2:9]),
                                      legend_title = expression("First decade of transition (ELI"[T[a]]*")"),
                                      spacing = "constant",
                                      font_size = 6,
                                      spacing_scaling = 2,
                                      width = .2,
                                      triangle_size = .175)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_trans_zone <- dbar_trans_zone + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))

plot <- grid.arrange(gt1,dbar_trans_zone, nrow = 2, heights = c(.8,.2))
plot_first_yr_trans_zone <- plot

trans_zone <- array(NaN,c(180,90,12))
year_id <- seq(1980,2090,10)
#################################################################################################################################################
#####################################################                       #####################################################################
##################################################### TEMPORAL DEFINITION!  #####################################################################
#####################################################                       #####################################################################
#################################################################################################################################################
for(i in 2:12){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(mmmean_dcorr_netrad_per_10yr[x,y,((i-1):i)])) == 2){
        if(sign(mmmean_dcorr_netrad_per_10yr[x,y,(i-1)]) != sign(mmmean_dcorr_netrad_per_10yr[x,y,(i)])){
          trans_zone[x,y,i] <- year_id[i-1]
        }
      }
    }
  }
}

how_many_trans_blocks <- array(NaN,c(180,90))
# just to see how many of the 10 year blocks a grid cell is transitional
for(x in 1:180){
  for(y in 1:90){
    how_many_trans_blocks[x,y] <- sum(!is.na(trans_zone[x,y,]))
  }
}

# I'll define trans_zone as the first year that the transitional zone has been there
first_yr_trans_zone <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(length(which(!is.na(trans_zone[x,y,])) > 0)){
      first_yr_trans_zone[x,y] <- min(trans_zone[x,y,], na.rm=T)
    }
  }
}

# trans_zone_vec <- c()
first_yr_trans_zone.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                   c("trans","lon","lat","year"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(first_yr_trans_zone[x,y])){
      first_yr_trans_zone.df <- rbind(first_yr_trans_zone.df,
                                      data.frame("trans" = first_yr_trans_zone[x,y],
                                                 "lon" = lon[x],
                                                 "lat" = lat[y]))
    }
  }
}

first_yr_trans_zone.df$trans_discrete <- factor(first_yr_trans_zone.df$trans, levels = seq(1980,2090,10))

c <- ggplot(first_yr_trans_zone.df, aes(x=lon,y=lat,fill=trans_discrete)) + 
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_viridis("First decade of transition", option = 'plasma', discrete = T) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, label.position = "bottom", nrow=1, byrow=TRUE,
                             label.theme = element_text(angle = 0))) +
  theme(legend.position = "none",
        # text = element_text(size=7),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        legend.key.width = unit(2, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("d)")
c
c <- ggplotGrob(c)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, c, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, point_mmmean_dcorr_netrad_per_10yr, t = 30, l = 4, b = 22, r = 10)
grid.draw(gt1)

cols_trans_zone <- viridis(12, option = "plasma")
myvalues_trans_zone <- seq(1980,2100,10)
dbar_trans_zone <- plot_discrete_cbar(myvalues_trans_zone,
                                      colors = c(cols_trans_zone[1],cols_trans_zone[10:12],cols_trans_zone[2:9]),
                                      legend_title = expression("First decade of transition"),
                                      spacing = "constant",
                                      font_size = 6,
                                      spacing_scaling = 2,
                                      width = .2,
                                      triangle_size = .175)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar_trans_zone <- dbar_trans_zone + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
plot <- grid.arrange(gt1,dbar_trans_zone, nrow = 2, heights = c(.8,.2))
plot_first_yr_trans_zone_netrad <- plot


plots <- grid.arrange(plot_mean_kendall_dcorr, plot_first_yr_trans_zone)

ggsave("testdir/SFig13.png", plot = plots, width = 9, height = 14, units = "in")

plots <- grid.arrange(plot_mean_kendall_dcorr_netrad, plot_mean_dcorr_netrad, plot_mean_dcorr, plot_first_yr_trans_zone_netrad, ncol = 2)

ggsave("testdir/Fig2.eps", plot = plots, width = 18, height = 14, units = "in", device = cairo_ps)
