# Script to make Supplementary Figures 17 and 18
# By: Jasper Denissen
# 2021-12-20

pdf(NULL)

##############################################################################################################################
##############################################################################################################################
######################## !!! Don't forget to reset the working directory to a directory of your choosing!!! ##################
##############################################################################################################################
##############################################################################################################################
setwd('/Net/Groups/BGI/work_3/HydroBioClim/archive/Denissen_etal_2022_Nat_Clim_Change/')

# Get the proper packages
source('Scripts/to_be_loaded_packages.R')
# load functions
source('Scripts/functions/parallel_raster_functions.R')
source('Scripts/functions/align_nine_plots.R')

# from /RData
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
# # from /testdir
# load('testdir/202106_cmip6_no_evspsblveg_hurs.RData')


# defined month of year for a 10 year period
cmip6_path <- "Data/cmip6_202106/"
vars <- c('tas','hfls','mrso')
files <- list.files(cmip6_path,
                    pattern = ".nc$")
moy <- rep(1:12,10)

monthly.df <- setNames(data.frame(matrix(ncol = 18, nrow = 0)),
                       c("raw_tas", "raw_mrso", "raw_hfls", 
                         "detrended_tas", "detrended_mrso", "detrended_hfls", 
                         "moy_tas", "moy_mrso", "moy_hfls", 
                         "anom_tas", "anom_mrso", "anom_hfls", 
                         "anom_wrm_tas", "anom_wrm_mrso", "anom_wrm_hfls", 
                         "source_id","wet_dry","month"))

decennial.df <- setNames(data.frame(matrix(ncol = 15, nrow = 0)),
                         c("trend_tas", "trend_mrso", "trend_hfls", 
                           "av_tas", "av_mrso", "av_hfls",
                           "intercept_tas", "intercept_mrso", "intercept_hfls",
                           "ELI", "corr_tas_hfls", "corr_mrso_hfls",
                           "source_id","wet_dry","month"))

for(i in 1:length(cmip6_data.df$source_id)){
  for(var in vars){
    var_files <- files[which(grepl(cmip6_data.df$source_id[i], files) & grepl(var, files) & grepl("2.0x2.0", files))]
    ncin_hist <- nc_open(paste0(cmip6_path,var_files[1]))
    ncin_scen <- nc_open(paste0(cmip6_path,var_files[2]))
    if(i %in% which(cmip6_data.df$source_id != "CAMS-CSM1-0" & cmip6_data.df$source_id != "TaiESM1")){ # in ACCESS-ESM1-5 we are missing the last year apparently, so we repeat values of 2099
      assign(paste0("raw_",var,"_wet"), c(ncvar_get(ncin_hist, var, start=c(143,70,13),count=c(1,1,408)),ncvar_get(ncin_scen, var, start=c(143,70,1),count=c(1,1,1032)))) # NAM-WET
      assign(paste0("raw_",var,"_dry"), c(ncvar_get(ncin_hist, var, start=c(129,64,13),count=c(1,1,408)),ncvar_get(ncin_scen, var, start=c(129,64,1),count=c(1,1,1032)))) # NAM-DRY
    }else if(cmip6_data.df$source_id[i] == "CAMS-CSM1-0"){
      assign(paste0("raw_",var,"_wet"), c(ncvar_get(ncin_hist, var, start=c(143,70,13),count=c(1,1,408)),ncvar_get(ncin_scen, var, start=c(143,70,1),count=c(1,1,1020)),ncvar_get(ncin_scen, var, start=c(143,70,1008),count=c(1,1,12)))) # NAM-WET
      assign(paste0("raw_",var,"_dry"), c(ncvar_get(ncin_hist, var, start=c(129,64,13),count=c(1,1,408)),ncvar_get(ncin_scen, var, start=c(129,64,1),count=c(1,1,1020)),ncvar_get(ncin_scen, var, start=c(129,64,1008),count=c(1,1,12)))) # NAM-DRY
    }else{
      assign(paste0("raw_",var,"_wet"), c(ncvar_get(ncin_hist, var, start=c(143,70,13),count=c(1,1,408)),ncvar_get(ncin_scen, var, start=c(143,70,1),count=c(1,1,1031)),ncvar_get(ncin_scen, var, start=c(143,70,1031),count=c(1,1,1)))) # NAM-WET
      assign(paste0("raw_",var,"_dry"), c(ncvar_get(ncin_hist, var, start=c(129,64,13),count=c(1,1,408)),ncvar_get(ncin_scen, var, start=c(129,64,1),count=c(1,1,1031)),ncvar_get(ncin_scen, var, start=c(129,64,1031),count=c(1,1,1)))) # NAM-DRY
    }
    nc_close(ncin_hist)
    nc_close(ncin_scen)
  }
  tas_mask_dry <- rep(NaN,1440)
  tas_mask_dry[which(raw_tas_dry > 273.15)] <- 1
  tas_mask_wet <- rep(NaN,1440)
  tas_mask_wet[which(raw_tas_wet > 273.15)] <- 1
  for(t in seq(1,1440,120)){ # so the loop moves 10 years and in the loop we read in 10 years of data.
    # tas dry
    tas_dry_10yr <- raw_tas_dry[t:(t+119)]
    av_tas_dry_10yr <- mean(tas_dry_10yr)
    if(sum(!is.na(tas_dry_10yr)) != 0){
      trend_tas_dry_10yr <- lm(tas_dry_10yr ~ c(1:120))$coef[2] # trend per month
      intercept_tas_dry_10yr <- lm(tas_dry_10yr ~ c(1:120))$coef[1] # trend per month
      detrend_tas_dry_10yr <- tas_dry_10yr - c(1:120)*trend_tas_dry_10yr
      moy_tas_dry_10yr <- c()
      for(m in 1:12){
        moy_tas_dry_10yr[m] <- mean(detrend_tas_dry_10yr[which(moy == m)])
      }
      moy_tas_dry_10yr_rep <- rep(moy_tas_dry_10yr, 10)
      anom_tas_dry_10yr <- detrend_tas_dry_10yr - moy_tas_dry_10yr_rep
      anom_tas_dry_wrm_10yr <- anom_tas_dry_10yr*tas_mask_dry[t:(t+119)]
    }
    # tas wet
    tas_wet_10yr <- raw_tas_wet[t:(t+119)]
    av_tas_wet_10yr <- mean(tas_wet_10yr)
    if(sum(!is.na(tas_wet_10yr)) != 0){
      trend_tas_wet_10yr <- lm(tas_wet_10yr ~ c(1:120))$coef[2] # trend per month
      detrend_tas_wet_10yr <- tas_wet_10yr - c(1:120)*trend_tas_wet_10yr
      intercept_tas_wet_10yr <- lm(tas_wet_10yr ~ c(1:120))$coef[1] # trend per month
      moy_tas_wet_10yr <- c()
      for(m in 1:12){
        moy_tas_wet_10yr[m] <- mean(detrend_tas_wet_10yr[which(moy == m)])
      }
      moy_tas_wet_10yr_rep <- rep(moy_tas_wet_10yr, 10)
      anom_tas_wet_10yr <- detrend_tas_wet_10yr - moy_tas_wet_10yr_rep
      anom_tas_wet_wrm_10yr <- anom_tas_wet_10yr*tas_mask_wet[t:(t+119)]
    }
    
    # mrso dry
    mrso_dry_10yr <- raw_mrso_dry[t:(t+119)]
    av_mrso_dry_10yr <- mean(mrso_dry_10yr)
    if(sum(!is.na(mrso_dry_10yr)) != 0){
      trend_mrso_dry_10yr <- lm(mrso_dry_10yr ~ c(1:120))$coef[2] # trend per month
      detrend_mrso_dry_10yr <- mrso_dry_10yr - c(1:120)*trend_mrso_dry_10yr
      intercept_mrso_dry_10yr <- lm(mrso_dry_10yr ~ c(1:120))$coef[1] # trend per month
      moy_mrso_dry_10yr <- c()
      for(m in 1:12){
        moy_mrso_dry_10yr[m] <- mean(detrend_mrso_dry_10yr[which(moy == m)])
      }
      moy_mrso_dry_10yr_rep <- rep(moy_mrso_dry_10yr, 10)
      anom_mrso_dry_10yr <- detrend_mrso_dry_10yr - moy_mrso_dry_10yr_rep
      anom_mrso_dry_wrm_10yr <- anom_mrso_dry_10yr*tas_mask_dry[t:(t+119)]
    }
    # mrso wet
    mrso_wet_10yr <- raw_mrso_wet[t:(t+119)]
    av_mrso_wet_10yr <- mean(mrso_wet_10yr)
    if(sum(!is.na(mrso_wet_10yr)) != 0){
      trend_mrso_wet_10yr <- lm(mrso_wet_10yr ~ c(1:120))$coef[2] # trend per month
      detrend_mrso_wet_10yr <- mrso_wet_10yr - c(1:120)*trend_mrso_wet_10yr
      intercept_mrso_wet_10yr <- lm(mrso_wet_10yr ~ c(1:120))$coef[1] # trend per month
      moy_mrso_wet_10yr <- c()
      for(m in 1:12){
        moy_mrso_wet_10yr[m] <- mean(detrend_mrso_wet_10yr[which(moy == m)])
      }
      moy_mrso_wet_10yr_rep <- rep(moy_mrso_wet_10yr, 10)
      anom_mrso_wet_10yr <- detrend_mrso_wet_10yr - moy_mrso_wet_10yr_rep
      anom_mrso_wet_wrm_10yr <- anom_mrso_wet_10yr*tas_mask_wet[t:(t+119)]
    }
    
    # hfls dry
    hfls_dry_10yr <- raw_hfls_dry[t:(t+119)]/26.15741 # (mm/d)
    av_hfls_dry_10yr <- mean(hfls_dry_10yr)
    if(sum(!is.na(hfls_dry_10yr)) != 0){
      trend_hfls_dry_10yr <- lm(hfls_dry_10yr ~ c(1:120))$coef[2] # trend per month
      detrend_hfls_dry_10yr <- hfls_dry_10yr - c(1:120)*trend_hfls_dry_10yr
      intercept_hfls_dry_10yr <- lm(hfls_dry_10yr ~ c(1:120))$coef[1] # trend per month
      moy_hfls_dry_10yr <- c()
      for(m in 1:12){
        moy_hfls_dry_10yr[m] <- mean(detrend_hfls_dry_10yr[which(moy == m)])
      }
      moy_hfls_dry_10yr_rep <- rep(moy_hfls_dry_10yr, 10)
      anom_hfls_dry_10yr <- detrend_hfls_dry_10yr - moy_hfls_dry_10yr_rep
      anom_hfls_dry_wrm_10yr <- anom_hfls_dry_10yr*tas_mask_dry[t:(t+119)]
    }
    # hfls wet
    hfls_wet_10yr <- raw_hfls_wet[t:(t+119)]/26.15741 # (mm/d)
    av_hfls_wet_10yr <- mean(hfls_wet_10yr)
    if(sum(!is.na(hfls_wet_10yr)) != 0){
      trend_hfls_wet_10yr <- lm(hfls_wet_10yr ~ c(1:120))$coef[2] # trend per month
      detrend_hfls_wet_10yr <- hfls_wet_10yr - c(1:120)*trend_hfls_wet_10yr
      intercept_hfls_wet_10yr <- lm(hfls_wet_10yr ~ c(1:120))$coef[1] # trend per month
      moy_hfls_wet_10yr <- c()
      for(m in 1:12){
        moy_hfls_wet_10yr[m] <- mean(detrend_hfls_wet_10yr[which(moy == m)])
      }
      moy_hfls_wet_10yr_rep <- rep(moy_hfls_wet_10yr, 10)
      anom_hfls_wet_10yr <- detrend_hfls_wet_10yr - moy_hfls_wet_10yr_rep
      anom_hfls_wet_wrm_10yr <- anom_hfls_wet_10yr*tas_mask_wet[t:(t+119)]
    }
    
    # corr_rgy_veg dry
    if(sum(!is.na(anom_tas_dry_wrm_10yr)) > 30 & sum(!is.na(anom_hfls_dry_wrm_10yr)) > 30){
      corr_tas_hfls_dry <- cor(anom_tas_dry_wrm_10yr, anom_hfls_dry_wrm_10yr, method = 'kendall', use = "pairwise.complete.obs")
    }
    # corr_wtr_veg dry
    if(sum(!is.na(anom_mrso_dry_wrm_10yr)) > 30 & sum(!is.na(anom_hfls_dry_wrm_10yr)) > 30){
      corr_mrso_hfls_dry <- cor(anom_mrso_dry_wrm_10yr, anom_hfls_dry_wrm_10yr, method = 'kendall', use = "pairwise.complete.obs")
    }
    ELI_dry <- corr_mrso_hfls_dry - corr_tas_hfls_dry
    # corr_rgy_veg wet
    if(sum(!is.na(anom_tas_wet_wrm_10yr)) > 30 & sum(!is.na(anom_hfls_wet_wrm_10yr)) > 30){
      corr_tas_hfls_wet <- cor(anom_tas_wet_wrm_10yr, anom_hfls_wet_wrm_10yr, method = 'kendall', use = "pairwise.complete.obs")
    }
    # corr_wtr_veg wet
    if(sum(!is.na(anom_mrso_wet_wrm_10yr)) > 30 & sum(!is.na(anom_hfls_wet_wrm_10yr)) > 30){
      corr_mrso_hfls_wet <- cor(anom_mrso_wet_wrm_10yr, anom_hfls_wet_wrm_10yr, method = 'kendall', use = "pairwise.complete.obs")
    }
    ELI_wet <- corr_mrso_hfls_wet - corr_tas_hfls_wet
    monthly.df <- rbind(monthly.df,
                        data.frame("raw_tas" = c(tas_dry_10yr, tas_wet_10yr),
                                   "raw_mrso" = c(mrso_dry_10yr, mrso_wet_10yr),
                                   "raw_hfls" = c(hfls_dry_10yr, hfls_wet_10yr),
                                   "detrended_tas" = c(detrend_tas_dry_10yr, detrend_tas_wet_10yr),
                                   "detrended_mrso" = c(detrend_mrso_dry_10yr, detrend_mrso_wet_10yr),
                                   "detrended_hfls" = c(detrend_hfls_dry_10yr, detrend_hfls_wet_10yr),
                                   "moy_tas" = c(moy_tas_dry_10yr_rep, moy_tas_wet_10yr_rep),
                                   "moy_mrso" = c(moy_mrso_dry_10yr_rep, moy_mrso_wet_10yr_rep),
                                   "moy_hfls" = c(moy_hfls_dry_10yr_rep, moy_hfls_wet_10yr_rep),
                                   "anom_tas" = c(anom_tas_dry_10yr, anom_tas_wet_10yr),
                                   "anom_mrso" = c(anom_mrso_dry_10yr, anom_mrso_wet_10yr),
                                   "anom_hfls" = c(anom_hfls_dry_10yr, anom_hfls_wet_10yr),
                                   "anom_wrm_tas" = c(anom_tas_dry_wrm_10yr, anom_tas_wet_wrm_10yr),
                                   "anom_wrm_mrso" = c(anom_mrso_dry_wrm_10yr, anom_mrso_wet_wrm_10yr),
                                   "anom_wrm_hfls" = c(anom_hfls_dry_wrm_10yr, anom_hfls_wet_wrm_10yr),
                                   "dry_wet" = c(rep('dry',120),rep('wet',120)),
                                   "month" = rep(c(t:(t+119)),2),
                                   "source_id" = rep(cmip6_data.df$source_id[i],240)))
    decennial.df <- rbind(decennial.df,
                          data.frame("trend_tas" = unname(c(trend_tas_dry_10yr, trend_tas_wet_10yr)),
                                     "trend_mrso" = unname(c(trend_mrso_dry_10yr, trend_mrso_wet_10yr)),
                                     "trend_hfls" = unname(c(trend_hfls_dry_10yr, trend_hfls_wet_10yr)),
                                     "av_tas" = c(av_tas_dry_10yr, av_tas_wet_10yr),
                                     "av_mrso" = c(av_mrso_dry_10yr, av_mrso_wet_10yr),
                                     "av_hfls" = c(av_hfls_dry_10yr, av_hfls_wet_10yr),
                                     "intercept_tas" = c(intercept_tas_dry_10yr, intercept_tas_wet_10yr),
                                     "intercept_mrso" = c(intercept_mrso_dry_10yr, intercept_mrso_wet_10yr),
                                     "intercept_hfls" = c(intercept_hfls_dry_10yr, intercept_hfls_wet_10yr),
                                     "ELI" = c(ELI_dry, ELI_wet),
                                     "corr_tas_hfls" = c(corr_tas_hfls_dry, corr_tas_hfls_wet),
                                     "corr_mrso_hfls" = c(corr_mrso_hfls_dry, corr_mrso_hfls_wet),
                                     "dry_wet" = c('dry','wet'),
                                     "month" = rep(t,2),
                                     "source_id" = rep(cmip6_data.df$source_id[i],2)))
  }
  
  save(monthly.df,
       decennial.df,
       
       file = "testdir/202112_dry_wet_example_ELI.RData")
  print(paste0(cmip6_data.df$source_id[i], " is done..."))
}

# Maybe for now we just focus on ACCESS-ESM1-5
slopes.df <- setNames(data.frame(matrix(ncol = 10, nrow = 0)),
                      c("slope_tas", "slope_mrso", "slope_hfls", 
                        "av_tas", "av_mrso", "av_hfls", 
                        "dry_wet","month","decade","source_id"))
decade <- 1
for(wet_dry in c('dry','wet')){
  for(t in seq(1,1440,120)){
    index <- which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == wet_dry & decennial.df$month == t)
    slopes.df <- rbind(slopes.df,
                       data.frame("slope_tas" = c(decennial.df$intercept_tas[index],
                                                  decennial.df$intercept_tas[index] + decennial.df$trend_tas[index]*120),
                                  "slope_mrso" = c(decennial.df$intercept_mrso[index],
                                                   decennial.df$intercept_mrso[index] + decennial.df$trend_mrso[index]*120),
                                  "slope_hfls" = c(decennial.df$intercept_hfls[index],
                                                   decennial.df$intercept_hfls[index] + decennial.df$trend_hfls[index]*120),
                                  "av_tas" = rep(decennial.df$av_tas[index],2),
                                  "av_mrso" = rep(decennial.df$av_mrso[index],2),
                                  "av_hfls" = rep(decennial.df$av_hfls[index],2),
                                  "dry_wet" = rep(wet_dry, 2),
                                  "month" = c(t,t+119),
                                  "decade" = decade,
                                  "source_id" = rep("ACCESS-ESM1-5", 2)))
    decade <- decade + 1
  }
  decade <- 1
}

cols_var <- c(brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2')

#########################################################################################################################
###############################################                       ###################################################
###############################################    separate: dry.     ###################################################
###############################################                       ###################################################
#########################################################################################################################

min_tas_y <- min(monthly.df$raw_tas[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry')])
max_tas_y <- max(monthly.df$raw_tas[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry')])
min_tas_y005 <- min_tas_y - .075*(max_tas_y - min_tas_y)
max_tas_y005 <- max_tas_y + .075*(max_tas_y - min_tas_y)
min_mrso_y <- min(monthly.df$raw_mrso[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry')])
max_mrso_y <- max(monthly.df$raw_mrso[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry')])
min_mrso_y005 <- min_mrso_y - .2*(max_mrso_y - min_mrso_y)
max_mrso_y005 <- max_mrso_y + .075*(max_mrso_y - min_mrso_y)
min_hfls_y <- min(monthly.df$raw_hfls[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry')])
max_hfls_y <- max(monthly.df$raw_hfls[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry')])
min_hfls_y005 <- min_hfls_y - .075*(max_hfls_y - min_hfls_y)
max_hfls_y005 <- max_hfls_y + .075*(max_hfls_y - min_hfls_y)

min_corr_mrso_hfls_y <- min(decennial.df$corr_mrso_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry')])
max_corr_mrso_hfls_y <- max(decennial.df$corr_mrso_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry')])
min_corr_mrso_hfls_y005 <- min_corr_mrso_hfls_y - .075*(max_corr_mrso_hfls_y - min_corr_mrso_hfls_y)
max_corr_mrso_hfls_y005 <- max_corr_mrso_hfls_y + .075*(max_corr_mrso_hfls_y - min_corr_mrso_hfls_y)
min_corr_tas_hfls_y <- min(decennial.df$corr_tas_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry')])
max_corr_tas_hfls_y <- max(decennial.df$corr_tas_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry')])
min_corr_tas_hfls_y005 <- min_corr_tas_hfls_y - .075*(max_corr_tas_hfls_y - min_corr_tas_hfls_y)
max_corr_tas_hfls_y005 <- max_corr_tas_hfls_y + .075*(max_corr_tas_hfls_y - min_corr_tas_hfls_y)
min_ELI_y <- min(decennial.df$ELI[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry')])
max_ELI_y <- max(decennial.df$ELI[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry')])
min_ELI_y005 <- min_ELI_y - .075*(max_ELI_y - min_ELI_y)
max_ELI_y005 <- max_ELI_y + .075*(max_ELI_y - min_ELI_y)

raw_tas_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry'),], aes(x=month,y=raw_tas,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_tas_y005,
                                 ymax = max_tas_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$dry_wet == 'dry'),], aes(x=month,y=slope_tas,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0), limits = c(min_tas_y005, max_tas_y005)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  geom_vline(xintercept = seq(120,1320,120),linetype = 'longdash') +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_tas_plot

raw_hfls_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry'),], aes(x=month,y=raw_hfls,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_hfls_y005,
                                 ymax = max_hfls_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$dry_wet == 'dry'),], aes(x=month,y=slope_hfls,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0), limits = c(min_hfls_y005, max_hfls_y005)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  geom_vline(xintercept = seq(120,1320,120),linetype = 'longdash') +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_hfls_plot

raw_mrso_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'dry'),], aes(x=month,y=raw_mrso,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_mrso_y005,
                                 ymax = max_mrso_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$dry_wet == 'dry'),], aes(x=month,y=slope_mrso,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0), limits = c(min_mrso_y005, max_mrso_y005)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  geom_vline(xintercept = seq(120,1320,120),linetype = 'longdash') +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_mrso_plot

# only 1 decade of raw data
raw_tas_plot_10yr <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=raw_tas,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$month < 121 & slopes.df$dry_wet == 'dry'),], aes(x=month,y=slope_tas,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = c("'80","'82","'84","'86","'88","'90"),
                     expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0), limits = c(min_tas_y005, max_tas_y005)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_tas_plot_10yr

raw_hfls_plot_10yr <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=raw_hfls,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$month < 121 & slopes.df$dry_wet == 'dry'),], aes(x=month,y=slope_hfls,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0), limits = c(min_hfls_y005, max_hfls_y005)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_hfls_plot_10yr

raw_mrso_plot_10yr <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=raw_mrso,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$month < 121 & slopes.df$dry_wet == 'dry'),], aes(x=month,y=slope_mrso,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0), limits = c(min_mrso_y005, max_mrso_y005)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_mrso_plot_10yr

# only 1 decade of detrended + moy time series
detrended_tas_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=detrended_tas,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,ymin=moy_tas,ymax=detrended_tas), col = cols_var[1], fill = cols_var[1], alpha = 0.25) +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=moy_tas), linetype = 'solid') +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = c("'80","'82","'84","'86","'88","'90"),
                     expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0), limits = c(min_tas_y005, max_tas_y005)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
detrended_tas_plot

detrended_hfls_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=detrended_hfls,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,ymin=moy_hfls,ymax=detrended_hfls), col = cols_var[3], fill = cols_var[3], alpha = 0.25) +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=moy_hfls), linetype = 'solid') +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0), limits = c(min_hfls_y005, max_hfls_y005)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
detrended_hfls_plot

detrended_mrso_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=detrended_mrso,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,ymin=moy_mrso,ymax=detrended_mrso), col = cols_var[2], fill = cols_var[2], alpha = 0.25) +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=moy_mrso), linetype = 'solid') +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0), limits = c(min_mrso_y005, max_mrso_y005)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
detrended_mrso_plot

# anomalies per decade
anom_tas_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=anom_wrm_tas,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=anom_tas,col=dry_wet), linetype = 'dashed') +
  geom_hline(yintercept = 0) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = c("'80","'82","'84","'86","'88","'90"), 
                     expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
anom_tas_plot

anom_mrso_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=anom_wrm_mrso,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=anom_mrso,col=dry_wet), linetype = 'dashed') +
  geom_hline(yintercept = 0) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
anom_mrso_plot

anom_hfls_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=anom_wrm_hfls,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'dry'),], aes(x=month,y=anom_hfls,col=dry_wet), linetype = 'dashed') +
  geom_hline(yintercept = 0) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
anom_hfls_plot

# ELI \& its individual components
corr_mrso_hfls_plot <- ggplot(decennial.df[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry'),], aes(x=month+60,y=corr_mrso_hfls,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_corr_mrso_hfls_y005,
                                 ymax = max_corr_mrso_hfls_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_point(size=1.5) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = seq(1,1441,120), linetype = 'longdash') +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous("cor(SM',ET')", expand = c(0,0), limits = c(min_corr_mrso_hfls_y005, max_corr_mrso_hfls_y005)) +
  scale_color_manual(values = c(brewer.pal(n=9,'BrBG')[2],brewer.pal(n=9,'BrBG')[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
corr_mrso_hfls_plot

corr_tas_hfls_plot <- ggplot(decennial.df[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry'),], aes(x=month+60,y=corr_tas_hfls,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_corr_tas_hfls_y005,
                                 ymax = max_corr_tas_hfls_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_point(size=1.5) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = seq(1,1441,120), linetype = 'longdash') +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous(expression("cor(T"[a]*"',ET')"), expand = c(0,0), limits = c(min_corr_tas_hfls_y005, max_corr_tas_hfls_y005)) +
  scale_color_manual(values = c(brewer.pal(n=9,'BrBG')[8],brewer.pal(n=9,'BrBG')[8])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
corr_tas_hfls_plot

ELI_plot <- ggplot(decennial.df[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'dry'),], aes(x=month+60,y=ELI,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_ELI_y005,
                                 ymax = max_ELI_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_point(size=1.5) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = seq(1,1441,120), linetype = 'longdash') +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = c(seq(1980,2080,20),""), expand = c(0,0)) +
  scale_y_continuous(expression("ELI"), expand = c(0,0), limits = c(min_ELI_y005, max_ELI_y005)) +
  scale_color_manual(values = c('black','black')) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
ELI_plot

dry_labels <- c("Raw time series of water-limited grid cell (1980 - 2100)",
                "Trend removal (1980 - 1990)",
                "Seasonal cycle removal",
                "Anomalies",
                "ELI and individual components")


dry_plots <- list(plot_grid(raw_mrso_plot, raw_hfls_plot, raw_tas_plot,nrow = 3, align = 'v'),
                  plot_grid(raw_mrso_plot_10yr, raw_hfls_plot_10yr, raw_tas_plot_10yr, nrow = 3, align = 'v'),
                  plot_grid(detrended_mrso_plot, detrended_hfls_plot, detrended_tas_plot, nrow = 3, align = 'v'),
                  plot_grid(anom_mrso_plot, anom_hfls_plot, anom_tas_plot, nrow = 3, align = 'v'),
                  plot_grid(corr_mrso_hfls_plot, corr_tas_hfls_plot, ELI_plot, nrow=3, align = 'v'))

dry_example <- dry_wet_plots(dry_plots, labels = dry_labels)

ggsave("testdir/SFig17.png", plot = dry_example, width = 12, height = 16, units = "in")

#########################################################################################################################
###############################################                       ###################################################
###############################################    separate: wet.     ###################################################
###############################################                       ###################################################
#########################################################################################################################

min_tas_y <- min(monthly.df$raw_tas[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet')])
max_tas_y <- max(monthly.df$raw_tas[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet')])
min_tas_y005 <- min_tas_y - .075*(max_tas_y - min_tas_y)
max_tas_y005 <- max_tas_y + .075*(max_tas_y - min_tas_y)
min_mrso_y <- min(monthly.df$raw_mrso[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet')])
max_mrso_y <- max(monthly.df$raw_mrso[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet')])
min_mrso_y005 <- min_mrso_y - .075*(max_mrso_y - min_mrso_y)
max_mrso_y005 <- max_mrso_y + .075*(max_mrso_y - min_mrso_y)
min_hfls_y <- min(monthly.df$raw_hfls[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet')])
max_hfls_y <- max(monthly.df$raw_hfls[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet')])
min_hfls_y005 <- min_hfls_y - .075*(max_hfls_y - min_hfls_y)
max_hfls_y005 <- max_hfls_y + .075*(max_hfls_y - min_hfls_y)

min_corr_mrso_hfls_y <- min(decennial.df$corr_mrso_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet')])
max_corr_mrso_hfls_y <- max(decennial.df$corr_mrso_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet')])
min_corr_mrso_hfls_y005 <- min_corr_mrso_hfls_y - .075*(max_corr_mrso_hfls_y - min_corr_mrso_hfls_y)
max_corr_mrso_hfls_y005 <- max_corr_mrso_hfls_y + .075*(max_corr_mrso_hfls_y - min_corr_mrso_hfls_y)
min_corr_tas_hfls_y <- min(decennial.df$corr_tas_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet')])
max_corr_tas_hfls_y <- max(decennial.df$corr_tas_hfls[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet')])
min_corr_tas_hfls_y005 <- min_corr_tas_hfls_y - .075*(max_corr_tas_hfls_y - min_corr_tas_hfls_y)
max_corr_tas_hfls_y005 <- max_corr_tas_hfls_y + .075*(max_corr_tas_hfls_y - min_corr_tas_hfls_y)
min_ELI_y <- min(decennial.df$ELI[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet')])
max_ELI_y <- max(decennial.df$ELI[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet')])
min_ELI_y005 <- min_ELI_y - .075*(max_ELI_y - min_ELI_y)
max_ELI_y005 <- max_ELI_y + .075*(max_ELI_y - min_ELI_y)

raw_tas_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet'),], aes(x=month,y=raw_tas,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_tas_y005,
                                 ymax = max_tas_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$dry_wet == 'wet'),], aes(x=month,y=slope_tas,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0), limits = c(min_tas_y005, max_tas_y005)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  geom_vline(xintercept = seq(120,1320,120),linetype = 'longdash') +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_tas_plot

raw_hfls_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet'),], aes(x=month,y=raw_hfls,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_hfls_y005,
                                 ymax = max_hfls_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$dry_wet == 'wet'),], aes(x=month,y=slope_hfls,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0), limits = c(min_hfls_y005, max_hfls_y005)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  geom_vline(xintercept = seq(120,1320,120),linetype = 'longdash') +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_hfls_plot

raw_mrso_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$dry_wet == 'wet'),], aes(x=month,y=raw_mrso,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_mrso_y005,
                                 ymax = max_mrso_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$dry_wet == 'wet'),], aes(x=month,y=slope_mrso,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0), limits = c(min_mrso_y005, max_mrso_y005)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  geom_vline(xintercept = seq(120,1320,120),linetype = 'longdash') +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_mrso_plot

# only 1 decade of raw data
raw_tas_plot_10yr <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=raw_tas,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$month < 121 & slopes.df$dry_wet == 'wet'),], aes(x=month,y=slope_tas,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = c("'80","'82","'84","'86","'88","'90"), 
                     expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0), limits = c(min_tas_y005, max_tas_y005)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_tas_plot_10yr

raw_hfls_plot_10yr <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=raw_hfls,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$month < 121 & slopes.df$dry_wet == 'wet'),], aes(x=month,y=slope_hfls,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0), limits = c(min_hfls_y005, max_hfls_y005)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_hfls_plot_10yr

raw_mrso_plot_10yr <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=raw_mrso,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = slopes.df[which(slopes.df$month < 121 & slopes.df$dry_wet == 'wet'),], aes(x=month,y=slope_mrso,group=decade)) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0), limits = c(min_mrso_y005, max_mrso_y005)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
raw_mrso_plot_10yr

# only 1 decade of detrended + moy time series
detrended_tas_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=detrended_tas,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,ymin=moy_tas,ymax=detrended_tas), col = cols_var[1], fill = cols_var[1], alpha = 0.25) +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=moy_tas), linetype = 'solid') +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = c("'80","'82","'84","'86","'88","'90"), 
                     expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0), limits = c(min_tas_y005, max_tas_y005)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
detrended_tas_plot

detrended_hfls_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=detrended_hfls,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,ymin=moy_hfls,ymax=detrended_hfls), col = cols_var[3], fill = cols_var[3], alpha = 0.25) +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=moy_hfls), linetype = 'solid') +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0), limits = c(min_hfls_y005, max_hfls_y005)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
detrended_hfls_plot

detrended_mrso_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=detrended_mrso,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_ribbon(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,ymin=moy_mrso,ymax=detrended_mrso), col = cols_var[2], fill = cols_var[2], alpha = 0.25) +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=moy_mrso), linetype = 'solid') +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0), limits = c(min_mrso_y005, max_mrso_y005)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
detrended_mrso_plot

# anomalies per decade
anom_tas_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=anom_wrm_tas,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=anom_tas,col=dry_wet), linetype = 'dashed') +
  geom_hline(yintercept = 0) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = c("'80","'82","'84","'86","'88","'90"), 
                     expand = c(0,0)) +
  scale_y_continuous(expression("T"[a]*"(K)"), expand = c(0,0)) +
  scale_color_manual(values = c(cols_var[1],cols_var[1])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
anom_tas_plot

anom_mrso_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=anom_wrm_mrso,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=anom_mrso,col=dry_wet), linetype = 'dashed') +
  geom_hline(yintercept = 0) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous("SM (mm)", expand = c(0,0)) +
  scale_color_manual(values = c(cols_var[2],cols_var[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
anom_mrso_plot

anom_hfls_plot <- ggplot(monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=anom_wrm_hfls,col=dry_wet)) +
  geom_line(linetype = 'solid') +
  geom_line(inherit.aes = F, data = monthly.df[which(monthly.df$source_id == "ACCESS-ESM1-5" & monthly.df$month < 121 & monthly.df$dry_wet == 'wet'),], aes(x=month,y=anom_hfls,col=dry_wet), linetype = 'dashed') +
  geom_hline(yintercept = 0) +
  scale_x_continuous("",
                     breaks = seq(1,121,24),
                     labels = seq(1980,1990,2), expand = c(0,0)) +
  scale_y_continuous(expression(paste("ET (mm/d)")), expand = c(0,0)) +
  scale_color_manual(values = c(cols_var[3],cols_var[3])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
anom_hfls_plot

# ELI \& its individual components
corr_mrso_hfls_plot <- ggplot(decennial.df[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet'),], aes(x=month+60,y=corr_mrso_hfls,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_corr_mrso_hfls_y005,
                                 ymax = max_corr_mrso_hfls_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_point(size=1.5) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = seq(1,1441,120), linetype = 'longdash') +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous("cor(SM',ET')", expand = c(0,0), limits = c(min_corr_mrso_hfls_y005, max_corr_mrso_hfls_y005)) +
  scale_color_manual(values = c(brewer.pal(n=9,'BrBG')[2],brewer.pal(n=9,'BrBG')[2])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
corr_mrso_hfls_plot

corr_tas_hfls_plot <- ggplot(decennial.df[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet'),], aes(x=month+60,y=corr_tas_hfls,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_corr_tas_hfls_y005,
                                 ymax = max_corr_tas_hfls_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_point(size=1.5) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = seq(1,1441,120), linetype = 'longdash') +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = seq(1980,2100,20), expand = c(0,0)) +
  scale_y_continuous(expression("cor(T"[a]*"',ET')"), expand = c(0,0), limits = c(min_corr_tas_hfls_y005, max_corr_tas_hfls_y005)) +
  scale_color_manual(values = c(brewer.pal(n=9,'BrBG')[8],brewer.pal(n=9,'BrBG')[8])) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
corr_tas_hfls_plot

ELI_plot <- ggplot(decennial.df[which(decennial.df$source_id == "ACCESS-ESM1-5" & decennial.df$dry_wet == 'wet'),], aes(x=month+60,y=ELI,col=dry_wet)) +
  geom_rect(inherit.aes = F, aes(xmin = 1,
                                 xmax = 120,
                                 ymin = min_ELI_y005,
                                 ymax = max_ELI_y005), col = 'transparent',fill = alpha('grey', .05)) +
  geom_point(size=1.5) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = seq(1,1441,120), linetype = 'longdash') +
  scale_x_continuous("",
                     breaks = seq(1,1441,240),
                     labels = c(seq(1980,2080,20),""), expand = c(0,0)) +
  scale_y_continuous(expression("ELI"), expand = c(0,0), limits = c(min_ELI_y005, max_ELI_y005)) +
  scale_color_manual(values = c('black','black')) +
  theme(legend.position = "none",
        strip.text = element_text(size=16),
        strip.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=16),
        axis.text.x = element_text(hjust = .2),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  )
ELI_plot

wet_labels <- c("Raw time series of energy-limited grid cell (1980 - 2100)",
                "Trend removal (1980 - 1990)",
                "Seasonal cycle removal",
                "Anomalies",
                "ELI and individual components")


wet_plots <- list(plot_grid(raw_mrso_plot, raw_hfls_plot, raw_tas_plot,nrow = 3, align = 'v'),
                  plot_grid(raw_mrso_plot_10yr, raw_hfls_plot_10yr, raw_tas_plot_10yr, nrow = 3, align = 'v'),
                  plot_grid(detrended_mrso_plot, detrended_hfls_plot, detrended_tas_plot, nrow = 3, align = 'v'),
                  plot_grid(anom_mrso_plot, anom_hfls_plot, anom_tas_plot, nrow = 3, align = 'v'),
                  plot_grid(corr_mrso_hfls_plot, corr_tas_hfls_plot, ELI_plot, nrow=3, align = 'v'))

wet_example <- dry_wet_plots(wet_plots, labels = wet_labels)

ggsave("testdir/SFig18.png", plot = wet_example, width = 12, height = 16, units = "in")
