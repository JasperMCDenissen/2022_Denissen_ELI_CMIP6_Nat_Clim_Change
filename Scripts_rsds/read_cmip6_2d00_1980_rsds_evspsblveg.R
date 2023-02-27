# By: Jasper Denissen
# 2021-05-18
# Script to read CMIP6 data 
# 1981 - 2100, 2.0x2.0 grid cell resolution WITH THE RASTER PACKAGE!
# 1) Read in all data and average to monthly timescale
# 2) detrend 
# 3) anomalies
# 4) keep only anomalies when T > threshold (10 deg C)
# 5) calculate correlation difference over all available data

##############################################################################################################################
##############################################################################################################################
######################## !!! Don't forget to reset the working directory to a directory of your choosing!!! ##################
##############################################################################################################################
##############################################################################################################################
setwd('/Net/Groups/BGI/work_3/HydroBioClim/archive/Denissen_etal_2022_Nat_Clim_Change/')

# packages
source('Scripts/to_be_loaded_packages.R')

# load table containing source_id and member_id of the regridded CMIP6
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
cmip6_data.df <- cmip6_data.df[c(1,2,4,5,6,8,10),]
# load functions
source('Scripts/functions/parallel_raster_functions.R')

# defined month of year for a 10 year period
cmip6_path <- "Data/cmip6_202106/"
files <- list.files(cmip6_path,
                    pattern = ".nc$")
cmip6_path_he <- "Data/cmip6_202106_w_evspsblveg/"
files_he <- list.files(cmip6_path_he,
                       pattern = ".nc$")
vars <- c('tas','mrso','hurs','evspsblveg','rsds')
moy <- rep(1:12,10)
dcorr_t.list <-
  corr_rgy_veg_t.list <-
  corr_wtr_veg_t.list <-
  av_hurs.list <-
  av_evspsblveg.list <-
  list()
for(i in 1:length(cmip6_data.df$source_id)){
  UseCores <- 32
  beginCluster(n = UseCores)
  for(var in vars){
    print(var)
    # read all the data from the different variables. historical and ssp585 sequentially, which means stacks of 180*90*1680
    if(var == 'tas' | var == 'mrso' | var == 'rlds' | var == 'rlus' | var == 'rsds' | var == 'rsus'){
      if(i %in% which(cmip6_data.df$source_id != "CAMS-CSM1-0" & cmip6_data.df$source_id != "TaiESM1")){ # in CAMS-CSM1-0 we are missing the last year apparently, so we repeat values of 2099
        assign(paste0("stack_",var), stack(paste0(cmip6_path,files[which(grepl(cmip6_data.df$source_id[i], files) & 
                                                                           grepl(var, files) & grepl("2.0x2.0", files))]))[[13:1452]])
      }else if(cmip6_data.df$source_id[i] == "CAMS-CSM1-0"){
        assign(paste0("stack_",var), stack(paste0(cmip6_path,files[which(grepl(cmip6_data.df$source_id[i], files) & 
                                                                           grepl(var, files) & grepl("2.0x2.0", files))]))[[13:1440]])
        assign(paste0("stack_",var), stack(stack(eval(parse(text = paste0("stack_",var)))), stack(eval(parse(text = paste0("stack_",var))))[[1417:1428]]))
      }else{
        assign(paste0("stack_",var), stack(paste0(cmip6_path,files[which(grepl(cmip6_data.df$source_id[i], files) & 
                                                                           grepl(var, files) & grepl("2.0x2.0", files))]))[[13:1451]])
        assign(paste0("stack_",var), stack(stack(eval(parse(text = paste0("stack_",var)))), stack(eval(parse(text = paste0("stack_",var))))[[1427]]))
      }
    }else{
      if(i %in% which(cmip6_data.df$source_id != "CAMS-CSM1-0" & cmip6_data.df$source_id != "TaiESM1")){ # in CAMS-CSM1-0 we are missing the last year apparently, so we repeat values of 2099
        assign(paste0("stack_",var), stack(paste0(cmip6_path_he,files_he[which(grepl(cmip6_data.df$source_id[i], files_he) & 
                                                                                 grepl(var, files_he) & grepl("2.0x2.0", files_he))]))[[13:1452]])
      }else if(cmip6_data.df$source_id[i] == "CAMS-CSM1-0"){
        assign(paste0("stack_",var), stack(paste0(cmip6_path_he,files_he[which(grepl(cmip6_data.df$source_id[i], files_he) & 
                                                                                 grepl(var, files_he) & grepl("2.0x2.0", files_he))]))[[13:1440]])
        assign(paste0("stack_",var), stack(stack(eval(parse(text = paste0("stack_",var)))), stack(eval(parse(text = paste0("stack_",var))))[[1417:1428]]))
      }else{
        assign(paste0("stack_",var), stack(paste0(cmip6_path_he,files_he[which(grepl(cmip6_data.df$source_id[i], files_he) & 
                                                                                 grepl(var, files_he) & grepl("2.0x2.0", files_he))]))[[13:1451]])
        assign(paste0("stack_",var), stack(stack(eval(parse(text = paste0("stack_",var)))), stack(eval(parse(text = paste0("stack_",var))))[[1427]]))
      }
    }
  }
  # Mask all data when tas_mon < 273.15+10
  tas_mask <- clusterR(stack_tas, calc, args = list(temp_K_mask_fun), progress = 'text')
  # analyze every 10 year block and shift by 10 years. starting at 1981-1990, 1991-2000, 2001-2010, 2011-2020, etc
  for(t in seq(1,1440,120)){ # so the loop moves 10 years and in the loop we read in 10 years of data.
    # tas_10yr <- stack(stack_tas[[t:(t+119)]])
    # detrend_tas_10yr <- clusterR(tas_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    # moy_tas_10yr <- clusterR(detrend_tas_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    # moy_tas_10yr_rep <- stack(replicate(10, moy_tas_10yr))
    # anom_tas_10yr <- clusterR(detrend_tas_10yr, fun = function(a,b){return(a-b)},
    #                           args = list(b = moy_tas_10yr_rep), progress = 'text')
    # anom_tas_wrm_10yr <- clusterR(anom_tas_10yr, fun = function(a,b){return(a*b)},
    #                               args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # mrso
    mrso_10yr <- stack(stack_mrso[[t:(t+119)]])
    detrend_mrso_10yr <- clusterR(mrso_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    moy_mrso_10yr <- clusterR(detrend_mrso_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    moy_mrso_10yr_rep <- stack(replicate(10, moy_mrso_10yr))
    anom_mrso_10yr <- clusterR(detrend_mrso_10yr, fun = function(a,b){return(a-b)},
                               args = list(b = moy_mrso_10yr_rep), progress = 'text')
    anom_mrso_wrm_10yr <- clusterR(anom_mrso_10yr, fun = function(a,b){return(a*b)},
                                   args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # evspsblveg
    evspsblveg_10yr <- stack(stack_evspsblveg[[t:(t+119)]])
    av_evspsblveg_10yr <- clusterR(evspsblveg_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    detrend_evspsblveg_10yr <- clusterR(evspsblveg_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    moy_evspsblveg_10yr <- clusterR(detrend_evspsblveg_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    moy_evspsblveg_10yr_rep <- stack(replicate(10, moy_evspsblveg_10yr))
    anom_evspsblveg_10yr <- clusterR(detrend_evspsblveg_10yr, fun = function(a,b){return(a-b)},
                                     args = list(b = moy_evspsblveg_10yr_rep), progress = 'text')
    anom_evspsblveg_wrm_10yr <- clusterR(anom_evspsblveg_10yr, fun = function(a,b){return(a*b)},
                                         args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    
    # # rlds
    # rlds_10yr <- stack(stack_rlds[[t:(t+119)]])
    # av_rlds_10yr <- clusterR(rlds_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # # rlus
    # rlus_10yr <- stack(stack_rlus[[t:(t+119)]])
    # av_rlus_10yr <- clusterR(rlus_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # rsds
    rsds_10yr <- stack(stack_rsds[[t:(t+119)]])
    detrend_rsds_10yr <- clusterR(rsds_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    moy_rsds_10yr <- clusterR(detrend_rsds_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    moy_rsds_10yr_rep <- stack(replicate(10, moy_rsds_10yr))
    anom_rsds_10yr <- clusterR(detrend_rsds_10yr, fun = function(a,b){return(a-b)},
                               args = list(b = moy_rsds_10yr_rep), progress = 'text')
    anom_rsds_wrm_10yr <- clusterR(anom_rsds_10yr, fun = function(a,b){return(a*b)},
                                   args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # av_rsds_10yr <- clusterR(rsds_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # # rsus
    # rsus_10yr <- stack(stack_rsus[[t:(t+119)]])
    # av_rsus_10yr <- clusterR(rsus_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    
    # # calculate net radiation
    # swnet_10yr <- clusterR(rsds_10yr, fun = function(a,b){return(a-b)},
    #                        args = list(b = rsus_10yr), progress = 'text')
    # lwnet_10yr <- clusterR(rlds_10yr, fun = function(a,b){return(a-b)},
    #                        args = list(b = rlus_10yr), progress = 'text')
    # netrad_10yr <- clusterR(swnet_10yr, fun = function(a,b){return(a+b)},
    #                         args = list(b = lwnet_10yr), progress = 'text')
    # detrend_netrad_10yr <- clusterR(netrad_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    # moy_netrad_10yr <- clusterR(detrend_netrad_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    # moy_netrad_10yr_rep <- stack(replicate(10, moy_netrad_10yr))
    # anom_netrad_10yr <- clusterR(detrend_netrad_10yr, fun = function(a,b){return(a-b)},
    #                              args = list(b = moy_netrad_10yr_rep), progress = 'text')
    # anom_netrad_wrm_10yr <- clusterR(anom_netrad_10yr, fun = function(a,b){return(a*b)},
    #                                  args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # hurs
    hurs_10yr <- stack(stack_hurs[[t:(t+119)]])
    av_hurs_10yr <- clusterR(hurs_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # dcorr
    
    # stack_anom_netrad_evspsblveg_wrm <- stack(anom_netrad_wrm_10yr, anom_evspsblveg_wrm_10yr)
    # stack_anom_mrso_evspsblveg_wrm <- stack(anom_mrso_wrm_10yr, anom_evspsblveg_wrm_10yr)
    # corr_rgy_veg_10yr_t <- clusterR(stack_anom_netrad_evspsblveg_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    # corr_wtr_veg_10yr_t <- clusterR(stack_anom_mrso_evspsblveg_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    # dcorr_10yr_t <- clusterR(corr_rgy_veg_10yr_t, fun = function(a,b){return(a-b)},
    #                          args = list(b = corr_wtr_veg_10yr_t), progress = 'text')
    
    stack_anom_rsds_evspsblveg_wrm <- stack(anom_rsds_wrm_10yr, anom_evspsblveg_wrm_10yr)
    stack_anom_mrso_evspsblveg_wrm <- stack(anom_mrso_wrm_10yr, anom_evspsblveg_wrm_10yr)
    corr_rgy_veg_10yr_t <- clusterR(stack_anom_rsds_evspsblveg_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    corr_wtr_veg_10yr_t <- clusterR(stack_anom_mrso_evspsblveg_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    dcorr_10yr_t <- clusterR(corr_rgy_veg_10yr_t, fun = function(a,b){return(a-b)},
                             args = list(b = corr_wtr_veg_10yr_t), progress = 'text')
    
    if(t == 1){
      dcorr_t <- dcorr_10yr_t
      corr_rgy_veg_t <- corr_rgy_veg_10yr_t
      corr_wtr_veg_t <- corr_wtr_veg_10yr_t
      av_hurs <- av_hurs_10yr
      av_evspsblveg <- av_evspsblveg_10yr
    }else{
      dcorr_t <- stack(dcorr_t, dcorr_10yr_t)
      corr_rgy_veg_t <- stack(corr_rgy_veg_t, corr_rgy_veg_10yr_t)
      corr_wtr_veg_t <- stack(corr_wtr_veg_t, corr_wtr_veg_10yr_t)
      av_hurs <- stack(av_hurs, av_hurs_10yr)
      av_evspsblveg <- stack(av_evspsblveg, av_evspsblveg_10yr)
    }
    print(paste("model ",i," and month ",t," are done...",sep=''))
  }
  # end cluster
  endCluster()
  dcorr_t.list[[i]] <- aperm(as.array(dcorr_t), c(2,1,3))[,90:1,]
  corr_rgy_veg_t.list[[i]] <- aperm(as.array(corr_rgy_veg_t), c(2,1,3))[,90:1,]
  corr_wtr_veg_t.list[[i]] <- aperm(as.array(corr_wtr_veg_t), c(2,1,3))[,90:1,]
  av_hurs.list[[i]] <- aperm(as.array(av_hurs), c(2,1,3))[,90:1,]
  av_evspsblveg.list[[i]] <- aperm(as.array(av_evspsblveg), c(2,1,3))[,90:1,]
  save(dcorr_t.list,
       corr_rgy_veg_t.list,
       corr_wtr_veg_t.list,
       av_hurs.list,
       av_evspsblveg.list,
       file = "testdir/202112_dcorr_cmip6_10yr_rsds_evspsblveg.RData")
}


