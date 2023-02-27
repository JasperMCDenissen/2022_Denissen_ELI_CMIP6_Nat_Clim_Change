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

# load functions
source('Scripts/functions/parallel_raster_functions.R')

# defined month of year for a 10 year period
cmip6_path <- "Data/cmip6_202106/"
vars <- c('tas','hfls','lai','mrso','pr','rlds','rlus','rsds','rsus')
files <- list.files(cmip6_path,
                    pattern = ".nc$")
moy <- rep(1:12,10)
# load("/Net/Groups/BGI/people/jdenis/scripts/Data/202105_dcorr_cmip6_10yr.RData")
dcorr.list <-
  corr_rgy_veg.list <-
  corr_wtr_veg.list <-
  # dcorr_netrad.list <-
  # corr_rgy_veg_netrad.list <-
  # corr_wtr_veg_netrad.list <-
  dcorr_rsds.list <-
  corr_rgy_veg_rsds.list <-
  corr_wtr_veg_rsds.list <-
  dpcorr.list <-
  pcorr_rgy_veg.list <-
  pcorr_wtr_veg.list <-
  av_tas.list <-
  av_mrso.list <-
  av_lai.list <-
  av_hfls.list <-
  av_rlds.list <-
  av_rlus.list <-
  av_rsds.list <-
  av_rsus.list <-
  av_pr.list <-
  dcorr_seascycle.list <-
  sd_anom_tas.list <-
  sd_anom_mrso.list <-
  sd_anom_hfls.list <-
  list()
for(i in 1:length(cmip6_data.df$source_id)){
  UseCores <- 32
  beginCluster(n = UseCores)
  for(var in vars){
    print(var)
    # read all the data from the different variables. historical and ssp585 sequentially, which means stacks of 180*90*1680
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
  }
  # Mask all data when tas_mon < 273.15+10
  tas_mask <- clusterR(stack_tas, calc, args = list(temp_K_mask_fun), progress = 'text')
  # analyze every 10 year block and shift by 10 years. starting at 1981-1990, 1991-2000, 2001-2010, 2011-2020, etc
  for(t in seq(1,1440,120)){ # so the loop moves 10 years and in the loop we read in 10 years of data.
    tas_10yr <- stack(stack_tas[[t:(t+119)]])
    av_tas_10yr <- clusterR(tas_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    detrend_tas_10yr <- clusterR(tas_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    moy_tas_10yr <- clusterR(detrend_tas_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    moy_tas_10yr_rep <- stack(replicate(10, moy_tas_10yr))
    anom_tas_10yr <- clusterR(detrend_tas_10yr, fun = function(a,b){return(a-b)},
                              args = list(b = moy_tas_10yr_rep), progress = 'text')
    sd_anom_tas_10yr <- clusterR(anom_tas_10yr, calc, args = list(sd, na.rm=T), progress = 'text')
    anom_tas_wrm_10yr <- clusterR(anom_tas_10yr, fun = function(a,b){return(a*b)},
                                  args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # hfls
    hfls_10yr <- stack(stack_hfls[[t:(t+119)]])
    av_hfls_10yr <- clusterR(hfls_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    detrend_hfls_10yr <- clusterR(hfls_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    moy_hfls_10yr <- clusterR(detrend_hfls_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    moy_hfls_10yr_rep <- stack(replicate(10, moy_hfls_10yr))
    anom_hfls_10yr <- clusterR(detrend_hfls_10yr, fun = function(a,b){return(a-b)},
                               args = list(b = moy_hfls_10yr_rep), progress = 'text')
    sd_anom_hfls_10yr <- clusterR(anom_hfls_10yr, calc, args = list(sd, na.rm=T), progress = 'text')
    anom_hfls_wrm_10yr <- clusterR(anom_hfls_10yr, fun = function(a,b){return(a*b)},
                                   args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # mrso
    mrso_10yr <- stack(stack_mrso[[t:(t+119)]])
    av_mrso_10yr <- clusterR(mrso_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    detrend_mrso_10yr <- clusterR(mrso_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    moy_mrso_10yr <- clusterR(detrend_mrso_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    moy_mrso_10yr_rep <- stack(replicate(10, moy_mrso_10yr))
    anom_mrso_10yr <- clusterR(detrend_mrso_10yr, fun = function(a,b){return(a-b)},
                               args = list(b = moy_mrso_10yr_rep), progress = 'text')
    sd_anom_mrso_10yr <- clusterR(anom_mrso_10yr, calc, args = list(sd, na.rm=T), progress = 'text')
    anom_mrso_wrm_10yr <- clusterR(anom_mrso_10yr, fun = function(a,b){return(a*b)},
                                   args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # rlds
    rlds_10yr <- stack(stack_rlds[[t:(t+119)]])
    av_rlds_10yr <- clusterR(rlds_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # rlus
    rlus_10yr <- stack(stack_rlus[[t:(t+119)]])
    av_rlus_10yr <- clusterR(rlus_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # rsds
    rsds_10yr <- stack(stack_rsds[[t:(t+119)]])
    av_rsds_10yr <- clusterR(rsds_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # rsus
    rsus_10yr <- stack(stack_rsus[[t:(t+119)]])
    av_rsus_10yr <- clusterR(rsus_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    
    # # calculate net radiation
    # swnet_10yr <- clusterR(rsds_10yr, fun = function(a,b){return(a-b)},
    #                        args = list(b = rsus_10yr), progress = 'text')
    # lwnet_10yr <- clusterR(rlds_10yr, fun = function(a,b){return(a-b)},
    #                        args = list(b = rlus_10yr), progress = 'text')
    # 
    # netrad_10yr <- clusterR(swnet_10yr, fun = function(a,b){return(a+b)},
    #                         args = list(b = lwnet_10yr), progress = 'text')
    
    # detrend_netrad_10yr <- clusterR(netrad_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    # moy_netrad_10yr <- clusterR(detrend_netrad_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    # moy_netrad_10yr_rep <- stack(replicate(10, moy_netrad_10yr))
    # anom_netrad_10yr <- clusterR(detrend_netrad_10yr, fun = function(a,b){return(a-b)},
    #                              args = list(b = moy_netrad_10yr_rep), progress = 'text')
    # sd_anom_netrad_10yr <- clusterR(anom_netrad_10yr, calc, args = list(sd, na.rm=T), progress = 'text')
    # anom_netrad_wrm_10yr <- clusterR(anom_netrad_10yr, fun = function(a,b){return(a*b)},
    #                                  args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    
    detrend_rsds_10yr <- clusterR(rsds_10yr, calc, args = list(detrend_fun_10yr), progress = 'text')
    moy_rsds_10yr <- clusterR(detrend_rsds_10yr, stackApply, args = list(indices = moy, fun = mean, na.rm = T), progress = 'text')
    moy_rsds_10yr_rep <- stack(replicate(10, moy_rsds_10yr))
    anom_rsds_10yr <- clusterR(detrend_rsds_10yr, fun = function(a,b){return(a-b)},
                                 args = list(b = moy_rsds_10yr_rep), progress = 'text')
    sd_anom_rsds_10yr <- clusterR(anom_rsds_10yr, calc, args = list(sd, na.rm=T), progress = 'text')
    anom_rsds_wrm_10yr <- clusterR(anom_rsds_10yr, fun = function(a,b){return(a*b)},
                                     args = list(b = tas_mask[[t:(t+119)]]), progress = 'text')
    # pr
    pr_10yr <- stack(stack_pr[[t:(t+119)]])
    av_pr_10yr <- clusterR(pr_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # lai
    lai_10yr <- stack(stack_lai[[t:(t+119)]])
    av_lai_10yr <- clusterR(lai_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
    # dcorr
    # corr_fun_10yr requires to stack the two variables to be correlated
    stack_anom_tas_hfls_wrm <- stack(anom_tas_wrm_10yr, anom_hfls_wrm_10yr)
    stack_anom_mrso_hfls_wrm <- stack(anom_mrso_wrm_10yr, anom_hfls_wrm_10yr)
    corr_rgy_veg_10yr <- clusterR(stack_anom_tas_hfls_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    corr_wtr_veg_10yr <- clusterR(stack_anom_mrso_hfls_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    dcorr_10yr <- clusterR(corr_rgy_veg_10yr, fun = function(a,b){return(a-b)},
                           args = list(b = corr_wtr_veg_10yr), progress = 'text')
    
    # # ELI with netrad
    # stack_anom_tas_netrad_wrm <- stack(anom_netrad_wrm_10yr, anom_hfls_wrm_10yr)
    # stack_anom_mrso_netrad_wrm <- stack(anom_mrso_wrm_10yr, anom_hfls_wrm_10yr)
    # corr_rgy_veg_10yr_netrad <- clusterR(stack_anom_tas_netrad_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    # corr_wtr_veg_10yr_netrad <- clusterR(stack_anom_mrso_netrad_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    # dcorr_10yr_netrad <- clusterR(corr_rgy_veg_10yr_netrad, fun = function(a,b){return(a-b)},
    #                               args = list(b = corr_wtr_veg_10yr_netrad), progress = 'text')
    
    # ELI with rsds
    stack_anom_tas_rsds_wrm <- stack(anom_rsds_wrm_10yr, anom_hfls_wrm_10yr)
    stack_anom_mrso_rsds_wrm <- stack(anom_mrso_wrm_10yr, anom_hfls_wrm_10yr)
    corr_rgy_veg_10yr_rsds <- clusterR(stack_anom_tas_rsds_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    corr_wtr_veg_10yr_rsds <- clusterR(stack_anom_mrso_rsds_wrm, calc, args = list(corr_fun_10yr), progress = 'text')
    dcorr_10yr_rsds <- clusterR(corr_rgy_veg_10yr_rsds, fun = function(a,b){return(a-b)},
                                args = list(b = corr_wtr_veg_10yr_rsds), progress = 'text')
    
    # # pcorr_fun_10yr requires to stack the three variables to be correlated
    # stack_anom_netrad_hfls_mrso_wrm <- stack(anom_netrad_wrm_10yr, anom_hfls_wrm_10yr, anom_mrso_wrm_10yr)
    # stack_anom_mrso_hfls_mrso_wrm <- stack(anom_mrso_wrm_10yr, anom_hfls_wrm_10yr, anom_netrad_wrm_10yr)
    # pcorr_rgy_veg_10yr <- clusterR(stack_anom_netrad_hfls_mrso_wrm, calc, args = list(pcorr_fun_10yr), progress = 'text')
    # pcorr_wtr_veg_10yr <- clusterR(stack_anom_mrso_hfls_mrso_wrm, calc, args = list(pcorr_fun_10yr), progress = 'text')
    # dpcorr_10yr <- clusterR(pcorr_rgy_veg_10yr, fun = function(a,b){return(a-b)},
    #                         args = list(b = pcorr_wtr_veg_10yr), progress = 'text')
    
    # pcorr_fun_10yr requires to stack the three variables to be correlated
    stack_anom_rsds_hfls_mrso_wrm <- stack(anom_rsds_wrm_10yr, anom_hfls_wrm_10yr, anom_mrso_wrm_10yr)
    stack_anom_mrso_hfls_mrso_wrm <- stack(anom_mrso_wrm_10yr, anom_hfls_wrm_10yr, anom_rsds_wrm_10yr)
    pcorr_rgy_veg_10yr <- clusterR(stack_anom_rsds_hfls_mrso_wrm, calc, args = list(pcorr_fun_10yr), progress = 'text')
    pcorr_wtr_veg_10yr <- clusterR(stack_anom_mrso_hfls_mrso_wrm, calc, args = list(pcorr_fun_10yr), progress = 'text')
    dpcorr_10yr <- clusterR(pcorr_rgy_veg_10yr, fun = function(a,b){return(a-b)},
                            args = list(b = pcorr_wtr_veg_10yr), progress = 'text')
    
    for(s in 1:12){
      # stack_anom_netrad_hfls_wrm_moy <- stack(anom_netrad_wrm_10yr[[which(moy == s)]], anom_hfls_wrm_10yr[[which(moy == s)]])
      # stack_anom_mrso_hfls_wrm_moy <- stack(anom_mrso_wrm_10yr[[which(moy == s)]], anom_hfls_wrm_10yr[[which(moy == s)]])
      # corr_rgy_veg_moy <- clusterR(stack_anom_netrad_hfls_wrm_moy, calc, args = list(corr_fun_seascycle), progress = 'text')
      # corr_wtr_veg_moy <- clusterR(stack_anom_mrso_hfls_wrm_moy, calc, args = list(corr_fun_seascycle), progress = 'text')
      # dcorr_moy <- clusterR(corr_rgy_veg_moy, fun = function(a,b){return(a-b)},
      #                       args = list(b = corr_wtr_veg_moy), progress = 'text')
      
      stack_anom_rsds_hfls_wrm_moy <- stack(anom_rsds_wrm_10yr[[which(moy == s)]], anom_hfls_wrm_10yr[[which(moy == s)]])
      stack_anom_mrso_hfls_wrm_moy <- stack(anom_mrso_wrm_10yr[[which(moy == s)]], anom_hfls_wrm_10yr[[which(moy == s)]])
      corr_rgy_veg_moy <- clusterR(stack_anom_rsds_hfls_wrm_moy, calc, args = list(corr_fun_seascycle), progress = 'text')
      corr_wtr_veg_moy <- clusterR(stack_anom_mrso_hfls_wrm_moy, calc, args = list(corr_fun_seascycle), progress = 'text')
      dcorr_moy <- clusterR(corr_rgy_veg_moy, fun = function(a,b){return(a-b)},
                            args = list(b = corr_wtr_veg_moy), progress = 'text')
      if(s == 1){
        dcorr_seascycle_10yr <- dcorr_moy # This stack will have 12 month-of-year
      }else{
        dcorr_seascycle_10yr <- stack(dcorr_seascycle_10yr, dcorr_moy)
      }
    }
    if(t == 1){
      dcorr <- dcorr_10yr
      corr_rgy_veg <- corr_rgy_veg_10yr
      corr_wtr_veg <- corr_wtr_veg_10yr
      # dcorr_netrad <- dcorr_10yr_netrad
      # corr_rgy_veg_netrad <- corr_rgy_veg_10yr_netrad
      # corr_wtr_veg_netrad <- corr_wtr_veg_10yr_netrad
      dcorr_rsds <- dcorr_10yr_rsds
      corr_rgy_veg_rsds <- corr_rgy_veg_10yr_rsds
      corr_wtr_veg_rsds <- corr_wtr_veg_10yr_rsds
      dpcorr <- dpcorr_10yr
      pcorr_rgy_veg <- pcorr_rgy_veg_10yr
      pcorr_wtr_veg <- pcorr_wtr_veg_10yr
      av_tas <- av_tas_10yr
      av_hfls <- av_hfls_10yr
      av_rlds <- av_rlds_10yr
      av_rlus <- av_rlus_10yr
      av_rsds <- av_rsds_10yr
      av_rsus <- av_rsus_10yr
      av_pr <- av_pr_10yr
      av_lai <- av_lai_10yr
      av_mrso <- av_mrso_10yr
      dcorr_seascycle <- dcorr_seascycle_10yr
      sd_anom_tas <- sd_anom_tas_10yr
      sd_anom_mrso <- sd_anom_mrso_10yr
      sd_anom_hfls <- sd_anom_hfls_10yr
    }else{
      dcorr <- stack(dcorr, dcorr_10yr)
      corr_rgy_veg <- stack(corr_rgy_veg, corr_rgy_veg_10yr)
      corr_wtr_veg <- stack(corr_wtr_veg, corr_wtr_veg_10yr)
      dpcorr <- stack(dpcorr, dpcorr_10yr)
      pcorr_rgy_veg <- stack(pcorr_rgy_veg, pcorr_rgy_veg_10yr)
      pcorr_wtr_veg <- stack(pcorr_wtr_veg, pcorr_wtr_veg_10yr)
      # dcorr_netrad <- stack(dcorr_netrad, dcorr_10yr_netrad)
      # corr_rgy_veg_netrad <- stack(corr_rgy_veg_netrad, corr_rgy_veg_10yr_netrad)
      # corr_wtr_veg_netrad <- stack(corr_wtr_veg_netrad, corr_wtr_veg_10yr_netrad)
      dcorr_rsds <- stack(dcorr_rsds, dcorr_10yr_rsds)
      corr_rgy_veg_rsds <- stack(corr_rgy_veg_rsds, corr_rgy_veg_10yr_rsds)
      corr_wtr_veg_rsds <- stack(corr_wtr_veg_rsds, corr_wtr_veg_10yr_rsds)
      av_tas <- stack(av_tas, av_tas_10yr)
      av_hfls <- stack(av_hfls, av_hfls_10yr)
      av_rlds <- stack(av_rlds, av_rlds_10yr)
      av_rlus <- stack(av_rlus, av_rlus_10yr)
      av_rsds <- stack(av_rsds, av_rsds_10yr)
      av_rsus <- stack(av_rsus, av_rsus_10yr)
      av_pr <- stack(av_pr, av_pr_10yr)
      av_lai <- stack(av_lai, av_lai_10yr)
      av_mrso <- stack(av_mrso, av_mrso_10yr)
      dcorr_seascycle <- stack(dcorr_seascycle, dcorr_seascycle_10yr)
      sd_anom_tas <- stack(sd_anom_tas, sd_anom_tas_10yr)
      sd_anom_mrso <- stack(sd_anom_mrso, sd_anom_mrso_10yr)
      sd_anom_hfls <- stack(sd_anom_hfls, sd_anom_hfls_10yr)
    }
    print(paste("model ",i," and month ",t," are done...",sep=''))
  }
  # end cluster
  endCluster()
  dcorr.list[[i]] <- aperm(as.array(dcorr), c(2,1,3))[,90:1,]
  corr_rgy_veg.list[[i]] <- aperm(as.array(corr_rgy_veg), c(2,1,3))[,90:1,]
  corr_wtr_veg.list[[i]] <- aperm(as.array(corr_wtr_veg), c(2,1,3))[,90:1,]
  dpcorr.list[[i]] <- aperm(as.array(dpcorr), c(2,1,3))[,90:1,]
  pcorr_rgy_veg.list[[i]] <- aperm(as.array(pcorr_rgy_veg), c(2,1,3))[,90:1,]
  pcorr_wtr_veg.list[[i]] <- aperm(as.array(pcorr_wtr_veg), c(2,1,3))[,90:1,]
  # dcorr_netrad.list[[i]] <- aperm(as.array(dcorr_netrad), c(2,1,3))[,90:1,]
  # corr_rgy_veg_netrad.list[[i]] <- aperm(as.array(corr_rgy_veg_netrad), c(2,1,3))[,90:1,]
  # corr_wtr_veg_netrad.list[[i]] <- aperm(as.array(corr_wtr_veg_netrad), c(2,1,3))[,90:1,]
  dcorr_rsds.list[[i]] <- aperm(as.array(dcorr_rsds), c(2,1,3))[,90:1,]
  corr_rgy_veg_rsds.list[[i]] <- aperm(as.array(corr_rgy_veg_rsds), c(2,1,3))[,90:1,]
  corr_wtr_veg_rsds.list[[i]] <- aperm(as.array(corr_wtr_veg_rsds), c(2,1,3))[,90:1,]
  av_tas.list[[i]] <- aperm(as.array(av_tas), c(2,1,3))[,90:1,]
  av_hfls.list[[i]] <- aperm(as.array(av_hfls), c(2,1,3))[,90:1,]
  av_rlds.list[[i]] <- aperm(as.array(av_rlds), c(2,1,3))[,90:1,]
  av_rlus.list[[i]] <- aperm(as.array(av_rlus), c(2,1,3))[,90:1,]
  av_rsds.list[[i]] <- aperm(as.array(av_rsds), c(2,1,3))[,90:1,]
  av_rsus.list[[i]] <- aperm(as.array(av_rsus), c(2,1,3))[,90:1,]
  av_pr.list[[i]] <- aperm(as.array(av_pr), c(2,1,3))[,90:1,]
  av_lai.list[[i]] <- aperm(as.array(av_lai), c(2,1,3))[,90:1,]
  av_mrso.list[[i]] <- aperm(as.array(av_mrso), c(2,1,3))[,90:1,]
  dcorr_seascycle.list[[i]] <- aperm(as.array(dcorr_seascycle), c(2,1,3))[,90:1,]
  sd_anom_tas.list[[i]] <- aperm(as.array(sd_anom_tas), c(2,1,3))[,90:1,]
  sd_anom_mrso.list[[i]] <- aperm(as.array(sd_anom_mrso), c(2,1,3))[,90:1,]
  sd_anom_hfls.list[[i]] <- aperm(as.array(sd_anom_hfls), c(2,1,3))[,90:1,]
  save(dcorr.list, 
       corr_rgy_veg.list, 
       corr_wtr_veg.list, 
       dpcorr.list, 
       pcorr_rgy_veg.list, 
       pcorr_wtr_veg.list, 
       # dcorr_netrad.list,
       # corr_rgy_veg_netrad.list,
       # corr_wtr_veg_netrad.list,
       dcorr_rsds.list,
       corr_rgy_veg_rsds.list,
       corr_wtr_veg_rsds.list,
       av_tas.list, 
       av_mrso.list, 
       av_hfls.list,
       av_rlds.list,
       av_rlus.list,
       av_rsds.list,
       av_rsus.list,
       av_pr.list, 
       av_lai.list, 
       dcorr_seascycle.list, 
       sd_anom_tas.list,
       sd_anom_mrso.list,
       sd_anom_hfls.list,
       file = "testdir/202208_dcorr_cmip6_10yr_SWin.RData")
}


