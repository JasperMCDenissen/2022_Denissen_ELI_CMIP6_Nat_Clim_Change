# read, regrid and save treeFrac & cropFrac from CMIP6
# 2021-06-23
# by: Jasper Denissen

# this part reads, regrids and averages the crop/treeFrac data per 10yr block

##############################################################################################################################
##############################################################################################################################
######################## !!! Don't forget to reset the working directory to a directory of your choosing!!! ##################
##############################################################################################################################
##############################################################################################################################
setwd('/Net/Groups/BGI/work_3/HydroBioClim/archive/Denissen_etal_2022_Nat_Clim_Change/')

# make a data.frame containing all the data to read
source('Scripts/to_be_loaded_packages.R')
source('Scripts/functions/aggregate_raster.R')
data_to_read.df <- data.frame("source_id" = c(rep('ACCESS-ESM1-5',4),rep('CESM2',4),rep('CMCC-CM2-SR5',4),rep('CNRM-ESM2-1',4),rep('EC-Earth3-Veg',4),rep('INM-CM4-8',4)),
                              "Frac" = rep(c(rep('cropFrac',2),rep('treeFrac',2)),6),
                              "activity_id" = rep(c('historical','ssp585'),12),
                              "member_id" = c(rep('r1i1p1f1',4), rep('r4i1p1f1',4), rep('r1i1p1f1',4), rep('r1i1p1f2',4), rep('r2i1p1f1',4), rep('r1i1p1f1',4)))
path <- "Data/cmip6_202106_frac/"
# make data.frame with necessary source_id and member_id information
for(i in seq(1,24,4)){
  if(i == 1){
    cmip6_Fracdata.df <- data.frame("source_id" = data_to_read.df$source_id[i],
                                    "member_id" = data_to_read.df$member_id[i])
  }else{
    cmip6_Fracdata.df <- rbind(cmip6_Fracdata.df,
                               data.frame("source_id" = data_to_read.df$source_id[i],
                                          "member_id" = data_to_read.df$member_id[i]))
  }
}

# This loops over all crop/treeFrac, historical/ssp585 and member_id to
# - read in the data
# - spatial regridding
# - temporal regridding (decennial averages)
av_cropFrac.list <- av_treeFrac.list <- list()
model_count <- 1
for(i in 1:length(data_to_read.df$source_id)){
  files <- list.files(path,
                      pattern = glob2rx(paste0(data_to_read.df$Frac[i],"*",data_to_read.df$source_id[i],"*",data_to_read.df$activity_id[i],"*.nc")))
  # read in data
  if(data_to_read.df$activity_id[i] =='historical'){
    stack_iter <- stack(paste0(path,files)) # put the historical simulation in a raster stack
  }else if(data_to_read.df$activity_id[i] =='ssp585'){
    stack_iter <- stack(stack_iter, stack(paste0(path,files))) # stack ssp585 on top of that
    stack_iter <- stack_iter[[1573:3012]] # select years 1981-2100
    # spatial regridding
    target_brick <- brick(nl = 1440, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    res(target_brick) <- 2
    xmin(target_brick) <- 0
    xmax(target_brick) <- 360
    ymin(target_brick) <- -90
    ymax(target_brick) <- 90
    stack_agg <- agg_raster(stack_iter,target_brick)
    # for parallel computing
    UseCores <- 16
    beginCluster(n = UseCores)
    # temporal regridding (decennial averages)
    for(t in seq(1,1440,120)){ # so the loop moves 10 years and in the loop we read in 10 years of data.
      stack_10yr <- stack(stack_agg[[t:(t+119)]])
      av_stack_10yr <- clusterR(stack_10yr, calc, args = list(mean, na.rm=T), progress = 'text')
      if(t == 1){
        av_stack <- av_stack_10yr
      }else{
        av_stack <- stack(av_stack, av_stack_10yr)
      }
    }
    print(paste0(data_to_read.df$Frac[i], " for ", data_to_read.df$source_id[i], " is done..."))
    
    # end cluster
    endCluster()
    if(data_to_read.df$Frac[i] == 'cropFrac'){
      av_cropFrac.list[[model_count]] <- aperm(as.array(av_stack), c(2,1,3))[,90:1,]
    }else if(data_to_read.df$Frac[i] == 'treeFrac'){
      av_treeFrac.list[[model_count]] <- aperm(as.array(av_stack), c(2,1,3))[,90:1,]
      model_count <- model_count + 1
    }
  }
  save(cmip6_Fracdata.df, av_cropFrac.list, av_treeFrac.list,
       file = "testdir/202106_cmip6_frac.RData")
}
