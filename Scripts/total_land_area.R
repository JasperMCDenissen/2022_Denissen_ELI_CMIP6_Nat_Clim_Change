# 2022-01-21
# by: Jasper Denissen
# Calculating the warm land area from the ensemble of CMIP6 projections
pdf(NULL)

##############################################################################################################################
##############################################################################################################################
######################## !!! Don't forget to reset the working directory to a directory of your choosing!!! ##################
##############################################################################################################################
##############################################################################################################################
setwd('/Net/Groups/BGI/work_3/HydroBioClim/archive/Denissen_etal_2022_Nat_Clim_Change/')

# from /RData
load("RData/202208_dcorr_cmip6_10yr_SWin.RData")

# # from /testdir
# load("testdir/202208_dcorr_cmip6_10yr_SWin.RData")

# Get the proper packages
source('Scripts/to_be_loaded_packages.R')
lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# select the right models and put in array
dcorr_all.array <- 
  array(NaN,c(180,90,11*12)) # 14 different source_id and 14*10 years (total: 154)
count_all <- 1
for(i in 1:11){
  dcorr_all.array[,,count_all:(count_all+11)] <- dcorr.list[[i]]
  count_all <- count_all + 12
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dcorr_all.array[91:180,,]; test[91:180,,] <- dcorr_all.array[1:90,,]; dcorr_all.array <- test; dcorr_all.array <- -1*dcorr_all.array

models_with_full_timeseries <- 
  models_with_full_timeseries_mask<- array(0,c(180,90))
count_i <- 1
for(i in seq(1,(11*12),12)){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(dcorr_all.array[x,y,(i:(i+11))])) == 12){ # only calculate if the model has full time series
        models_with_full_timeseries[x,y] <- models_with_full_timeseries[x,y] + 1
        if(models_with_full_timeseries[x,y] > 4){
          models_with_full_timeseries_mask[x,y] <- 1
        }
      }
    }
  }
  print(i)
  count_i <- count_i + 1
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
total_land_area <- sum(models_with_full_timeseries_mask[,which(lat > -60 & lat < 70)]*area.array[,which(lat > -60 & lat < 70)], na.rm = T)

save(models_with_full_timeseries, total_land_area,
     file = "testdir/total_land_area.RData")
