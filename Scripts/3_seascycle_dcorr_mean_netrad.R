# # By: Jasper Denissen
# 2021-02-04
# Script to look at how trends in ERA5 reanalysis data change seasonally over time
# 1980-2019, 2.0x2.0 grid cell resolution WITH THE RASTER PACKAGE!
pdf(NULL)

##############################################################################################################################
##############################################################################################################################
######################## !!! Don't forget to reset the working directory to a directory of your choosing!!! ##################
##############################################################################################################################
##############################################################################################################################
setwd('/Net/Groups/BGI/work_3/HydroBioClim/archive/Denissen_etal_2022_Nat_Clim_Change/')

# Get the proper packages
source('Scripts/to_be_loaded_packages.R')

##########################################################################################
##########################################################################################
########################################## 2d00 ##########################################
##########################################################################################
##########################################################################################

# from /RData
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
load('RData/202112_dcorr_netrad_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
load('RData/total_land_area.RData')
# # from /testdir
# load('testdir/202106_cmip6_no_evspsblveg_hurs.RData')
# load('testdir/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
# load('testdir/total_land_area.RData')

# source functions
source('Scripts/functions/calc_boxes.R')
source('Scripts/functions/plot_discrete_cbar.R')

lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

for(i in 1:11){
  test <- array(NaN,c(180,90,12*12)); test[1:90,,] <- dcorr_seascycle.list[[i]][91:180,,]; test[91:180,,] <- dcorr_seascycle.list[[i]][1:90,,]; dcorr_seascycle.list[[i]] <- test; dcorr_seascycle.list[[i]] <- -1*dcorr_seascycle.list[[i]]
}

# select the right models and put in array
dcorr_netrad_all.array <- av_mrso.array <- array(NaN,c(180,90,11*12)) # 11 different source_id and 12*10 years
count_all <- 1
for(i in 1:11){
  dcorr_netrad_all.array[,,count_all:(count_all+11)] <- dcorr_netrad.list[[i]]
  av_mrso.array[,,count_all:(count_all+11)] <- av_mrso.list[[i]]
  count_all <- count_all + 12
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dcorr_netrad_all.array[91:180,,]; test[91:180,,] <- dcorr_netrad_all.array[1:90,,]; dcorr_netrad_all.array <- test; dcorr_netrad_all.array <- -1*dcorr_netrad_all.array
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_mrso.array[91:180,,]; test[91:180,,] <- av_mrso.array[1:90,,]; av_mrso.array <- test

ensemble_mean_dcorr_netrad_all <- 
  array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(models_with_full_timeseries[x,y])){
      if(models_with_full_timeseries[x,y] > 4){
        ensemble_mean_dcorr_netrad_all[x,y] <- mean(dcorr_netrad_all.array[x,y,], na.rm = T)
      }
    }
  }
}

# first calculate the month-of-year multimodel mean dcorr_netrad
mmmean_dcorr_seascycle <- array(NaN,c(180,90,12*12)) # for every 12 months-of-year and 12 10 year blocks
for(x in 1:180){ 
  for(y in 1:90){
    for(i in 1:144){ # loop over consequently 
      inter_dcorr_netrad <- c() # to collect models with same month and 10 yr block
      for(source_id in 1:11){
        inter_dcorr_netrad[source_id] <- dcorr_seascycle.list[[source_id]][x,y,i]
      }
      if(sum(!is.na(inter_dcorr_netrad)) > 4){
        mmmean_dcorr_seascycle[x,y,i] <- mean(inter_dcorr_netrad, na.rm = T)
      }
    }
  }
  print(paste0(round(x/180*100,2), "% is done..."))
}

# make a mask for complete time series per moy
index_moy <- seq(0,143,12) # for every 12 months-of-year and 12 10 year blocks
mask_cmip6 <- array(NaN,c(180,90,12))
for(i in 1:12){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(mmmean_dcorr_seascycle[x,y,(index_moy+i)])) == 12){ # check if this grid cell has a complete time series
        mask_cmip6[x,y,i] <- 1
      }
    }
  }
}

# from this we can infer the month of year trends
kendall_dcorr_seascycle <-
  array(NaN,c(180,90,12,2))
for(i in 1:12){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mask_cmip6[x,y,i])){
        kendall_dcorr_seascycle[x,y,i,] <- c(unname(kendallTrendTest(mmmean_dcorr_seascycle[x,y,(index_moy+i)])$estimate[2]), unname(kendallTrendTest(mmmean_dcorr_seascycle[x,y,(index_moy+i)])$p.value))
      }
    }
  }
  print(i)
}

# find out the number of water limited months per 10 year block per grid cell
sum_wtr_lim_months <-
  array(NaN,c(180,90,12))
for(i in 1:12){
  for(x in 1:180){
    for(y in 1:90){
      if(models_with_full_timeseries[x,y] > 4){
        if(sum(!is.na(mmmean_dcorr_seascycle[x,y,(index_moy[i]+1):((index_moy[i]+12))])) > 0){
          sum_wtr_lim_months[x,y,i] <- length(which(mmmean_dcorr_seascycle[x,y,(index_moy[i]+1):((index_moy[i]+12))] > 0))
        }
      }
    }
  }
  print(i)
}

# calculate how many months per 10 years the dry season increases in length with the kendall trend
trnd_in_wtr_lim_months <- array(NaN,c(180,90,2))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(sum_wtr_lim_months[x,y,])) == 12){
      trnd_in_wtr_lim_months[x,y,] <- c(kendallTrendTest(sum_wtr_lim_months[x,y,])$estimate[2], unname(kendallTrendTest(sum_wtr_lim_months[x,y,])$p.value))
    }
  }
}

# Find in which month the minimum slope occurs
kendall_month_min_trend <- 
  kendall_min_trend <- 
  kendall_max_abs_trend <- 
  kendall_month_max_abs_trend <- 
  kendall_most_sign_trend_p.value <-
  kendall_most_sign_trend <-
  kendall_month_most_sign_trend <-
  sum_months_with_trends <- array(NaN,c(180,90))
count_equal_slope <- count_equal_p.value <- 1
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(kendall_dcorr_seascycle[x,y,,1])) > 0){
      sum_months_with_trends[x,y] <- sum(!is.na(kendall_dcorr_seascycle[x,y,,1]))
      kendall_min_trend[x,y] <- min(kendall_dcorr_seascycle[x,y,,1], na.rm = T)
      kendall_max_abs_trend[x,y] <- max(abs(kendall_dcorr_seascycle[x,y,,1]), na.rm = T)
      kendall_month_min_trend[x,y] <- which(kendall_dcorr_seascycle[x,y,,1] == kendall_min_trend[x,y])[1] # if there are more months that have the exact same minimal slope, pick the first one
      kendall_month_max_abs_trend[x,y] <- which(abs(kendall_dcorr_seascycle[x,y,,1]) == kendall_max_abs_trend[x,y])[1] # if there are more months that have the exact same minimal slope, pick the first one
      if(length(which(kendall_dcorr_seascycle[x,y,,1] == kendall_max_abs_trend[x,y])) > 1){
        count_equal_slope <- count_equal_slope + 1
      }
      kendall_most_sign_trend_p.value[x,y] <- min(kendall_dcorr_seascycle[x,y,,2], na.rm = T)
      kendall_month_most_sign_trend[x,y] <- which(kendall_dcorr_seascycle[x,y,,2] == kendall_most_sign_trend_p.value[x,y])[1]
      if(length(which(kendall_dcorr_seascycle[x,y,,2] == kendall_most_sign_trend_p.value[x,y])) > 1){
        count_equal_p.value <- count_equal_p.value + 1
      }
      kendall_most_sign_trend[x,y] <- kendall_dcorr_seascycle[x,y,kendall_month_most_sign_trend[x,y],1]
    }
  }
}

kendall_month_min_trend_vec <- kendall_min_trend_vec <- kendall_most_sign_trend_vec <- kendall_month_most_sign_trend_vec <- 
  sum_months_with_trends_vec <- kendall_month_max_abs_trend_vec <- 
  trnd_in_wtr_lim_months_vec <- trnd_in_wtr_lim_months.p_vec <- 
  c()
count <- 1
for(x in 1:180){
  for(y in 1:90){
    kendall_month_min_trend_vec[count] <- kendall_month_min_trend[x,y]
    kendall_min_trend_vec[count] <- kendall_min_trend[x,y]
    kendall_month_max_abs_trend_vec[count] <- kendall_month_max_abs_trend[x,y]
    kendall_most_sign_trend_vec[count] <- kendall_most_sign_trend[x,y]
    kendall_month_most_sign_trend_vec[count] <- kendall_month_most_sign_trend[x,y]
    sum_months_with_trends_vec[count] <- sum_months_with_trends[x,y]
    trnd_in_wtr_lim_months_vec[count] <- trnd_in_wtr_lim_months[x,y,1]
    trnd_in_wtr_lim_months.p_vec[count] <- trnd_in_wtr_lim_months[x,y,2]
    count <- count + 1
  }
}

moy_trends.df <- data.frame("kendall_month_min_trend" = kendall_month_min_trend_vec,
                            "kendall_month_max_abs_trend" = kendall_month_max_abs_trend_vec,
                            "kendall_min_trend" = kendall_min_trend_vec,
                            "kendall_most_sign_trend" = kendall_most_sign_trend_vec,
                            "kendall_month_most_sign_trend" = kendall_month_most_sign_trend_vec,
                            "sum_months_with_trends" = sum_months_with_trends_vec,
                            "trnd_in_wtr_lim_months" = trnd_in_wtr_lim_months_vec*12,
                            "trnd_in_wtr_lim_months.p" = trnd_in_wtr_lim_months.p_vec,
                            "lon" = rep(rep(lon, each = 90),2),
                            "lat" = rep(rep(lat, 180),2))

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

# define hot spots regions
hotspot_regs.df <- data.frame("x" = c(lon[52], lon[62], lon[52], lon[52], lon[30], lon[58], lon[30], lon[30], lon[92], lon[100], lon[92], lon[92], lon[103], lon[138], lon[103], lon[103], lon[138], lon[152], lon[138], lon[138]),
                              "y" = c(lat[38], lat[38], lat[38], lat[48], lat[65], lat[65], lat[65], lat[77], lat[67], lat[67], lat[67], lat[72], lat[72], lat[72], lat[72], lat[77], lat[63], lat[63], lat[63], lat[57]),
                              "xend" = c(lon[52], lon[62], lon[62], lon[62], lon[30], lon[58], lon[58], lon[58], lon[92], lon[100], lon[100], lon[100], lon[103], lon[138], lon[138], lon[138], lon[138], lon[152], lon[152], lon[152]),
                              "yend" = c(lat[48], lat[48], lat[38], lat[48], lat[77], lat[77], lat[65], lat[77], lat[72], lat[72], lat[67], lat[72], lat[77], lat[77], lat[72], lat[77], lat[57], lat[57], lat[63], lat[57]))

myvalues <- seq(.5,12.5,1)
moy_trends.df$cuts_kendall_month_most_sign_trend <- cut(moy_trends.df$kendall_month_most_sign_trend, myvalues, include.lowest = T)
moy_trends.df$cuts_kendall_month_max_abs_trend <- cut(moy_trends.df$kendall_month_max_abs_trend, myvalues, include.lowest = T)
myvalues_trnd <- c(-Inf, c(-1,-.0001,.0001,1,2,3,4,5,6), Inf) # in months per 120 years
cols_trnd <- c(rev(brewer.pal(4, "Greens")[2:3]), 'snow2', brewer.pal(7, "YlOrBr")[1:7])

moy_trends.df$cuts_trnd_in_wtr_lim_months <- cut(moy_trends.df$trnd_in_wtr_lim_months, myvalues_trnd, include.lowest = T)

# maybe now per surface area instead of % of grid cells?
r <- raster()  # by default 1 by 1 degree
res(r) <- 2 # so change the resolution
a <- raster::area(r) # calculate the area of a 2x2 degree grid from N - S, as area varies only by latitude, not longitude
area <- a[,1]
area.array <- array(NaN,c(180,90))
for(x in 1:180){
  area.array[x,] <- area
}
100*sum(area.array[which(trnd_in_wtr_lim_months[,,1] > 0)])/total_land_area
100*sum(area.array[which(trnd_in_wtr_lim_months[,,1] < 0)])/total_land_area

dcorr_seascycle_reg.df <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                                   c("dcorr_netrad", "reg",
                                     "moy","center_moy","block","center_year"))
reg <- c("SAM","NAM", "CEU", "NEA", "EAS")
count_j <- 1
moy <- rep(seq(1,12),12)
center_moy <- rep(seq(1,12),12)+.5
block <- c(rep("1980-1990",12), rep("1990-2000",12), rep("2000-2010",12),
           rep("2010-2020",12), rep("2020-2030",12), rep("2030-2040",12),
           rep("2040-2050",12), rep("2050-2060",12), rep("2060-2070",12),
           rep("2070-2080",12), rep("2080-2090",12), rep("2090-2100",12))
center_year <- rep(seq(1985,2095,10),each=12)
for(j in seq(1,20,4)){ # loop over all regions
  lonmin <- min(hotspot_regs.df$x[j:(j+3)])
  lonmax <- max(hotspot_regs.df$x[j:(j+3)])
  latmin <- min(hotspot_regs.df$y[j:(j+3)])
  latmax <- max(hotspot_regs.df$y[j:(j+3)])
  for(t in 1:144){ # loop over all moy and blocks
    # only calculate multi-model mean fingerprint when at least half of the grid cells in the moy and decade have values.
    if(sum(!is.na(mmmean_dcorr_seascycle[which(lon >= lonmin & lon <= lonmax),
                                         which(lat >= latmin & lat <= latmax),t])) > .5*(dim(mmmean_dcorr_seascycle[which(lon >= lonmin & lon <= lonmax),
                                                                                                                    which(lat >= latmin & lat <= latmax),t])[1] * dim(mmmean_dcorr_seascycle[which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                                                             which(lat >= latmin & lat <= latmax),t])[2])){ 
      dcorr_seascycle_reg.df <- rbind(dcorr_seascycle_reg.df,
                                      data.frame("dcorr_netrad" = weighted.mean(x = mmmean_dcorr_seascycle[which(lon >= lonmin & lon <= lonmax),
                                                                                                    which(lat >= latmin & lat <= latmax),t],
                                                                         w = area.array[which(lon >= lonmin & lon <= lonmax),
                                                                                        which(lat >= latmin & lat <= latmax)],
                                                                         na.rm = T),
                                                 "reg" = reg[count_j],
                                                 "moy" = moy[t],
                                                 "center_moy" = center_moy[t],
                                                 "block" = block[t],
                                                 "center_year" = center_year[t]))
    
    }else{
      dcorr_seascycle_reg.df <- rbind(dcorr_seascycle_reg.df,
                                      data.frame("dcorr_netrad" = NaN,
                                                 "reg" = reg[count_j],
                                                 "moy" = moy[t],
                                                 "center_moy" = center_moy[t],
                                                 "block" = block[t],
                                                 "center_year" = center_year[t]))
    }
  }
  count_j <- count_j + 1
}

# dcorr_netradcol <- brewer.pal(9,"BrBG")
dcorr_netradcol <- c("[-Inf,-0.66]" = brewer.pal(9,"BrBG")[9],
              "(-0.66,-0.44]" = brewer.pal(9,"BrBG")[8],
              "(-0.44,-0.22]" = brewer.pal(9,"BrBG")[7],
              "(-0.22,-0.0001]" = brewer.pal(9,"BrBG")[6],
              "(-0.0001,0.0001]" = brewer.pal(9,"BrBG")[5],
              "(0.0001,0.22]" = brewer.pal(9,"BrBG")[4],
              "(0.22,0.44]" = brewer.pal(9,"BrBG")[3],
              "(0.44,0.66]" = brewer.pal(9,"BrBG")[2],
              "(0.66, Inf]" = brewer.pal(9,"BrBG")[1])

myvalues <- c(-Inf,-.66,-.44,-.22,-.0001,.0001,.22,.44,.66,Inf)
dcorr_seascycle_reg.df$cuts_dcorr_netrad <- cut(dcorr_seascycle_reg.df$dcorr_netrad, myvalues, include.lowest = T)

mmmean_fingerprintNAM <- ggplot(dcorr_seascycle_reg.df[which(dcorr_seascycle_reg.df$reg == reg[2]),], aes(x=center_year,y=moy,fill=cuts_dcorr_netrad)) + 
  geom_tile() + 
  scale_y_continuous("",breaks = seq(1,12),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand = c(0,0)) + 
  scale_x_continuous("",breaks = seq(1980,2100,30),
                     expand = c(0,0)) +
  scale_fill_manual(values = dcorr_netradcol, na.value = 'white') + # watch how many unique col classes there are in dcorr_netrad.df$cuts
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        plot.margin = unit(c(0,20,0,20), "pt"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_blank(),
        plot.title = element_text(size=22),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.text.x = element_text(angle = 0),
        axis.title.y.right = element_text(color = "grey80")
  ) + ggtitle(reg[2])
mmmean_fingerprintNAM

mmmean_fingerprintSAM <- ggplot(dcorr_seascycle_reg.df[which(dcorr_seascycle_reg.df$reg == reg[1]),], aes(x=center_year,y=moy,fill=cuts_dcorr_netrad)) + 
  geom_tile() + 
  scale_y_continuous("",breaks = seq(1,12),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand = c(0,0)) + 
  scale_x_continuous("",breaks = seq(1980,2100,30),
                     expand = c(0,0)) +
  scale_fill_manual(values = dcorr_netradcol, na.value = 'white') + # watch how many unique col classes there are in dcorr_netrad.df$cuts
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        plot.margin = unit(c(0,20,0,20), "pt"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_blank(),
        plot.title = element_text(size=22),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.text.x = element_text(angle = 0),
        axis.title.y.right = element_text(color = "grey80")
  ) + ggtitle(reg[1])
mmmean_fingerprintSAM

mmmean_fingerprintNEA <- ggplot(dcorr_seascycle_reg.df[which(dcorr_seascycle_reg.df$reg == reg[4]),], aes(x=center_year,y=moy,fill=cuts_dcorr_netrad)) + 
  geom_tile() + 
  scale_y_continuous("",breaks = seq(1,12),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand = c(0,0)) + 
  scale_x_continuous("",breaks = seq(1980,2100,30),
                     expand = c(0,0)) +
  scale_fill_manual(values = dcorr_netradcol, na.value = 'white') + # watch how many unique col classes there are in dcorr_netrad.df$cuts
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        plot.margin = unit(c(0,20,0,20), "pt"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_blank(),
        plot.title = element_text(size=22),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.text.x = element_text(angle = 0),
        axis.title.y.right = element_text(color = "grey80")
  ) + ggtitle(reg[4])
mmmean_fingerprintNEA

mmmean_fingerprintCEU <- ggplot(dcorr_seascycle_reg.df[which(dcorr_seascycle_reg.df$reg == reg[3]),], aes(x=center_year,y=moy,fill=cuts_dcorr_netrad)) + 
  geom_tile() + 
  scale_y_continuous("",breaks = seq(1,12),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand = c(0,0)) + 
  scale_x_continuous("",breaks = seq(1980,2100,30),
                     expand = c(0,0)) +
  scale_fill_manual(values = dcorr_netradcol, na.value = 'white') + # watch how many unique col classes there are in dcorr_netrad.df$cuts
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        plot.margin = unit(c(0,20,0,20), "pt"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_blank(),
        plot.title = element_text(size=22),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.text.x = element_text(angle = 0),
        axis.title.y.right = element_text(color = "grey80")
  ) + ggtitle(reg[3])
mmmean_fingerprintCEU

mmmean_fingerprintEAS <- ggplot(dcorr_seascycle_reg.df[which(dcorr_seascycle_reg.df$reg == reg[5]),], aes(x=center_year,y=moy,fill=cuts_dcorr_netrad)) + 
  geom_tile() + 
  scale_y_continuous("",breaks = seq(1,12),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand = c(0,0)) + 
  scale_x_continuous("",breaks = seq(1980,2100,30),
                     expand = c(0,0)) +
  scale_fill_manual(values = dcorr_netradcol, na.value = 'white') + # watch how many unique col classes there are in dcorr_netrad.df$cuts
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key.width = unit(1.5, "cm"),
        plot.margin = unit(c(0,20,0,20), "pt"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_blank(),
        plot.title = element_text(size=22),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.text.x = element_text(angle = 0),
        axis.title.y.right = element_text(color = "grey80")
  ) + ggtitle(reg[5])
mmmean_fingerprintEAS

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

# plot the discrete colorbar for mean dcorr_netrad
dbar <- plot_discrete_cbar(breaks = c(myvalues[1:4],0,myvalues[7:10]),
                           colors = c(unname(dcorr_netradcol[1:4]),unname(dcorr_netradcol[6:9])),
                           legend_title = "Ecosystem Water Limitation Index",
                           spacing = "constant",
                           font_size = 3,
                           spacing_scaling = 4,
                           width = .2,
                           triangle_size = .175,
                           legend_direction = "horizontal")

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- gridExtra::grid.arrange(empty, dbar, empty , ncol=3, widths = c(.5,5,.5))

count_wtr_lim_trnd.df <- data.frame("count" = c(length(which(moy_trends.df$trnd_in_wtr_lim_months < 0)), length(which(moy_trends.df$trnd_in_wtr_lim_months == 0)), length(which(moy_trends.df$trnd_in_wtr_lim_months > 0))),
                                    "area" = 100*c(sum(area.array[which(trnd_in_wtr_lim_months[,,1] < 0)])/total_land_area, sum(area.array[which(trnd_in_wtr_lim_months[,,1] == 0)])/total_land_area, sum(area.array[which(trnd_in_wtr_lim_months[,,1] > 0)])/total_land_area),
                                    "trend" = c("wettening","no change","drying"))

sign_wtr_lim_trnd.df <- data.frame("lon" = rep(lon, each = 90)[which(trnd_in_wtr_lim_months.p_vec < .05 & trnd_in_wtr_lim_months_vec != 0)],
                                   "lat" = rep(lat, 180)[which(trnd_in_wtr_lim_months.p_vec < .05 & trnd_in_wtr_lim_months_vec != 0)])

moy_trends.df$cuts_trnd_in_wtr_lim_months_newfac <- NaN
newfac <- c("< -1", "-1,0", "= 0","0,1", "1,2", "2,3", "3,4", "4,5","5,6", "> 6")
oldfac <- levels(moy_trends.df$cuts_trnd_in_wtr_lim_months)
count <- 1
for(i in 1:10){
  print(oldfac[i])
  moy_trends.df$cuts_trnd_in_wtr_lim_months_newfac[which(moy_trends.df$cuts_trnd_in_wtr_lim_months == oldfac[i])] <- newfac[i]
}
moy_trends.df$cuts_trnd_in_wtr_lim_months_newfac_fac <- factor(moy_trends.df$cuts_trnd_in_wtr_lim_months_newfac, levels = newfac)

names.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                     c("lon","lat","label"))
jump_four <- seq(1,20,4)
regs <- c("SAM","NAM","CEU","NEA","EAS")
for(i in 1:5){
  names.df <- rbind(names.df, data.frame("lon" = (min(hotspot_regs.df$x[jump_four[i]:(jump_four[i]+3)]) + max(hotspot_regs.df$x[jump_four[i]:(jump_four[i]+3)]))/2,
                                         "lat" = max(hotspot_regs.df$y[jump_four[i]:(jump_four[i]+3)]),
                                         "label" = regs[i]))
}

g <- ggplot(moy_trends.df[which(!is.na(moy_trends.df$cuts_trnd_in_wtr_lim_months)),], aes(x=lon,y=lat,fill=cuts_trnd_in_wtr_lim_months_newfac_fac)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = sign_wtr_lim_trnd.df, aes(x=lon,y=lat), size = .05) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_text(inherit.aes = F, data = names.df, aes(x=lon,y=lat+3.5,label=label), size = 5, col = 1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Trend in length of water-limited season (months/120yr)",
                    values = cols_trnd,
                    drop = F) + # watch how many unique col classes there are in dcorr_netrad.df$cuts
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, label.position = "bottom", nrow=1, byrow=TRUE,
                             label.theme = element_text(angle = 0))) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        plot.margin = unit(c(0,20,0,20), "pt"),
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
  )
g

rect_lightgrey <- data.frame(x = c(1),
                             colors = c("snow2"))

dbar_lightgrey <- ggplot(rect_lightgrey, aes(x, y = 0, fill = colors)) +
  geom_tile(width = .9, height = .3) + # make square tiles
  scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
  coord_fixed() + # make sure tiles are square
  ggtitle("no (significant) change") +
  theme_void() + # remove any axis markings
  theme(plot.title = element_text(size=18, hjust = 0.5, margin = margin(0, 0, .3, 0, "cm")),
        plot.margin = unit(c(.6,0,1.7,0),"cm"))

# plot the discrete colorbar for median dcorr_netrad
dbar <- plot_discrete_cbar(breaks = c(myvalues_trnd[1:2],0,myvalues_trnd[5:11]),
                           colors = c(cols_trnd[1:2],cols_trnd[4:10]),
                           legend_title = expression("Trend in length of water-limited season (months/120yr)"),
                           spacing = "constant",
                           font_size = 6,
                           spacing_scaling = 4,
                           width = .2,
                           triangle_size = .175,
                           legend_direction = "horizontal")

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar_lightgrey, empty, dbar, empty, ncol=5, widths = c(1,.4,.6,4,1))

plot <- grid.arrange(g,dbar_smaller, nrow = 2, heights = c(.8,.2))

dbar <- plot_discrete_cbar(breaks = c(myvalues[1:4],0,myvalues[7:10]),
                           colors = c(unname(dcorr_netradcol[1:4]),unname(dcorr_netradcol[6:9])),
                           legend_title = "Ecosystem Water Limitation Index",
                           spacing = "constant",
                           font_size = 4,
                           spacing_scaling = 4,
                           width = .2,
                           triangle_size = .175,
                           legend_direction = "horizontal")
plots1 <- grid.arrange(mmmean_fingerprintNAM, mmmean_fingerprintSAM, dbar, empty, heights = c(1/3,1/3,2/9,1/9))
plots2 <- grid.arrange(mmmean_fingerprintNEA, mmmean_fingerprintCEU, mmmean_fingerprintEAS, heights = c(1/3,1/3,1/3))
plots3 <- grid.arrange(plots1, plot, plots2,
                       widths = c(1/6,2/3,1/6))
# ggsave("testdir/Fig3.png", plot = plots3, width = 15*1.25, height = 7*1.25, units = "in")
ggsave("testdir/Fig3.eps", plot = plots3, width = 15*1.25, height = 7*1.25, units = "in", device = cairo_ps)
