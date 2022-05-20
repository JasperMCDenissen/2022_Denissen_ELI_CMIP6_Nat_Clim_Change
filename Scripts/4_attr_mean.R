# Plotting: x-axis trend T/SM/LE, y-axis prior dcorr and color-coded trend in correlation difference
# 01-02-2020
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

##########################################################################################
##########################################################################################
########################################## 2d00 ##########################################
##########################################################################################
##########################################################################################

# from /RData
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
load('RData/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
load('RData/total_land_area.RData')
# # from /testdir
# load('testdir/202106_cmip6_no_evspsblveg_hurs.RData')
# load('testdir/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
# load('testdir/total_land_area.RData')

lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# define hot spot regions
hotspot_regs.df <- data.frame("x" = c(lon[52], lon[62], lon[52], lon[52], lon[30], lon[58], lon[30], lon[30], lon[92], lon[100], lon[92], lon[92], lon[103], lon[138], lon[103], lon[103], lon[138], lon[152], lon[138], lon[138]),
                              "y" = c(lat[38], lat[38], lat[38], lat[48], lat[65], lat[65], lat[65], lat[77], lat[67], lat[67], lat[67], lat[72], lat[72], lat[72], lat[72], lat[77], lat[63], lat[63], lat[63], lat[57]),
                              "xend" = c(lon[52], lon[62], lon[62], lon[62], lon[30], lon[58], lon[58], lon[58], lon[92], lon[100], lon[100], lon[100], lon[103], lon[138], lon[138], lon[138], lon[138], lon[152], lon[152], lon[152]),
                              "yend" = c(lat[48], lat[48], lat[38], lat[48], lat[77], lat[77], lat[65], lat[77], lat[72], lat[72], lat[67], lat[72], lat[77], lat[77], lat[72], lat[77], lat[57], lat[57], lat[63], lat[57]))

# function
source('Scripts/functions/calc_boxes.R')
source('Scripts/functions/plot_discrete_cbar.R')


# select the right models and put in array
dcorr_netrad_all.array <- av_tas.array <-
  av_mrso.array <- av_hfls.array <-
  av_rlds.array <- av_rlus.array <-
  av_rsds.array <- av_rsus.array <-
  av_pr.array <- av_lai.array <-
  array(NaN,c(180,90,11*12)) # 14 different source_id and 14*10 years (total: 154)
count_all <- 1
for(i in 1:11){
  dcorr_netrad_all.array[,,count_all:(count_all+11)] <- dcorr_netrad.list[[i]]
  av_tas.array[,,count_all:(count_all+11)] <- av_tas.list[[i]]
  av_mrso.array[,,count_all:(count_all+11)] <- av_mrso.list[[i]]
  av_lai.array[,,count_all:(count_all+11)] <- av_lai.list[[i]]
  av_hfls.array[,,count_all:(count_all+11)] <- av_hfls.list[[i]]
  av_rlds.array[,,count_all:(count_all+11)] <- av_rlds.list[[i]]
  av_rlus.array[,,count_all:(count_all+11)] <- av_rlus.list[[i]]
  av_rsds.array[,,count_all:(count_all+11)] <- av_rsds.list[[i]]
  av_rsus.array[,,count_all:(count_all+11)] <- av_rsus.list[[i]]
  av_pr.array[,,count_all:(count_all+11)] <- av_pr.list[[i]]
  count_all <- count_all + 12
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dcorr_netrad_all.array[91:180,,]; test[91:180,,] <- dcorr_netrad_all.array[1:90,,]; dcorr_netrad_all.array <- test; dcorr_netrad_all.array <- -1*dcorr_netrad_all.array
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_tas.array[91:180,,]; test[91:180,,] <- av_tas.array[1:90,,]; av_tas.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_mrso.array[91:180,,]; test[91:180,,] <- av_mrso.array[1:90,,]; av_mrso.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_hfls.array[91:180,,]; test[91:180,,] <- av_hfls.array[1:90,,]; av_hfls.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rlds.array[91:180,,]; test[91:180,,] <- av_rlds.array[1:90,,]; av_rlds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rlus.array[91:180,,]; test[91:180,,] <- av_rlus.array[1:90,,]; av_rlus.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rsds.array[91:180,,]; test[91:180,,] <- av_rsds.array[1:90,,]; av_rsds.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_rsus.array[91:180,,]; test[91:180,,] <- av_rsus.array[1:90,,]; av_rsus.array <- test
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- av_pr.array[91:180,,]; test[91:180,,] <- av_pr.array[1:90,,]; av_pr.array <- test
av_rnet.array <- ((av_rlds.array - av_rlus.array) + (av_rsds.array - av_rsus.array))/26.15741 # in mm
# rho <- 1000 #kg/m3
# l_v <- 2.26 #MJ/kg
# # W/m2 = 1000 (kg/m3) * 2.26*10^6 (J/kg) * 1 mm/day*(1/(24*60^2))(day/s) * (1/1000)(mm/m)

# conversion factor 1mm = 28.35648 Wm-2
av_ar.array <- av_rnet.array/(av_pr.array*24*60*60)

# maybe now per surface area instead of % of grid cells?
r <- raster()  # by default 1 by 1 degree
res(r) <- 2 # so change the resolution
a <- raster::area(r) # calculate the area of a 2x2 degree grid from N - S, as area varies only by latitude, not longitude
area <- a[,1]
area.array <- array(NaN,c(180,90))
for(x in 1:180){
  area.array[x,] <- area
}

# make a mask from the observations
mask_obs <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(dcorr_netrad_all.array[x,y,])) == 11*12){ # check if all the models have a value there
      mask_obs[x,y] <- 1
    }
  }
}

mmmean_dcorr_netrad_per_10yr <- mmmean_tas_per_10yr <- mmmean_netrad_per_10yr <-
  mmmean_mrso_per_10yr <- mmmean_hfls_per_10yr <-
  mmmean_lai_per_10yr <- mmmean_ar_per_10yr <-
  array(NaN,c(180,90,12))
index_per_10yr <- seq(0,131,12)
for(i in 1:12){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(models_with_full_timeseries[x,y])){
        if(models_with_full_timeseries[x,y] > 4){
          mmmean_dcorr_netrad_per_10yr[x,y,i] <- mean(dcorr_netrad_all.array[x,y,(index_per_10yr+i)],na.rm=T)
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

mmmean_kendall_dcorr_netrad <- mmmean_kendall_tas <- mmmean_kendall_netrad <- mmmean_kendall_hfls <-
  mmmean_kendall_mrso <- mmmean_kendall_lai <- mmmean_kendall_ar <- array(NaN,c(180,90,2)) # 2: slope (all) & p.value (all)
sum_blocks <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_netrad_per_10yr[x,y,])) == 12){
      sum_blocks[x,y] <- sum(!is.na(mmmean_dcorr_netrad_per_10yr[x,y,]))
      mmmean_kendall_dcorr_netrad[x,y,] <- c(unname(kendallTrendTest(mmmean_dcorr_netrad_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_dcorr_netrad_per_10yr[x,y,])$p.value))
      mmmean_kendall_tas[x,y,] <- c(unname(kendallTrendTest(mmmean_tas_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_tas_per_10yr[x,y,])$p.value))
      mmmean_kendall_netrad[x,y,] <- c(unname(kendallTrendTest(mmmean_netrad_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_netrad_per_10yr[x,y,])$p.value))
      mmmean_kendall_mrso[x,y,] <- c(unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$p.value))
      mmmean_kendall_hfls[x,y,] <- c(unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$p.value))
      mmmean_kendall_lai[x,y,] <- c(unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$p.value))
      mmmean_kendall_ar[x,y,] <- c(unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$p.value))
    }
  }
}

# dredge + calc.relimp()
# https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15385
options(na.action = "na.fail")
vars <- c("netrad","mrso","hfls","lai","ar","adj. R2 < 0.5")
dom_var_dredge_relimp <- array(NaN,c(180,90,2))
dom_var_dredge_relimp_all <- array(0,c(180,90,5))
adj_R2_dredge_relimp <- array(NaN,c(180,90))
lin_mod.array <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_netrad_per_10yr[x,y,])) == 12 & sum(!is.na(mmmean_lai_per_10yr[x,y,])) == 12 & round(sd(mmmean_lai_per_10yr[x,y,]),4) != 0 & round(sd(mmmean_mrso_per_10yr[x,y,]),4) != 0){ # there should be variability in mrso and lai, otherwise there is no explanatory power in their respective time series
      relimp_variables.list <- list()
      rsq.vec <- c()
      # dredge: automated model selection
      lm_dcorr_netrad.df <- lm(mmmean_dcorr_netrad_per_10yr[x,y,] ~
                          mmmean_netrad_per_10yr[x,y,] + mmmean_mrso_per_10yr[x,y,] + mmmean_hfls_per_10yr[x,y,] +
                          mmmean_lai_per_10yr[x,y,] + mmmean_ar_per_10yr[x,y,])
      dd <- dredge(lm_dcorr_netrad.df, extra = list(AIC))

      # IMPORTANCE: pick a subset of best models (all have same performance/complexity in terms of AIC)
      dd_subset <- subset(dd, delta < 4)
      relimp_var_lmg.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                    c("var","lmg","weight"))
      total_var_expl.c <- c()
      for(lin_mod in 1:dim(dd_subset)[1]){ # loop over all models of good enough quality
        rsq.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$r.squared
        # p.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$coefficients
        if(rsq.vec[lin_mod] > .5){ # double check whether their adjusted r squared are half decent
          print(lin_mod)
          if(length(names(summary(get.models(dd_subset, lin_mod)[[1]])$aliased)) - 1 > 1){ # check if model has multiple variables
            relimp <- calc.relimp(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:5){
              if(sum(grepl(vars[i], names(relimp@lmg))) == 1){ # extract all variables and explained variance per variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = unname(relimp$lmg[which(grepl(vars[i], names(relimp@lmg)) == T)]), # explained variance per variable
                                                      "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                dom_var_dredge_relimp_all[x,y,i] <- dom_var_dredge_relimp_all[x,y,i] + 1
              }
            }
          }else{ # if the model has only one variable
            expl_dcorr_netrad_dredge.df <- summary(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:5){
              if(grepl(vars[i],names(expl_dcorr_netrad_dredge.df$aliased)[2])){ # if there are only two explanatory variables left in the model, the first one is the intercept and the second one is the variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = expl_dcorr_netrad_dredge.df$r.squared, # explained variance for dominant variable
                                                      "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                dom_var_dredge_relimp_all[x,y,i] <- dom_var_dredge_relimp_all[x,y,i] + 1
                next
              }
            }
          }
        }
        if(lin_mod == dim(dd_subset)[1]){ # on the last model
          lin_mod.array[x,y] <- lin_mod
          adj_R2_dredge_relimp[x,y] <- mean(rsq.vec, na.rm = T)
          for(j in 1:5){
            total_var_expl.c[j] <- weighted.mean(x = relimp_var_lmg.df$lmg[which(relimp_var_lmg.df$var == j)], w = relimp_var_lmg.df$weight[which(relimp_var_lmg.df$var == j)], na.rm = T)
          }
          if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 1){ # if there is one dominant variable
            dom_var_dredge_relimp[x,y,1] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 2){ # if there is a two way tie
            dom_var_dredge_relimp[x,y,] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }
        }
      }


    }
  }
  print(paste(round(x/(180)*100,2),"%"))
}

weighted.mean(adj_R2_dredge_relimp, area.array, na.rm=T)

# insert '6' where there is warm land area and adj R2 < 0.5
for(x in 1:180){
  for(y in 1:90){
    if(models_with_full_timeseries[x,y] > 4 & is.na(dom_var_dredge_relimp[x,y,1])){
      dom_var_dredge_relimp[x,y,1] <- 6
    }
  }
}


how_many_vars_relimp <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    how_many_vars_relimp[x,y] <- length(which(dom_var_dredge_relimp_all[x,y,] != 0))
  }
}
how_many_vars_relimp[which(how_many_vars_relimp == 0)] <- NaN

dom_var_ts.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                          c("dom_var_dredge_relimp","lon","lat","number_of_expl_var"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(dom_var_dredge_relimp[x,y,1])){
      dom_var_ts.df <- rbind(dom_var_ts.df,
                             data.frame("dom_var_dredge_relimp" = dom_var_dredge_relimp[x,y,1],
                                        "lon" = lon[x],
                                        "lat" = lat[y],
                                        "number_of_expl_var" = how_many_vars_relimp[x,y]
                             ))
    }
  }
}

cols_dom_var <- c(brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(11,"BrBG")[3], 'snow2')
myvalues_dom_var <- seq(0.5,6.5,1)

dom_var_ts.df$cuts_dom_var_dredge_relimp <- cut(dom_var_ts.df$dom_var_dredge_relimp, myvalues_dom_var, include.lowest = T)

# the mean dcorr_netrad hist plot
count_dom_var_dredge_relimp.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                           c("count","area","var"))
for(i in 1:5){
  count_dom_var_dredge_relimp.df <- rbind(count_dom_var_dredge_relimp.df,
                                          data.frame("count" = length(which(dom_var_ts.df$dom_var_dredge_relimp == i)),
                                                     "area" = 100*(sum(area.array[which(dom_var_dredge_relimp == i)])/total_land_area),
                                                     "var" = vars[i]))
}


count_dom_var_dredge_relimp.df$var_factor <- factor(count_dom_var_dredge_relimp.df$var, levels = vars[1:5])

bar_dom_var_dredge_relimp <- ggplot(count_dom_var_dredge_relimp.df, aes(x = var_factor, y = area, fill = var_factor)) +
  geom_bar(stat='identity',col = 'black', width = .5) +
  geom_text(inherit.aes = F, data = count_dom_var_dredge_relimp.df, aes(x=var_factor,y=area-.5, fill = var_factor,label=round(area,0)), size = 2.5) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), position = "right") +
  scale_fill_manual(values = cols_dom_var,
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
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size=10)
  )
bar_dom_var_dredge_relimp
bar_dom_var_dredge_relimp <- ggplotGrob(bar_dom_var_dredge_relimp)
plot_bar_dom_var_dredge_relimp <- bar_dom_var_dredge_relimp

count_dom_var_dredge_relimp_reg.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                        c("reg","count","area","var"))
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
  total_sign_land_area_reg <- sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                             which(lat >= latmin & lat <= latmax)][which(!is.na(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                           which(lat >= latmin & lat <= latmax)]) & dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                                                               which(lat >= latmin & lat <= latmax)] != 6)])
  for(i in 1:5){
    count_dom_var_dredge_relimp_reg.df <- rbind(count_dom_var_dredge_relimp_reg.df,
                                                data.frame("reg" = reg[count_j],
                                                           "count" = length(which(dom_var_ts.df$dom_var_dredge_relimp[which(dom_var_ts.df$lon >= lonmin &
                                                                                                                              dom_var_ts.df$lon <= lonmax &
                                                                                                                              dom_var_ts.df$lat >= latmin &
                                                                                                                              dom_var_ts.df$lat <= latmax)] == i)),
                                                           "area" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                                        which(lat >= latmin & lat <= latmax)][which(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                               which(lat >= latmin & lat <= latmax)] == i)])/total_land_area_reg),
                                                           "sign_area" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                                             which(lat >= latmin & lat <= latmax)][which(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                                    which(lat >= latmin & lat <= latmax)] == i)])/total_sign_land_area_reg),
                                                           "var" = vars[i]))
  }
  count_j <- count_j + 1
}

count_dom_var_dredge_relimp_reg.df$var_factor <- factor(count_dom_var_dredge_relimp_reg.df$var, levels = vars[1:5])
count_dom_var_dredge_relimp_reg.df$reg_factor <- factor(count_dom_var_dredge_relimp_reg.df$reg, levels = reg)


bar_dom_var_dredge_relimp <- ggplot(count_dom_var_dredge_relimp_reg.df, aes(x = reg_factor, y = sign_area, fill = var_factor, label=round(sign_area,2))) +
  geom_bar(stat='identity', position = position_dodge(width = NULL), col = 'black', width = .8) +
  geom_text(size = 2.5, position = position_dodge(width=.8), vjust = +1) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-% per region")), expand = c(0,0)) +
  scale_fill_manual("",labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index"),
                    values = cols_dom_var[1:5],
                    drop = F) +
  theme(legend.position = c(.45,.85),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  )
bar_dom_var_dredge_relimp

ggsave("testdir/SFig17.png", plot = bar_dom_var_dredge_relimp, width = 9, height = 7, units = "in")

bar_dom_var_dredge_relimp_netrad_per_reg <- ggplot(count_dom_var_dredge_relimp_reg.df[which(count_dom_var_dredge_relimp_reg.df$var == 'netrad'),], aes(x = reg_factor, y = sign_area, fill = var_factor, label=round(sign_area,0))) +
  geom_bar(stat='identity', position = position_dodge(width = NULL), col = 'black', width = .5) +
  geom_text(size = 2.5, position = position_dodge(width=.8), vjust = +1) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-% per region")), expand = c(0,0)) +
  scale_fill_manual("",labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index"),
                    values = cols_dom_var,
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
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_dom_var_dredge_relimp_netrad_per_reg
bar_dom_var_dredge_relimp_netrad_per_reg <- ggplotGrob(bar_dom_var_dredge_relimp_netrad_per_reg)
plot_bar_dom_var_dredge_relimp_netrad_per_reg <- bar_dom_var_dredge_relimp_netrad_per_reg

single_var.df <- dom_var_ts.df[which(dom_var_ts.df$number_of_expl_var == 1),]

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

a <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = single_var.df, aes(x=lon,y=lat), size = .5) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var,
                    labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index",expression("adjusted R"^2*" < 0.5")),
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  )
a
a <- ggplotGrob(a)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, a, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp_netrad_per_reg, t = 26, l = 3, b = 18, r = 8)
gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp, t = 16, l = 3, b = 10, r = 8)
grid.draw(gt1)

# how much % actually has a good prediction?
sum(area.array[which(!is.na(dom_var_dredge_relimp[,,1]) & dom_var_dredge_relimp[,,1] != 6)], na.rm = T) / total_land_area

# ggsave("testdir/Fig4.png", plot = gt1, width = 9, height = 7, units = "in") 
ggsave("testdir/Fig4.eps", plot = gt1, width = 9, height = 7, units = "in", device = cairo_ps) 

# adjusted R2 > 0.7

options(na.action = "na.fail")
vars <- c("netrad","mrso","hfls","lai","ar","adj. R2 < 0.7")
# dredge + calc.relimp()
# https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15385
dom_var_dredge_relimp <- array(NaN,c(180,90,2))
dom_var_dredge_relimp_all <- array(0,c(180,90,5))
adj_R2_dredge_relimp <- array(NaN,c(180,90))
lin_mod.array <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_netrad_per_10yr[x,y,])) == 12 & sum(!is.na(mmmean_lai_per_10yr[x,y,])) == 12 & round(sd(mmmean_lai_per_10yr[x,y,]),4) != 0 & round(sd(mmmean_mrso_per_10yr[x,y,]),4) != 0){ # there should be variability in mrso and lai, otherwise there is no explanatory power in their respective time series
      relimp_variables.list <- list()
      rsq.vec <- c()
      # dredge: automated model selection
      lm_dcorr_netrad.df <- lm(mmmean_dcorr_netrad_per_10yr[x,y,] ~
                          mmmean_netrad_per_10yr[x,y,] + mmmean_mrso_per_10yr[x,y,] + mmmean_hfls_per_10yr[x,y,] +
                          mmmean_lai_per_10yr[x,y,] + mmmean_ar_per_10yr[x,y,])
      dd <- dredge(lm_dcorr_netrad.df, extra = list(AIC))
      
      # IMPORTANCE: pick a subset of best models (all have same performance/complexity in terms of AIC)
      dd_subset <- subset(dd, delta < 4)
      relimp_var_lmg.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                    c("var","lmg","weight"))
      total_var_expl.c <- c()
      for(lin_mod in 1:dim(dd_subset)[1]){ # loop over all models of good enough quality
        rsq.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$r.squared
        if(rsq.vec[lin_mod] > .7){ # double check whether their adjusted r squared are half decent
          print(lin_mod)
          if(length(names(summary(get.models(dd_subset, lin_mod)[[1]])$aliased)) - 1 > 1){ # check if model has multiple variables
            relimp <- calc.relimp(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:5){
              if(sum(grepl(vars[i], names(relimp@lmg))) == 1){ # extract all variables and explained variance per variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = unname(relimp$lmg[which(grepl(vars[i], names(relimp@lmg)) == T)]), # explained variance per variable
                                                      "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                dom_var_dredge_relimp_all[x,y,i] <- dom_var_dredge_relimp_all[x,y,i] + 1
              }
            }
          }else{ # if the model has only one variable
            expl_dcorr_netrad_dredge.df <- summary(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:5){
              if(grepl(vars[i],names(expl_dcorr_netrad_dredge.df$aliased)[2])){ # if there are only two explanatory variables left in the model, the first one is the intercept and the second one is the variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = expl_dcorr_netrad_dredge.df$r.squared, # explained variance for dominant variable
                                                      "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                dom_var_dredge_relimp_all[x,y,i] <- dom_var_dredge_relimp_all[x,y,i] + 1
                next
              }
            }
          }
        }
        if(lin_mod == dim(dd_subset)[1]){ # on the last model
          lin_mod.array[x,y] <- lin_mod
          adj_R2_dredge_relimp[x,y] <- mean(rsq.vec, na.rm = T)
          for(j in 1:5){
            total_var_expl.c[j] <- weighted.mean(x = relimp_var_lmg.df$lmg[which(relimp_var_lmg.df$var == j)], w = relimp_var_lmg.df$weight[which(relimp_var_lmg.df$var == j)], na.rm = T)
          }
          if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 1){ # if there is one dominant variable
            dom_var_dredge_relimp[x,y,1] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 2){ # if there is a two way tie
            dom_var_dredge_relimp[x,y,] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }
        }
      }
      
      
    }
  }
  print(paste(round(x/(180)*100,2),"%"))
}

weighted.mean(adj_R2_dredge_relimp, area.array, na.rm=T)

# insert '6' where there is warm land area and adj R2 < 0.5
for(x in 1:180){
  for(y in 1:90){
    if(models_with_full_timeseries[x,y] > 4 & is.na(dom_var_dredge_relimp[x,y,1])){
      dom_var_dredge_relimp[x,y,1] <- 6
    }
  }
}


how_many_vars_relimp <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    how_many_vars_relimp[x,y] <- length(which(dom_var_dredge_relimp_all[x,y,] != 0))
  }
}
how_many_vars_relimp[which(how_many_vars_relimp == 0)] <- NaN

dom_var_ts.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                          c("dom_var_dredge_relimp","lon","lat","number_of_expl_var"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(dom_var_dredge_relimp[x,y,1])){
      dom_var_ts.df <- rbind(dom_var_ts.df, 
                             data.frame("dom_var_dredge_relimp" = dom_var_dredge_relimp[x,y,1],
                                        "lon" = lon[x],
                                        "lat" = lat[y],
                                        "number_of_expl_var" = how_many_vars_relimp[x,y]
                             ))
    }
  }
}

cols_dom_var <- c(brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(11,"BrBG")[3], 'snow2')
myvalues_dom_var <- seq(0.5,6.5,1)

dom_var_ts.df$cuts_dom_var_dredge_relimp <- cut(dom_var_ts.df$dom_var_dredge_relimp, myvalues_dom_var, include.lowest = T)

# the mean dcorr_netrad hist plot
count_dom_var_dredge_relimp.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                           c("count","area","var"))
for(i in 1:5){
  count_dom_var_dredge_relimp.df <- rbind(count_dom_var_dredge_relimp.df,
                                          data.frame("count" = length(which(dom_var_ts.df$dom_var_dredge_relimp == i)),
                                                     "area" = 100*(sum(area.array[which(dom_var_dredge_relimp == i)])/total_land_area),
                                                     "var" = vars[i]))
}


count_dom_var_dredge_relimp.df$var_factor <- factor(count_dom_var_dredge_relimp.df$var, levels = vars[1:5])

bar_dom_var_dredge_relimp <- ggplot(count_dom_var_dredge_relimp.df, aes(x = var_factor, y = area, fill = var_factor)) +
  geom_bar(stat='identity',col = 'black', width = .5) +
  geom_text(inherit.aes = F, data = count_dom_var_dredge_relimp.df, aes(x=var_factor,y=area-.5, fill = var_factor,label=round(area,0)), size = 2.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), position = "right") +
  scale_fill_manual(values = cols_dom_var,
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
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size=10)
  )
bar_dom_var_dredge_relimp
bar_dom_var_dredge_relimp <- ggplotGrob(bar_dom_var_dredge_relimp)
plot_bar_dom_var_dredge_relimp <- bar_dom_var_dredge_relimp

count_dom_var_dredge_relimp_reg.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                               c("reg","count","area","var"))
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
  total_sign_land_area_reg <- sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                             which(lat >= latmin & lat <= latmax)][which(!is.na(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                           which(lat >= latmin & lat <= latmax)]) & dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                                                               which(lat >= latmin & lat <= latmax)] != 6)])
  for(i in 1:5){
    count_dom_var_dredge_relimp_reg.df <- rbind(count_dom_var_dredge_relimp_reg.df,
                                                data.frame("reg" = reg[count_j],
                                                           "count" = length(which(dom_var_ts.df$dom_var_dredge_relimp[which(dom_var_ts.df$lon >= lonmin &
                                                                                                                              dom_var_ts.df$lon <= lonmax &
                                                                                                                              dom_var_ts.df$lat >= latmin &
                                                                                                                              dom_var_ts.df$lat <= latmax)] == i)),
                                                           "area" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                                        which(lat >= latmin & lat <= latmax)][which(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                               which(lat >= latmin & lat <= latmax)] == i)])/total_land_area_reg),
                                                           "sign_area" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                                             which(lat >= latmin & lat <= latmax)][which(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                                    which(lat >= latmin & lat <= latmax)] == i)])/total_sign_land_area_reg),
                                                           "var" = vars[i]))
  }
  count_j <- count_j + 1
}

count_dom_var_dredge_relimp_reg.df$var_factor <- factor(count_dom_var_dredge_relimp_reg.df$var, levels = vars[1:5])
count_dom_var_dredge_relimp_reg.df$reg_factor <- factor(count_dom_var_dredge_relimp_reg.df$reg, levels = reg)

bar_dom_var_dredge_relimp_netrad_per_reg <- ggplot(count_dom_var_dredge_relimp_reg.df[which(count_dom_var_dredge_relimp_reg.df$var == 'netrad'),], aes(x = reg_factor, y = sign_area, fill = var_factor, label=round(sign_area,0))) +
  geom_bar(stat='identity', position = position_dodge(width = NULL), col = 'black', width = .5) +
  geom_text(size = 2.5, position = position_dodge(width=.8), vjust = +1) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-% per region")), expand = c(0,0)) +
  scale_fill_manual("",labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index"),
                    values = cols_dom_var,
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
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_dom_var_dredge_relimp_netrad_per_reg
bar_dom_var_dredge_relimp_netrad_per_reg <- ggplotGrob(bar_dom_var_dredge_relimp_netrad_per_reg)
plot_bar_dom_var_dredge_relimp_netrad_per_reg <- bar_dom_var_dredge_relimp_netrad_per_reg

single_var.df <- dom_var_ts.df[which(dom_var_ts.df$number_of_expl_var == 1),]

a <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = single_var.df, aes(x=lon,y=lat), size = .5) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var, 
                    labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index",expression("adjusted R"^2*" < 0.7")),
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("a)")
a
a <- ggplotGrob(a)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, a, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp_netrad_per_reg, t = 26, l = 3, b = 18, r = 8)
gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp, t = 16, l = 3, b = 10, r = 8)
grid.draw(gt1)

gt_relimp0.7 <- gt1

# adjusted R2 > 0.3

options(na.action = "na.fail")
vars <- c("netrad","mrso","hfls","lai","ar","adj. R2 < 0.3")
# dredge + calc.relimp()
# https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15385
dom_var_dredge_relimp <- array(NaN,c(180,90,2))
dom_var_dredge_relimp_all <- array(0,c(180,90,5))
adj_R2_dredge_relimp <- array(NaN,c(180,90))
lin_mod.array <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_netrad_per_10yr[x,y,])) == 12 & sum(!is.na(mmmean_lai_per_10yr[x,y,])) == 12 & round(sd(mmmean_lai_per_10yr[x,y,]),4) != 0 & round(sd(mmmean_mrso_per_10yr[x,y,]),4) != 0){ # there should be variability in mrso and lai, otherwise there is no explanatory power in their respective time series
      relimp_variables.list <- list()
      rsq.vec <- c()
      # dredge: automated model selection
      lm_dcorr_netrad.df <- lm(mmmean_dcorr_netrad_per_10yr[x,y,] ~
                          mmmean_netrad_per_10yr[x,y,] + mmmean_mrso_per_10yr[x,y,] + mmmean_hfls_per_10yr[x,y,] +
                          mmmean_lai_per_10yr[x,y,] + mmmean_ar_per_10yr[x,y,])
      dd <- dredge(lm_dcorr_netrad.df, extra = list(AIC))
      
      # IMPORTANCE: pick a subset of best models (all have same performance/complexity in terms of AIC)
      dd_subset <- subset(dd, delta < 4)
      relimp_var_lmg.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                    c("var","lmg","weight"))
      total_var_expl.c <- c()
      for(lin_mod in 1:dim(dd_subset)[1]){ # loop over all models of good enough quality
        rsq.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$r.squared
        if(rsq.vec[lin_mod] > .3){ # double check whether their adjusted r squared are half decent
          print(lin_mod)
          if(length(names(summary(get.models(dd_subset, lin_mod)[[1]])$aliased)) - 1 > 1){ # check if model has multiple variables
            relimp <- calc.relimp(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:5){
              if(sum(grepl(vars[i], names(relimp@lmg))) == 1){ # extract all variables and explained variance per variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = unname(relimp$lmg[which(grepl(vars[i], names(relimp@lmg)) == T)]), # explained variance per variable
                                                      "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                dom_var_dredge_relimp_all[x,y,i] <- dom_var_dredge_relimp_all[x,y,i] + 1
              }
            }
          }else{ # if the model has only one variable
            expl_dcorr_netrad_dredge.df <- summary(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:5){
              if(grepl(vars[i],names(expl_dcorr_netrad_dredge.df$aliased)[2])){ # if there are only two explanatory variables left in the model, the first one is the intercept and the second one is the variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = expl_dcorr_netrad_dredge.df$r.squared, # explained variance for dominant variable
                                                      "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                dom_var_dredge_relimp_all[x,y,i] <- dom_var_dredge_relimp_all[x,y,i] + 1
                next
              }
            }
          }
        }
        if(lin_mod == dim(dd_subset)[1]){ # on the last model
          lin_mod.array[x,y] <- lin_mod
          adj_R2_dredge_relimp[x,y] <- mean(rsq.vec, na.rm = T)
          for(j in 1:5){
            total_var_expl.c[j] <- weighted.mean(x = relimp_var_lmg.df$lmg[which(relimp_var_lmg.df$var == j)], w = relimp_var_lmg.df$weight[which(relimp_var_lmg.df$var == j)], na.rm = T)
          }
          if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 1){ # if there is one dominant variable
            dom_var_dredge_relimp[x,y,1] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 2){ # if there is a two way tie
            dom_var_dredge_relimp[x,y,] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }
        }
      }
      
      
    }
  }
  print(paste(round(x/(180)*100,2),"%"))
}

weighted.mean(adj_R2_dredge_relimp, area.array, na.rm=T)

# insert '6' where there is warm land area and adj R2 < 0.5
for(x in 1:180){
  for(y in 1:90){
    if(models_with_full_timeseries[x,y] > 4 & is.na(dom_var_dredge_relimp[x,y,1])){
      dom_var_dredge_relimp[x,y,1] <- 6
    }
  }
}


how_many_vars_relimp <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    how_many_vars_relimp[x,y] <- length(which(dom_var_dredge_relimp_all[x,y,] != 0))
  }
}
how_many_vars_relimp[which(how_many_vars_relimp == 0)] <- NaN

dom_var_ts.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                          c("dom_var_dredge_relimp","lon","lat","number_of_expl_var"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(dom_var_dredge_relimp[x,y,1])){
      dom_var_ts.df <- rbind(dom_var_ts.df, 
                             data.frame("dom_var_dredge_relimp" = dom_var_dredge_relimp[x,y,1],
                                        "lon" = lon[x],
                                        "lat" = lat[y],
                                        "number_of_expl_var" = how_many_vars_relimp[x,y]
                             ))
    }
  }
}

cols_dom_var <- c(brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(11,"BrBG")[3], 'snow2')
myvalues_dom_var <- seq(0.5,6.5,1)

dom_var_ts.df$cuts_dom_var_dredge_relimp <- cut(dom_var_ts.df$dom_var_dredge_relimp, myvalues_dom_var, include.lowest = T)

# the mean dcorr_netrad hist plot
count_dom_var_dredge_relimp.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                           c("count","area","var"))
for(i in 1:5){
  count_dom_var_dredge_relimp.df <- rbind(count_dom_var_dredge_relimp.df,
                                          data.frame("count" = length(which(dom_var_ts.df$dom_var_dredge_relimp == i)),
                                                     "area" = 100*(sum(area.array[which(dom_var_dredge_relimp == i)])/total_land_area),
                                                     "var" = vars[i]))
}


count_dom_var_dredge_relimp.df$var_factor <- factor(count_dom_var_dredge_relimp.df$var, levels = vars[1:5])

bar_dom_var_dredge_relimp <- ggplot(count_dom_var_dredge_relimp.df, aes(x = var_factor, y = area, fill = var_factor)) +
  geom_bar(stat='identity',col = 'black', width = .5) +
  geom_text(inherit.aes = F, data = count_dom_var_dredge_relimp.df, aes(x=var_factor,y=area-.5, fill = var_factor,label=round(area,0)), size = 2.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), position = "right") +
  scale_fill_manual(values = cols_dom_var,
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
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size=10)
  )
bar_dom_var_dredge_relimp
bar_dom_var_dredge_relimp <- ggplotGrob(bar_dom_var_dredge_relimp)
plot_bar_dom_var_dredge_relimp <- bar_dom_var_dredge_relimp

count_dom_var_dredge_relimp_reg.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                               c("reg","count","area","var"))
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
  total_sign_land_area_reg <- sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                             which(lat >= latmin & lat <= latmax)][which(!is.na(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                           which(lat >= latmin & lat <= latmax)]) & dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                                                               which(lat >= latmin & lat <= latmax)] != 6)])
  for(i in 1:5){
    count_dom_var_dredge_relimp_reg.df <- rbind(count_dom_var_dredge_relimp_reg.df,
                                                data.frame("reg" = reg[count_j],
                                                           "count" = length(which(dom_var_ts.df$dom_var_dredge_relimp[which(dom_var_ts.df$lon >= lonmin &
                                                                                                                              dom_var_ts.df$lon <= lonmax &
                                                                                                                              dom_var_ts.df$lat >= latmin &
                                                                                                                              dom_var_ts.df$lat <= latmax)] == i)),
                                                           "area" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                                        which(lat >= latmin & lat <= latmax)][which(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                               which(lat >= latmin & lat <= latmax)] == i)])/total_land_area_reg),
                                                           "sign_area" = 100*(sum(area.array[which(lon >= lonmin & lon <= lonmax),
                                                                                             which(lat >= latmin & lat <= latmax)][which(dom_var_dredge_relimp[,,1][which(lon >= lonmin & lon <= lonmax),
                                                                                                                                                                    which(lat >= latmin & lat <= latmax)] == i)])/total_sign_land_area_reg),
                                                           "var" = vars[i]))
  }
  count_j <- count_j + 1
}

count_dom_var_dredge_relimp_reg.df$var_factor <- factor(count_dom_var_dredge_relimp_reg.df$var, levels = vars[1:5])
count_dom_var_dredge_relimp_reg.df$reg_factor <- factor(count_dom_var_dredge_relimp_reg.df$reg, levels = reg)



bar_dom_var_dredge_relimp_netrad_per_reg <- ggplot(count_dom_var_dredge_relimp_reg.df[which(count_dom_var_dredge_relimp_reg.df$var == 'netrad'),], aes(x = reg_factor, y = sign_area, fill = var_factor, label=round(sign_area,0))) +
  geom_bar(stat='identity', position = position_dodge(width = NULL), col = 'black', width = .5) +
  geom_text(size = 2.5, position = position_dodge(width=.8), vjust = +1) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-% per region")), expand = c(0,0)) +
  scale_fill_manual("",labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index"),
                    values = cols_dom_var,
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
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_dom_var_dredge_relimp_netrad_per_reg
bar_dom_var_dredge_relimp_netrad_per_reg <- ggplotGrob(bar_dom_var_dredge_relimp_netrad_per_reg)
plot_bar_dom_var_dredge_relimp_netrad_per_reg <- bar_dom_var_dredge_relimp_netrad_per_reg

single_var.df <- dom_var_ts.df[which(dom_var_ts.df$number_of_expl_var == 1),]

a <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = single_var.df, aes(x=lon,y=lat), size = .5) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var, 
                    labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index",expression("adjusted R"^2*" < 0.3")),
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24),
        plot.tag.position = c(.55,0.03)
  ) + ggtitle("b)")
a
a <- ggplotGrob(a)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, a, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp_netrad_per_reg, t = 26, l = 3, b = 18, r = 8)
gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp, t = 16, l = 3, b = 10, r = 8)
grid.draw(gt1)

gt_relimp0.3 <- gt1

# how much % actually has a good prediction?
sum(area.array[which(!is.na(dom_var_dredge_relimp[,,1]) & dom_var_dredge_relimp[,,1] != 6)], na.rm = T) / total_land_area

plots <- plot_grid(gt_relimp0.7, gt_relimp0.3, nrow = 2, align = 'hv')

ggsave("testdir/SFig14.png", plot = plots, width = 9, height = 14, units = "in")
