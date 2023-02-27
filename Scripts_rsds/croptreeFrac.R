# read treeFrac & cropFrac
# 2021-06-23
# by: Jasper Denissen
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

# from /RData
load('RData/202106_cmip6_no_evspsblveg_hurs.RData')
# load('RData/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
load('RData/total_land_area.RData')
load('RData/202106_cmip6_frac.RData')
# # from /testdir
# load('testdir/202106_cmip6_no_evspsblveg_hurs.RData')
# load('testdir/202112_dcorr_cmip6_10yr_no_evspsblveg_hurs_netrad.RData')
# load('testdir/total_land_area.RData')
# load("testdir/202106_cmip6_frac.RData")
load("testdir/202208_dcorr_cmip6_10yr_SWin.RData")

dcorr.list <- list(dcorr.list[[9]], dcorr.list[[8]], dcorr.list[[10]], dcorr.list[[2]],
                   dcorr.list[[4]], dcorr.list[[6]])
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
test <- array(NaN,c(180,90,6*12)); test[1:90,,] <- dcorr_all.array[91:180,,]; test[91:180,,] <- dcorr_all.array[1:90,,]; dcorr_all.array <- test
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
  array(NaN,c(180,90,6,2)) # 7: models, 2: slope (all) & p.value (all)
count_i <- 1
for(i in seq(1,(6*12),12)){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(dcorr_all.array[x,y,(i:(i+11))])) == 12){ # only calculate if the model has full time series
        kendall_dcorr[x,y,count_i,] <- c(unname(kendallTrendTest(dcorr_all.array[x,y,(i:(i+11))])$estimate[2]), unname(kendallTrendTest(dcorr_all.array[x,y,(i:(i+11))])$p.value))
      }
    }
  }
  print(i)
  count_i <- count_i + 1
}

mmmean_dcorr_per_10yr <- mmmean_tas_per_10yr <- mmmean_rsds_per_10yr <- 
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
          mmmean_tas_per_10yr[x,y,i] <- mean(av_tas.array[x,y,(index_per_10yr+i)],na.rm=T)
          mmmean_rsds_per_10yr[x,y,i] <- mean(av_rsds.array[x,y,(index_per_10yr+i)],na.rm=T)
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

mmmean_kendall_dcorr <- mmmean_kendall_rsds <- mmmean_kendall_hfls <-
  mmmean_kendall_mrso <- mmmean_kendall_lai <- mmmean_kendall_ar <- array(NaN,c(180,90,2)) # 2: slope (all) & p.value (all)
sum_blocks <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_per_10yr[x,y,])) == 12){
      sum_blocks[x,y] <- sum(!is.na(mmmean_dcorr_per_10yr[x,y,]))
      mmmean_kendall_dcorr[x,y,] <- c(unname(kendallTrendTest(mmmean_dcorr_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_dcorr_per_10yr[x,y,])$p.value))
      mmmean_kendall_rsds[x,y,] <- c(unname(kendallTrendTest(mmmean_rsds_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_rsds_per_10yr[x,y,])$p.value))
      mmmean_kendall_mrso[x,y,] <- c(unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_mrso_per_10yr[x,y,])$p.value))
      mmmean_kendall_hfls[x,y,] <- c(unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_hfls_per_10yr[x,y,])$p.value))
      mmmean_kendall_lai[x,y,] <- c(unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_lai_per_10yr[x,y,])$p.value))
      mmmean_kendall_ar[x,y,] <- c(unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$estimate[2]), unname(kendallTrendTest(mmmean_ar_per_10yr[x,y,])$p.value))
    }
  }
}

options(na.action = "na.fail")
vars <- c("rsds","mrso","hfls","lai","ar","cropFrac","treeFrac","adj. R2 < 0.5")
dom_var_dredge_relimp <- array(NaN,c(180,90,2))
dom_var_dredge_relimp_all <- array(0,c(180,90,7))
adj_R2_dredge_relimp <- array(NaN,c(180,90))
lin_mod.array <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(mmmean_dcorr_per_10yr[x,y,])) == 12 & sum(!is.na(mmmean_lai_per_10yr[x,y,])) == 12 & round(sd(mmmean_lai_per_10yr[x,y,]),4) != 0 & round(sd(mmmean_cropFrac_per_10yr[x,y,]),4) != 0 & round(sd(mmmean_mrso_per_10yr[x,y,]),4) != 0){ # there should be variability in mrso and lai, otherwise there is no explanatory power in their respective time series
      relimp_variables.list <- list()
      rsq.vec <- c()
      # dredge: automated model selection
      lm_dcorr.df <- lm(mmmean_dcorr_per_10yr[x,y,] ~
                          mmmean_rsds_per_10yr[x,y,] + mmmean_mrso_per_10yr[x,y,] + mmmean_hfls_per_10yr[x,y,] +
                          mmmean_lai_per_10yr[x,y,] + mmmean_ar_per_10yr[x,y,] + 
                          mmmean_cropFrac_per_10yr[x,y,] + mmmean_treeFrac_per_10yr[x,y,])
      dd <- dredge(lm_dcorr.df, extra = list(AIC))
      
      # IMPORTANCE: pick a subset of best models (all have same performance/complexity in terms of AIC)
      dd_subset <- subset(dd, delta < 4)
      relimp_var_lmg.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                    c("var","lmg","weight"))
      total_var_expl.c <- c()
      for(lin_mod in 1:dim(dd_subset)[1]){ # loop over all models of good enough quality
        rsq.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$r.squared
        if(rsq.vec > .5){ # double check whether their adjusted r squared are half decent
          print(lin_mod)
          if(length(names(summary(get.models(dd_subset, lin_mod)[[1]])$aliased)) - 1 > 1){ # check if model has multiple variables
            relimp <- calc.relimp(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:7){
              if(sum(grepl(vars[i], names(relimp@lmg))) == 1){ # extract all variables and explained variance per variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = unname(relimp$lmg[which(grepl(vars[i], names(relimp@lmg)) == T)]), # explained variance per variable
                                                      "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                dom_var_dredge_relimp_all[x,y,i] <- dom_var_dredge_relimp_all[x,y,i] + 1
              }
            }
          }else{ # if the model has only one variable
            expl_dcorr_dredge.df <- summary(get.models(dd_subset, lin_mod)[[1]])
            for(i in 1:7){
              if(grepl(vars[i],names(expl_dcorr_dredge.df$aliased)[2])){ # if there are only two explanatory variables left in the model, the first one is the intercept and the second one is the variable
                relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                           data.frame("var" = i, # which variable
                                                      "lmg" = expl_dcorr_dredge.df$r.squared, # explained variance for dominant variable
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
          for(j in 1:7){
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

# insert '8' where there is warm land area and adj R2 < 0.5
for(x in 1:180){
  for(y in 1:90){
    if(models_with_full_timeseries[x,y] > 4 & is.na(dom_var_dredge_relimp[x,y,1])){
      dom_var_dredge_relimp[x,y,1] <- 8
    }
  }
}


how_many_vars_relimp <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    how_many_vars_relimp[x,y] <- length(which(dom_var_dredge_relimp_all[x,y,] > 0))
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

cols_dom_var <- c(brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(11,"BrBG")[3], 'goldenrod1', 'deeppink2', 'snow2')
myvalues_dom_var <- seq(0.5,8.5,1)

dom_var_ts.df$cuts_dom_var_dredge_relimp <- cut(dom_var_ts.df$dom_var_dredge_relimp, myvalues_dom_var, include.lowest = T)

# the mean dcorr hist plot
count_dom_var_dredge_relimp.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                           c("count","area","var"))
for(i in 1:7){
  count_dom_var_dredge_relimp.df <- rbind(count_dom_var_dredge_relimp.df,
                                          data.frame("count" = length(which(dom_var_ts.df$dom_var_dredge_relimp == i)),
                                                     "area" = 100*(sum(area.array[which(dom_var_dredge_relimp == i)])/total_land_area),
                                                     "var" = vars[i]))
}


count_dom_var_dredge_relimp.df$var_factor <- factor(count_dom_var_dredge_relimp.df$var, levels = vars[1:7])

bar_dom_var_dredge_relimp <- ggplot(count_dom_var_dredge_relimp.df, aes(x = var_factor, y = area, fill = var_factor)) +
  geom_bar(stat='identity',col = 'black', width = .5) +
  geom_text(inherit.aes = F, data = count_dom_var_dredge_relimp.df, aes(x=var_factor,y=area-.5, fill = var_factor,label=round(area,0)), size = 2.5) + 
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), position = "left") +
  scale_fill_manual(values = cols_dom_var[1:7],
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

single_var.df <- dom_var_ts.df[which(dom_var_ts.df$number_of_expl_var == 1),]

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

# define hot spot regions
hotspot_regs.df <- data.frame("x" = c(lon[52], lon[62], lon[52], lon[52], lon[30], lon[58], lon[30], lon[30], lon[92], lon[100], lon[92], lon[92], lon[103], lon[138], lon[103], lon[103], lon[138], lon[152], lon[138], lon[138]),
                              "y" = c(lat[38], lat[38], lat[38], lat[48], lat[65], lat[65], lat[65], lat[77], lat[67], lat[67], lat[67], lat[72], lat[72], lat[72], lat[72], lat[77], lat[63], lat[63], lat[63], lat[57]),
                              "xend" = c(lon[52], lon[62], lon[62], lon[62], lon[30], lon[58], lon[58], lon[58], lon[92], lon[100], lon[100], lon[100], lon[103], lon[138], lon[138], lon[138], lon[138], lon[152], lon[152], lon[152]),
                              "yend" = c(lat[48], lat[48], lat[38], lat[48], lat[77], lat[77], lat[65], lat[77], lat[72], lat[72], lat[67], lat[72], lat[77], lat[77], lat[72], lat[77], lat[57], lat[57], lat[63], lat[57]))


a <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp)) + 
  geom_tile() +
  geom_point(inherit.aes = F, data = single_var.df, aes(x=lon,y=lat), size = .5) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), 
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var, 
                    labels = c("Incoming shortwave radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index","Crop fraction","Tree fraction",expression("adjusted R"^2*" < 0.5")),
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
gt1 <- gtable_add_grob(gt1, bar_dom_var_dredge_relimp, t = 25, l = 4, b = 17, r = 10)
grid.draw(gt1)
plot_dom_var_ts_dredge_croptreeFrac <- gt1

ggsave("testdir/SFig15.png", plot = plot_dom_var_ts_dredge_croptreeFrac, width = 9, height = 7, units = "in")
