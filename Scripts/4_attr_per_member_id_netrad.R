# Trying attribution of ELI trends to predictors per individual ensemble member
# 03-09-2021
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
# source('Scripts/functions/smoothed.R')
source('Scripts/functions/plot_discrete_cbar.R')

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

# select the right models and put in array
dcorr_netrad_all.array <- 
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
  dcorr_netrad_all.array[,,count_all:(count_all+11)] <- dcorr_netrad.list[[i]]
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
test <- array(NaN,c(180,90,11*12)); test[1:90,,] <- dcorr_netrad_all.array[91:180,,]; test[91:180,,] <- dcorr_netrad_all.array[1:90,,]; dcorr_netrad_all.array <- test; dcorr_netrad_all.array <- -1*dcorr_netrad_all.array
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

# maybe now per surface area instead of % of grid cells?
r <- raster()  # by default 1 by 1 degree
res(r) <- 2 # so change the resolution
a <- raster::area(r) # calculate the area of a 2x2 degree grid from N - S, as area varies only by latitude, not longitude
area <- a[,1]
area.array <- array(NaN,c(180,90))
for(x in 1:180){
  area.array[x,] <- area
}

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

# define hot spot regions
hotspot_regs.df <- data.frame("x" = c(lon[52], lon[62], lon[52], lon[52], lon[30], lon[58], lon[30], lon[30], lon[92], lon[100], lon[92], lon[92], lon[103], lon[138], lon[103], lon[103], lon[138], lon[152], lon[138], lon[138]),
                              "y" = c(lat[38], lat[38], lat[38], lat[48], lat[65], lat[65], lat[65], lat[77], lat[67], lat[67], lat[67], lat[72], lat[72], lat[72], lat[72], lat[77], lat[63], lat[63], lat[63], lat[57]),
                              "xend" = c(lon[52], lon[62], lon[62], lon[62], lon[30], lon[58], lon[58], lon[58], lon[92], lon[100], lon[100], lon[100], lon[103], lon[138], lon[138], lon[138], lon[138], lon[152], lon[152], lon[152]),
                              "yend" = c(lat[48], lat[48], lat[38], lat[48], lat[77], lat[77], lat[65], lat[77], lat[72], lat[72], lat[67], lat[72], lat[77], lat[77], lat[72], lat[77], lat[57], lat[57], lat[63], lat[57]))

# dredge + calc.relimp()
# https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.15385
options(na.action = "na.fail")
vars <- c("netrad","mrso","hfls","lai","ar","adj. R2 < 0.5")
dom_var_dredge_relimp <- array(NaN,c(180,90,11,2)) # the most important variable for all CMIP6 models
dom_var_dredge_relimp_all <- array(0,c(180,90,11,5)) # count of all the variables across all the multilinear models for all CMIP6 models
adj_R2_dredge_relimp <- array(NaN,c(180,90,11))
lin_mod.array <- array(NaN,c(180,90,11))

# rename variables, otherwise ".array" from the character string is interpreted as 'ar' --> aridity variables
dcorr_netrad_all <- dcorr_netrad_all.array; av_tas <- av_tas.array; av_netrad <- av_rnet.array; av_mrso <- av_mrso.array; av_hfls <- av_hfls.array; av_lai <- av_lai.array; av_ar <- av_ar.array
count <- 1
for(t in seq(1,(11*12),12)){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(dcorr_netrad_all[x,y,(t:(t+11))])) == 12 & sum(!is.na(av_lai[x,y,(t:(t+11))])) == 12 & round(sd(av_lai[x,y,(t:(t+11))]),4) != 0 & round(sd(av_mrso[x,y,(t:(t+11))]),4) != 0){ # there should be variability in mrso and lai, otherwise there is no explanatory power in their respective time series
        relimp_variables.list <- list()
        rsq.vec <- c()
        # dredge: automated model selection
        lm_dcorr_netrad.df <- lm(dcorr_netrad_all[x,y,(t:(t+11))] ~
                            av_netrad[x,y,(t:(t+11))] + av_mrso[x,y,(t:(t+11))] + av_hfls[x,y,(t:(t+11))] +
                            av_lai[x,y,(t:(t+11))] + av_ar[x,y,(t:(t+11))])
        dd <- dredge(lm_dcorr_netrad.df, extra = list(AIC))
        
        # IMPORTANCE: pick a subset of best models (all have same performance/complexity in terms of AIC)
        dd_subset <- subset(dd, delta < 4)
        relimp_var_lmg.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                      c("var","lmg","weight"))
        total_var_expl.c <- c()
        for(lin_mod in 1:dim(dd_subset)[1]){ # loop over all models of good enough quality
          rsq.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$r.squared
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
                  dom_var_dredge_relimp_all[x,y,count,i] <- dom_var_dredge_relimp_all[x,y,count,i] + 1
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
                  dom_var_dredge_relimp_all[x,y,count,i] <- dom_var_dredge_relimp_all[x,y,count,i] + 1
                  next
                }
              }
            }
          }
          if(lin_mod == dim(dd_subset)[1]){ # on the last model
            lin_mod.array[x,y,count] <- lin_mod
            adj_R2_dredge_relimp[x,y,count] <- mean(rsq.vec, na.rm = T)
            for(j in 1:5){
              total_var_expl.c[j] <- weighted.mean(x = relimp_var_lmg.df$lmg[which(relimp_var_lmg.df$var == j)], w = relimp_var_lmg.df$weight[which(relimp_var_lmg.df$var == j)], na.rm = T)
            }
            if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 1){ # if there is one dominant variable
              dom_var_dredge_relimp[x,y,count,1] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
            }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 2){ # if there is a two way tie
              dom_var_dredge_relimp[x,y,count,] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
            }
          }
        }
        
        
      }
    }
    print(paste0("t = ",count,", ",round(x/(180)*100,2),"%"))
  }
  count <- count + 1
}

save.image(file = 'testdir/4_attr_per_member_id.RData')

# insert '6' where there is warm land area and adj R2 < 0.5
for(i in 1:11){
  for(x in 1:180){
    for(y in 1:90){
      if(models_with_full_timeseries[x,y] > 4 & is.na(dom_var_dredge_relimp[x,y,i,1])){
        dom_var_dredge_relimp[x,y,i,1] <- 6
      }
    }
  }
}

dom_var_ts.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                          c("dom_var_dredge_relimp","lon","lat","source_id"))
for(i in 1:11){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(dom_var_dredge_relimp[x,y,i,1])){
        dom_var_ts.df <- rbind(dom_var_ts.df, 
                               data.frame("dom_var_dredge_relimp" = dom_var_dredge_relimp[x,y,i,1],
                                          "lon" = lon[x],
                                          "lat" = lat[y],
                                          "source_id" = cmip6_data.df$source_id[i]
                               ))
      }
    }
  }
}

cols_dom_var <- c(brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(11,"BrBG")[3], 'snow2')
myvalues_dom_var <- seq(0.5,6.5,1)

dom_var_ts.df$cuts_dom_var_dredge_relimp <- cut(dom_var_ts.df$dom_var_dredge_relimp, myvalues_dom_var, include.lowest = T)

# the mean dcorr_netrad hist plot
count_dom_var_dredge_relimp.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                           c("count","area","var","source_id"))
for(t in 1:11){
  for(i in 1:5){
    count_dom_var_dredge_relimp.df <- rbind(count_dom_var_dredge_relimp.df,
                                            data.frame("count" = length(which(dom_var_ts.df$source_id == cmip6_data.df$source_id[t] & dom_var_ts.df$dom_var_dredge_relimp == i)),
                                                       "area" = 100*(sum(area.array[which(dom_var_dredge_relimp[,,t,1] == i)])/total_land_area),
                                                       "var" = vars[i],
                                                       "source_id" = cmip6_data.df$source_id[t]))
  }
}


count_dom_var_dredge_relimp.df$var_factor <- factor(count_dom_var_dredge_relimp.df$var, levels = vars[1:5])

bar_dom_var_dredge_relimp <- ggplot(count_dom_var_dredge_relimp.df, aes(x = var_factor, y = area, fill = var_factor)) +
  geom_boxplot() + 
  scale_x_discrete("Dominant variable",
                   labels = c("","","","","")) +
  scale_y_continuous(expression(paste("land area-%")), limits = c(4,20), expand = c(0,0)) +
  scale_fill_manual("",
                    values = cols_dom_var,
                    labels = c("Surface net radiation","Soil moisture","Terrestrial evaporation","Leaf Area Index","Aridity Index"),
                    drop = F) +
  theme(legend.position = c(.85,.9),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        legend.key = element_rect(color = 'transparent', fill  = 'transparent'),
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

ggsave("testdir/SFig16.png", plot = bar_dom_var_dredge_relimp, width = 9, height = 7, units = "in")
