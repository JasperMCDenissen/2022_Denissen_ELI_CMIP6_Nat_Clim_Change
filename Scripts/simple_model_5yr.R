# Simple model to test ELI for typically water-limited and energy-limited grid cell
# 2022-04-12

# define constants & parameters
alfa_MD <- 0.8 # (-)
L <- 1000 # (mm)
SM_wilt <- 0.171 # From the original TESSEL model
SM_crit <- 0.323 # From the original TESSEL model
SM_sat <- 0.472 # From the original TESSEL model
b <- 5.39 # for a typical soil
ks <- 50 # (mm/d)
p = 1013 # Pa


lon <- seq(-179.75,179.75,.5)
lat <- rev(seq(-89.75,89.75,.5))

# Congo (Congo), Riau (Indonesia), Boven-Coppename (Suriname), 
loc <- c('Congo (1.25S, 21.25E)',
         'Indonesia (1.25N, 101.75E)',
         'Suriname (3.25N, 56.25W)',
         'United Kingdom (55.25N, 2.25W)',
         'Brasil (6.25S, 71.25W)')
lons <- c(which(lon == 21.25), which(lon == 101.75), which(lon == -56.25), which(lon == -2.25), which(lon == -71.25))
lats <- c(which(lat == -1.25), which(lat == 1.25), which(lat == 3.25), which(lat == 55.25), which(lat == -6.25))

# SM index reports the fraction of time steps that is water-limited
SM_index <- corr_wtr_veg <- corr_rgy_veg <- ELI <- array(NaN,c(5,7)) # 10 grid cells, 7 time steps of 5 years

# # So I think it would be wise to do a loop over 1) all grid cells and 2) all 5 year periods. 
# count_t <- 1
# for(xy in 1:5){
#   for(t in seq(0,34,5)){
#     
#     # read in necessary variables from ERA5
#     # mm/d
#     path_tp <- "/Net/Groups/data_BGC/era5/e1/0d50_daily/tp/"
#     tp <- c()
#     for(year in (1981+t):(1981+t+4)){
#       tp_ncin <- nc_open(paste0(path_tp,'tp.daily.fc.era5.720.360.',year,'.nc'))
#       if(year %% 4 != 0){
#         tp <- c(tp, ncvar_get(tp_ncin, 'tp', start = c(lons[xy],lats[xy],1), count = c(1,1,365)))
#       }else{
#         tp <- c(tp,ncvar_get(tp_ncin, 'tp', start = c(lons[xy],lats[xy],1), count = c(1,1,366)))
#       }
#       nc_close(tp_ncin)
#       print(paste0("tp in year ",year," is done..."))
#     }
#     
#     # K
#     path_t2m <- "/Net/Groups/data_BGC/era5/e1/0d50_daily/t2m/"
#     t2m <- c()
#     for(year in (1981+t):(1981+t+4)){
#       t2m_ncin <- nc_open(paste0(path_t2m,'t2m.daily.an.era5.720.360.',year,'.nc'))
#       if(year %% 4 != 0){
#         t2m <- c(t2m, ncvar_get(t2m_ncin, 't2m', start = c(lons[xy],lats[xy],1), count = c(1,1,365)))
#       }else{
#         t2m <- c(t2m, ncvar_get(t2m_ncin, 't2m', start = c(lons[xy],lats[xy],1), count = c(1,1,366)))
#       }
#       nc_close(t2m_ncin)
#       print(paste0("t2m in year ",year," is done..."))
#     }
#     
#     # MJ/m2
#     path_snr <- "/Net/Groups/data_BGC/era5/e1/0d50_daily/snr/"
#     snr <- c()
#     count <- 1
#     for(year in (1981+t):(1981+t+4)){
#       snr_ncin <- nc_open(paste0(path_snr,'snr.daily.calc.era5.720.360.',year,'.nc'))
#       if(year %% 4 != 0){
#         snr <- c(snr, ncvar_get(snr_ncin, 'snr', start = c(lons[xy],lats[xy],1), count = c(1,1,365))/86400*10^6/26.15741)
#       }else{
#         snr <- c(snr, ncvar_get(snr_ncin, 'snr', start = c(lons[xy],lats[xy],1), count = c(1,1,366))/86400*10^6/26.15741)
#       }
#       nc_close(snr_ncin)
#       print(paste0("snr in year ",year," is done..."))
#     }
#     
#     # loop over 5 years of daily data
#     ETp <- ET <- Q_leakage <- c()
#     SM <- SM_crit
#     for(i in 1:length(tp)){
#       ETp[i] <- alfa_MD*snr[i] # from Milly and Dunne (2016), Eq. (4) from Maes et al., 2018
#       ET[i] <- ETp[i]*max(c(0,min(c((SM[i] - SM_wilt)/(SM_crit - SM_wilt),1))))
#       Q_leakage[i] <- L*SM[i] - L*SM_sat*((SM[i]/SM_sat)^(-2*b-2) + ((2*b+2)*ks)/(SM_sat*L))^(-1/(2*b+2)) # From Buitink et al., 2021
#       if(i < length(tp)){
#         SM[i+1] <- min(c(SM_sat, SM[i] + (1/L)*(tp[i] - ET[i] - Q_leakage[i]))) # everything in mm?
#       }
#     }
#     
#     SM_index[xy,count_t] <- length(which(SM[which(t2m > 283.15)] < SM_crit)) / length(which(t2m > 283.15))
#     
#     # monthly average
#     days <- c(0:(length(tp)-1))
#     day1 <- dmy(paste0("01-01-",as.character(1981+t)))
#     days <- day1 + days
#     count <- 1
#     day_to_month <- c() # amount of days per month from 2009 - 2018
#     for(i in (1981+t):(1981+t+4)){
#       for(j in 1:12){
#         day_to_month[which(year(days) == i & month(days) == j)] <- count
#         count <- count + 1
#       }
#     }
#     
#     t2m_mon <- SM_mon <- snr_mon <- ET_mon <- c()
#     for(i in 1:60){
#       t2m_mon[i] <- mean(t2m[which(day_to_month == i)])
#       SM_mon[i] <- mean(SM[which(day_to_month == i)])
#       snr_mon[i] <- mean(snr[which(day_to_month == i)])
#       ET_mon[i] <- mean(ET[which(day_to_month == i)])
#     }
#     
#     
#     # Now calculate ELI for the same time period
#     moy <- rep(c(1:12),5)
#     detrend_snr_mon <- moy_snr_mon <- anom_snr_mon <- 
#       detrend_ET_mon <- moy_ET_mon <- anom_ET_mon <- 
#       detrend_SM_mon <- moy_SM_mon <- anom_SM_mon <- 
#       c()
#     detrend_snr_mon <- snr_mon - c(1:60) * lm(snr_mon ~ c(1:60))$coef[2]
#     detrend_ET_mon <- ET_mon - c(1:60) * lm(ET_mon ~ c(1:60))$coef[2]
#     detrend_SM_mon <- SM_mon - c(1:60) * lm(SM_mon ~ c(1:60))$coef[2]
#     for(mon in 1:12){
#       moy_snr_mon[which(moy == mon)] <- mean(detrend_snr_mon[which(moy == mon)])
#       moy_ET_mon[which(moy == mon)] <- mean(detrend_ET_mon[which(moy == mon)])
#       moy_SM_mon[which(moy == mon)] <- mean(detrend_SM_mon[which(moy == mon)])
#     }
#     anom_snr_mon <- detrend_snr_mon - moy_snr_mon
#     anom_ET_mon <- detrend_ET_mon - moy_ET_mon
#     anom_SM_mon <- detrend_SM_mon - moy_SM_mon
#     if(length(which(t2m_mon > 283.15)) > 15){
#       corr_wtr_veg[xy,count_t] <- cor(anom_SM_mon[which(t2m_mon > 283.15)],
#                                       anom_ET_mon[which(t2m_mon > 283.15)],
#                                       method = 'kendall', use = "pairwise.complete.obs")
#       corr_rgy_veg[xy,count_t] <- cor(anom_snr_mon[which(t2m_mon > 283.15)],
#                                       anom_ET_mon[which(t2m_mon > 283.15)],
#                                       method = 'kendall', use = "pairwise.complete.obs")
#       ELI[xy,count_t] <- corr_wtr_veg[xy,count_t] - corr_rgy_veg[xy,count_t]
#     }
#     
#     print(paste0((1981+t)," - ",(1981+t+4), " is done..."))
#     count_t <- count_t + 1
#   }
#   print(paste0("grid cell ",xy," is done..."))
#   count_t <- 1
# }
# 
# # plot(SM_index[1,],ELI[1,])
# # plot(SM_index[2,],ELI[2,])
# # plot(SM_index[3,],ELI[3,])
# # plot(SM_index[4,],ELI[4,])
# # plot(SM_index[5,],ELI[5,])
# # plot(SM_index[6,],ELI[6,])
# # plot(SM_index[7,],ELI[7,])
# # plot(SM_index[8,],ELI[8,])
# # plot(SM_index[9,],ELI[9,])
# # plot(SM_index[10,],ELI[10,])
# 
# # SM_index <- 1-SM_index
# agree <- agree_corr_wtr_veg <- rep(0,5)
# cor <- c()
# for(xy in 1:5){
#   cor[xy] <- cor(ELI[xy,],SM_index[xy,])
#   for(t in 1:5){
#     if(sign(SM_index[xy,t] - SM_index[xy,(t+1)]) == sign(ELI[xy,t] - ELI[xy,(t+1)])){
#       agree[xy] <- agree[xy] + 1
#     }
#     if(sign(SM_index[xy,t] - SM_index[xy,(t+1)]) == sign(corr_wtr_veg[xy,t] - corr_wtr_veg[xy,(t+1)])){
#       agree_corr_wtr_veg[xy] <- agree[xy] + 1
#     }
#   }
# }
# 
# comp.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
#                     c("SM_index","ELI", "time", "loc"))
# for(xy in 1:5){
#   comp.df <- rbind(comp.df,
#                    data.frame("SM_index" = SM_index[xy,],
#                               "ELI" = ELI[xy,],
#                               "time" = seq(1981,2011,5),
#                               "loc" = rep(loc[xy],7)))
# }
# 
# 
# save(comp.df,
#      file = "testdir/202204_simple_model_ELI_5yr.RData")
# from /RData
load('RData/202204_simple_model_ELI_5yr.RData')
# from /testdir
# load('testdir/202204_simple_model_ELI_5yr.RData')

cols_loc <- c(brewer.pal(11,"RdYlBu")[3], brewer.pal(11,"RdYlBu")[9], 'turquoise2', brewer.pal(11,"PRGn")[9], brewer.pal(8, "Set2")[6])

SM_index_plot <- ggplot(comp.df, aes(x=time,y=SM_index,col=loc)) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0.5, linetype='dashed') +
  scale_x_continuous("",
                     breaks = seq(1981,2011,5),
                     labels = c("'81","'86","'91","'96","'01","'06","'11"), 
                     expand = c(0,0)) +
  scale_y_continuous(expression("fraction of days where SM < SM"[crit]*""), expand = c(0,0)
  ) +
  scale_color_manual("",values = c(cols_loc)) +
  theme(legend.position = c(.25,.5),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"), 
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle("a)")
SM_index_plot

ELI_plot <- ggplot(comp.df, aes(x=time,y=ELI,col=loc)) +
  geom_line(linetype = 'solid') +
  geom_hline(yintercept = 0, linetype='dashed') + 
  scale_x_continuous("",
                     breaks = seq(1981,2011,5),
                     labels = c("'81","'86","'91","'96","'01","'06","'11"), 
                     expand = c(0,0)) +
  scale_y_continuous(expression("Ecosystem Limitation Index"), expand = c(0,0)
  ) +
  scale_color_manual(values = c(cols_loc)) +
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
        axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle("b)")
ELI_plot

scatter_plot <- ggplot(comp.df, aes(x=SM_index,y=ELI)) +
  geom_smooth(method='lm', formula = y~x, col = 'black') +
  geom_point(inherit.aes = F, aes(x=SM_index,y=ELI,col=loc)) +
  geom_hline(yintercept = 0, linetype='dashed') + 
  geom_vline(xintercept = 0.5, linetype='dashed') + 
  scale_x_continuous(expression("fraction of days where SM < SM"[crit]*""), expand = c(0,0)) +
  scale_y_continuous(expression("Ecosystem Limitation Index"), expand = c(0,0)
  ) +
  scale_color_manual("",values = c(cols_loc)) +
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
        axis.title = element_text(size=20),
        plot.title = element_text(size=24),
        axis.line.y.right = element_line(color = "grey80"),
        axis.ticks.y.right = element_line(color = "grey80"),
        axis.text.y.right = element_text(color = "grey80"),
        axis.title.y.right = element_text(color = "grey80")
        
  ) + ggtitle("c)")
scatter_plot

plots <- plot_grid(SM_index_plot, ELI_plot, scatter_plot, ncol = 3, align = 'h')
ggsave("testdir/SFig20.png", plot = plots, width = 15*1.25, height = 5*1.25, units = "in")
