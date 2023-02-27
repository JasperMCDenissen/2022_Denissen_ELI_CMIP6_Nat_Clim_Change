# 2022-01-20
# by: Jasper Denissen
# define a data.frame that carries the source_id and member_id of the 11 models used in the study

##############################################################################################################################
##############################################################################################################################
######################## !!! Don't forget to reset the working directory to a directory of your choosing!!! ##################
##############################################################################################################################
##############################################################################################################################
setwd('/Net/Groups/BGI/work_3/HydroBioClim/archive/Denissen_etal_2022_Nat_Clim_Change/')

cmip6_data.df <- data.frame("source_id" = c("GFDL-CM4", "CNRM-ESM2-1", "BCC-CSM2-MR", "EC-Earth3-Veg", "UKESM1-0-LL", "INM-CM4-8", "CAMS-CSM1-0", "CESM2", "ACCESS-ESM1-5", "CMCC-CM2-SR5", "TaiESM1"),
                            "institution_id" = c("NOAA-GFDL", "CNRM-CERFACS", "BCC", "EC-Earth-Consortium", "MOHC", "INM", "CAMS", "NCAR", "CSIRO", "CMCC", "AS-RCEC"),
                            "member_id" = c("r1i1p1f1", "r1i1p1f2", "r1i1p1f1", "r2i1p1f1", "r2i1p1f2", "r1i1p1f1", "r2i1p1f1", "r4i1p1f1", "r1i1p1f1", "r1i1p1f1", "r1i1p1f1"))

save(cmip6_data.df, file = 'testdir/202106_cmip6_no_evspsblveg_hurs.RData')
