2023-02-26
by: Jasper Denissen

This is a text message describing the use of all the files included in this directory for the paper "Widespread shift from ecosystem energy to water limitation with climate change", Denissen et al., 2022.

This main directory should have 4 subfolders:
Data: This can be downloaded from https://doi.org/10.5281/zenodo.7682660
RData: This can be downloaded from https://doi.org/10.5281/zenodo.7682660
Figures: A folder with the original paper figures.
testdir: An empty folder meant for output of all scripts.

Any .R script can be executed by opening R from the terminal by simply typing 'R' in the command prompt. In R, execute 'source('name_script.R')'. All the output paths have been set to write to testdir/. If the complete collection of scripts is to be tested, with intermediate .RData files in between, the input paths should be changed accordingly (from RData/ to testdir/). Any warning messages can be ignored.

[NOTE] It is not necessary to download and aggregate the CMIP6 runs and pre-process the data, the paper figures can be made directly in [3]. 
[0] states the R environment originally used.
[1] download and aggregate the CMIP6 runs
[2] Data pre-processing for plot figures
[3] Making the paper figures 

[0] R environment
The following version of R has been used to compile the scripts
R version 4.0.4 (2021-02-15)
Platform: x86_64-redhat-linux-gnu

Install all necessary packages from within the R terminal: source('Scripts/to_be_installed_packages.R')

Make sure to re-set the working directory to a directory of your choosing, preferably where you downloaded the data & scripts, at the start of every script!

[1] download and aggregate the CMIP6 runs
The python script to download and aggregate the CMIP6 runs is available from http://doi.org/10.5281/zenodo.5900392
For details on variable_id, institution_id, source_id and member_id, look up Table 3 in the Methods section of the xxxmethodsxxx
More information on downloading CMIP6 data directly from cloud storage can be found here: https://pangeo-data.github.io/pangeo-cmip6-cloud/overview.html
Downloading and aggregating the data is not necessary. Aggregated 2.0x2.0 degree resolution is not available as .nc files for crop and tree fractions. The regridded data of all variables excluding crop and tree fraction have been saved in /Data/cmip6_202106 and Data/cmip6_202106_w_evspsblveg. 

Unfortunately, downloading crop and tree fractions was not possible with http://doi.org/10.5281/zenodo.5900392 (you might want to re-try, actually), but have been downloaded manually through https://esgf-data.dkrz.de/search/cmip6-dkrz/ and backed up in cmip6_202106_frac.
Aggregating cropFrac and treeFrac is not necessary, as all the regridded data necessary for making the figures can be found in RData/. However, aggregating can be done by executing Scripts/regrid_croptreeFrac.R. 

Data/
cmip6_202106                                      # folder containing regridded temperature (tas), soil moisture (mrso), latent heat flux(hfls), Leaf Area Index (lai), all radiative components (rlus,rlds,rsus,rsds), and precipitation (pr) from 12 CMIP6 models
cmip6_202106_w_evspsblveg                         # folder containing regridded plant transpiration (evspsblveg) and near-surface relative humidity (hurs) from 7 CMIP6 models
cmip6_202106_frac                                 # folder containing native resolution crop (cropFrac) and tree fraction (treeFrac) for 6 CMIP6 models

[2] Data pre-processing
It is not necessary to compute the pre-processed data, as all the .RData files and functions necessary to make the paper figures can be found here:

RData/
202106_cmip6_frac.RData                           # .RData file containing spatially (2.0x2.0 degree) and temporally (decennial) aggregated cropFrac and treeFrac
202112_dcorr_cmip6_10yr_rsds_evspsblveg.RData     # .RData file containing output from read_cmip6_2d00_1980_no_evspsblveg_hurs.R
202208_dcorr_cmip6_10yr_SWin.RData                # .RData file containing output from read_cmip6_2d00_1980.R 
202106_cmip6_no_evspsblveg_hurs.RData             # .RData file containing a data.frame with the source_id, institution_id and member_id of all simulations used in this study
total_land_area.RData                             # .RData file containing an array with the warm land area from total_land_area.R

Scripts/functions/
aggregate_raster.R                                # Function to spatially aggregate raster objects
align_nine_plots.R                                # Function to draw boxes around panels and arrows between panels in Figure 1 and Supplementary Figures 17 & 18
calc_boxes.R                                      # Function to average a target variable across bins along axes of two other variables (side panels in Figure 3)
parallel_raster_functions.R                       # Collection of functions to apply to raster objects in a parallel computing environment
plot_discrete_cbar.R                              # Function that compiles a discrete color bar

If there is interest to pre-process the data, it can be done with the following scripts in this sequence:

Scripts/
cmip6_data.R                                      # Script to make and save 202106_cmip6_no_evspsblveg_hurs.RData
regrid_croptreeFrac.R                             # Script to read, aggregate spatially and decennially cropFrac and treeFrac
read_cmip6_2d00_1980_rsds_evspsblveg.R            # Script to decennially aggregate and pre-process individual variables and other computations necessary for paper figures for 11 CMIP6 models (Table 3). This script makes use of parallel computing, so run it with the proper resources. This script may take a +10 hours to run through.
read_cmip6_2d00_1980_SWin.R                       # Script to decennially aggregate and pre-process hurs, evspsblveg and related computations necessary for paper figures for 7 CMIP6 models (Table 3). This script makes use of parallel computing, so run it with the proper resources. This script may take a +10 hours to run through.
total_land_area.R                                 # Script to make and save total_land_area.RData

[3] Making the paper figures
All the original paper figures can be found here:

With the following scripts one can remake all the original paper and supplementary material figures (Fig 1-4 and SFig 1-20):

Scripts/
1_global_time_series_SWin.R                       # Script to make Figure 1 and SFig 1-7
2_glob_spat_cmip6_mean_rsds.R                     # Script to make Figure 2 and SFig 8-13
3_seascycle_dcorr_mean_rsds.R                     # Script to make Figure 3
4_attr_mean_rsds.R                                # Script to make Figure 4 and SFig 14 and 17
croptreeFrac.R                                    # Script to make SFig 15
4_attr_per_member_id_rsds.R                       # Script to make SFig 16 (This script may take +/- 2 hours to run through)
dry_wet_example_ELI_rsds.R                        # Script to make SFig 18 and 19
simple_model_5yr_ssrd.R                           # Script to make SFig20
