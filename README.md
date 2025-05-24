# Datasets, R, and Google Earth Engine scripts used in "Recent fire history enhances semi-arid conifer forest drought resistance"
---

These data sets and scripts allow for the creation of all figures and supplementary figures and tables in the following manuscript. When using these data and script please cite the following manuscript.
Norlen, C.A.; Hemes, K.S.; Wang, J.A.; Randerson, J.T.; Battles, J.J.; Tubbesing, C.L.; Goulden, M.L. (2024) "Recent fire history enhances semi-arid conifer forest drought resistance" Forest Ecology and Management. 573 (2024): 122331. https://doi.org/10.1016/j.foreco.2024.122331

## Data Access
The data sets required to create the figures are available in the following DRYAD repository: 
Norlen, C.A.; Hemes, K.S.; Wang, J.A.; Randerson, J.T.; Battles, J.J.; Tubbesing, C.L.; Goulden, M.L. (2024). "Recent fire history enhances semi-arid conifer forest drought resistance"" [Dataset]. Dryad. https://doi.org/10.5061/dryad.s4mw6m9f2

## Description of the data and file structure
Shape File of USFS Ecological Subsections used to create Manuscript Figures 1a, S1
  *  subsection_shp.zip
  
Shape Files of CALFIRE fire perimeters used to create Manuscript Figures 1a,c, S1a,b
  *  fire21_1_shp.zip

Shape Files of USFS fire perimeters with added fire severity information used to create Manuscript Figures 1c, S1c
  *  VebBurnSeverity18_1_shp.zip

CSV files with samples from CALFIRE wildfire and prescribed fire perimeters (burned) and 2-km buffers (controls). Each time series sample for the data set is in a row of the each CSV file  The data includes Evapotranspiration (AET, mm/yr), annual precipitation (ppt, mm/yr), annual temperature (tmax, C), climate normal precipitation (clm_precip_sum, mm/yr), climate normal temperature (clm_temp_mean, C), ADS dieback (tpa, dead trees/acre), NDMI (unit less), 
Wang et al, 2022 Vegetation Cover: Tree Cover (Tree_Cover, % cover), Shrub Cover (Shrub_Cover, % cover), Herbaceous Cover (Herb_Cover, % cover), Bare-ground Cover (Bare_Cover, % cover),
Allred et al, 2021 Vegetation cover: Tree Cover (TRE, % cover), Shrub Cover (SHR, % cover), Annual Forbs and Grasses (AFG, % Cover), Perrennial Forbs and Grasses (PFG, % cover), Bare-ground (BGR, % cover), 
elevation (meters), latitude, longitude, forty-eight month standardized precipitation index (SPI48, unit less), LANDFIRE Existing Vegetation Type (unitless), most recent fire year in 2010 (fire_year_2010), most recent fire year in 2019 (fire_year_2019), most recent fire year in 2020 (fire_year_2020), most recent fire type in 2010 (fire_type_2010), most recent fire type in 2019 (fire_type_2019), most recent fire type in 2020 (fire_type_2020), number of fires record as of 2010 (fire_count_2010), number of fires record as of 2019 (fire_count_2019), number of fires record as of 2020 (fire_count_2020), Column number for each sample (system.index, unitless), Stratification Layer (stratlayer, unitless) for each grid cell. There are multiple columns for each variable with with pre-fixes from X1_ (1985) to X35_ (2019) to indicate years. The data is used in the following script: fig2_fig_4_figs1_table_1_table_s1_frap_rx_fire_recovery_ts_30m.r 
to create Figures 2, 4, 5, S2, S4, S9, S11, R3, and Tables 1, S1. 
  * fire_south_sierra_FRAP_wildfire_300pt_5_fire_year_10tree_ts4_30m_20231204.csv (wildfire burned samples)
  * control_south_sierra_FRAP_2km_buffer_300pt_5_fire_year_10tree_ts4_30m_20231204.csv (wildfire control samples)
  * fire_south_sierra_FRAP_rxfire_300pt_5_fire_year_10tree_ts4_30m_20231204.csv (prescribed fire burned samples)
  * control_south_sierra_Rx_2km_buffer_300pt_5_fire_year_10tree_ts4_30m_20231204.csv (prescribed fire control samples)

CSV files with samples from USFS wildfire perimeters with fire severity information (burned) and 2-km buffers (controls). Each time series sample for the data set is in a row of the each CSV file  The data includes Evapotranspiration (AET, mm/yr), annual precipitation (ppt, mm/yr), annual temperature (tmax, C), climate normal precipitation (clm_precip_sum, mm/yr), climate normal temperature (clm_temp_mean, C), ADS dieback (tpa, dead trees/acre), NDMI (unit less), 
Wang et al, 2022 Vegetation Cover: Tree Cover (Tree_Cover, % cover), Shrub Cover (Shrub_Cover, % cover), Herbaceous Cover (Herb_Cover, % cover), Bare-ground Cover (Bare_Cover, % cover),
Allred et al, 2021 Vegetation cover: Tree Cover (TRE, % cover), Shrub Cover (SHR, % cover), Annual Forbs and Grasses (AFG, % Cover), Perrennial Forbs and Grasses (PFG, % cover), Bare-ground (BGR, % cover),   
elevation (meters), latitude, longitude, forty-eight month standardized precipitation index (SPI48, unit less), LANDFIRE Existing Vegetation Type (unitless), number of fires record as of 2010 (fire_count_2010), number of fires record as of 2019 (fire_count_2019), number of fires record as of 2020 (fire_count_2020), most recent fire severity in 2010 (fire_sev_2010), most recent fire severity in 2019 (fire_sev_2019), most recent fire severity in 2020 (fire_sev_2020), most recent fire ID in 2010 (fire_ID_2010), most recent fire ID in 2019 (fire_ID_2019), most recent fire ID in 2020 (fire_ID_2020), Column number for each sample (system.index, unitless), Stratification Layer (stratlayer, unitless) for each grid cell. There are multiple columns for each variable with with pre-fixes from X1_ (1985) to X35_ (2019) to indicate years. The data is used in the following script: fig3_fig5_fig_s2_table_2_table_s2_fire_sev_recovery_ts_30m.r to create Figures 3, 6, 7, S3, S5, S8, S10, S12, R4, and Tables 1, S2,     
  * fire_south_sierra_USFS_sevfire_600pt_5_fire_year_20tree_ts4_30m_20231204.csv (burned samples)
  * control_south_sierra_sev_2km_buffer_600pt_5_fire_year_20tree_ts4_30m_20231204.csv (control samples)

  
CSV files of annual data from 10 eddy covariance tower locations used to create Figure S4. Each file is for one of the 10 eddy covariance sites. Each file contains the following variables: wYEAR (water year), Evapotranspiration (ET, mm/yr), n_days (number of days with data), and ID (site description).


CSV file of system index (system:index), annual NDVI (NDVI_mean, unit less), Pixel #	year, Pixel # (Number of 9 upwind Landsat pixels from tower), Site (FluxNext Site ID), precipitation (ppt, mm/yr), solar radiation (srad, W/m^2), temprature (tmean, C), year and month (yearmonth), geographic coordinates (.geo) extracted from PRISM, GRDMET, and Landsat for each of the 10 eddy covariance sites used to create Figure S6, S7.
  * UCIupwind_pixels_NDVI_met_30m_export.csv
  
CSV file of FluxNext Site (Site), date (Mean date), daily ET (Efill.1, mm/day), Predicted ET (Predicted ET, mm/day), Dry Season Drawdown (Bucket 2 DSD, mm), Measured ET divided by Predicted ET (MeasureE/Predicted E, unitless) from combined records of 10 eddy covariance sites.
  * Monthly_towerdata3_publish.csv

## Sharing/Access information

Data was derived from these publicly available sources:
  * Landsat data on Google Earth Engine (GEE): https://developers.google.com/earth-engine/datasets/catalog/landsat
  * 48-month Standardized Precipitation Index (SPI48) data: https://wrcc.dri.edu/wwdt/archive.php
  * California state perimeter: https://developers.google.com/earth-engine/datasets/catalog/TIGER_2016_States
  * California Fire Perimeters: https://frap.fire.ca.gov/mapping/gis-data/
  * LANDFIRE Existing Vegetation Type: https://www.landfire.gov/viewer/
  * USFS Ecological Subsections: https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=ecomap+subsection
  * USFS Aerial Detection Surveys: https://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696
  * PRISM Precipitation data: https://developers.google.com/earth-engine/datasets/catalog/OREGONSTATE_PRISM_AN81m   
  * PRISM Temperature data: https://developers.google.com/earth-engine/datasets/catalog/OREGONSTATE_PRISM_AN81m
  * USGS Digital Elevation Model: https://developers.google.com/earth-engine/datasets/catalog/USGS_NED.
  * SRTM Digital Elevation Model: https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1
  * GRIDMET Solar Radiation: https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET 
  * Eddy Covariance Data: https://www.ess.uci.edu/~california/

## Code/Software
The code shared with this submission were written in JavaScript for Google Earth Engine (GEE) and R 4.3.3 run using RStudio.
The code requires the tidyverse, sf, RSQlite, rFIA, RSToolbox, patchwork, ggpubr, kableExtra, and gstat packages. GEE code can be added to the 
GEE Code Editor using the following link: https://code.earthengine.google.com/?accept_repo=users/cnorlen/fireDieoff.

## R Code
Script used to create Figure 1, S2 of the manuscript.
  * fig1_fire_history_maps.r
  
Script used to to create Figures 2, 4, 5 and Table 1 of the manuscript and Figures S2, S4, S9, S11, R3, and Table S1.  
  * frap_rx_fire_recovery_dieback_comparison_30m.r
  
Script used to create Figure Figures 3, 6, 7, and Table 1 of the manuscript and Figures S3, S5, S8, S10, S12, R4, and Tables S2,
  * fire_sev_recovery_dieback_comparison_30m
  
Script used to create Figures S6 and S7. 
  * fig_s9_ndvi_et_scaling
  
## GEE JavaScript Code
Script used to compare Rx perimeters to CCDC disturbance detection
  * compare_Rx_CCDC_Disturbances.js

Script used to export stratified fire data for analysis
  * Fire_stratified_data_export_for_analysis.js
  
Script used to export raster version of the wild and prescribed fire data to GEE Assets
  * Frap_wild_rx_raster_export.js
  
Script used to export monthly NDVI data for the eddy covariance sites for analysis
  * monthly_NDVI_raster_export.js
  
Script used to export raster versions of the wildfire severity data to GEE Assets
  * USFS_Fire_severity_raster_export.js
  
Functions that for an Aerial Detection Survesy (ADS) time series to be added to scripts
  * ads.js
  
Functions that allow for rasterized wildfire data to be added to other scripts
  * frap.js
  
Functions that allow for a time series stack of Landsat data to be added to other scripts
  * landsat.js
