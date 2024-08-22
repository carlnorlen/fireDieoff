# Datasets and R scripts used in "Recent fire history enhances semi-arid conifer forest drought resistance"
---

These data sets and scripts allow for the creation of all figures and supplementary figures and tables cited in 
Norlen, C.A.; Hemes, K.S.; Wang, J.A.; Randerson, J.T.; Battles, J.J.; Tubbesing, C.L.; Goulden, M.L. (2024) "Recent fire history enhances semi-arid conifer forest drought resistance" Forest Ecology and management

## Data Access
The data sets required to create the figures are available in the following DRYAD repository: 
Norlen, C.A.; Hemes, K.S.; Wang, J.A.; Randerson, J.T.; Battles, J.J.; Tubbesing, C.L.; Goulden, M.L. (2024). "Recent fire history enhances semi-arid conifer forest drought resistance"" [Dataset]. Dryad. 

## Description of the data and file structure
Shape File of USFS Ecological Subsections used to create Manuscript Figures 1a, S1
  *  subsection_shp.zip
  
Shape Files of CALFIRE fire perimeters used to create Manuscript Figures 1a,c, S1a,b
  *  fire21_1_shp.zip

Shape Files of USFS fire perimeters with added fire severity information used to create Manuscript Figures 1c, S1c
  *  VebBurnSeverity18_1_shp.zip

CSV files with samples from CALFIRE wildfire and prescribed fire perimeters (burned) and 2-km buffers (controls). Each time series sample for the data set is in a row of the each CSV file  The data includes Evapotranspiration (AET, mm/yr), annual precipitation (ppt, mm/yr), annual temperature (tmax, C), climate normal precipitation (clm_precip_sum, mm/yr), climate normal temperature (clm_temp_mean, C), ADS dieback (tpa, dead trees/acre), NDMI (unit less), Tree Cover (TRE, % cover), Shrub Cover (SHR, % cover),  elevation (meters), latitude, longitude, forty-eight month standardized precipitation index (SPI48, unit less), LANDFIRE Existing Vegetation Type (unitless), most recent fire year in 2010 (fire_year_2010), most recent fire year in 2019 (fire_year_2019), most recent fire type in 2010 (fire_type_2010), Column number for each sample (system.index, unitless), Stratification Layer (stratlayer, unitless) for each grid cell. There are multiple columns for each variable with with pre-fixes from X1_ (1985) to X35_ (2019) to indicate years. The data is used in the following script: fig2_fig_4_figs1_table_1_table_s1_frap_rx_fire_recovery_ts_30m.r 
to create Figures 2, 4, and Tables 1, S1, 
  * fire_south_sierra_FRAP_wildfire_300pt_5_fire_year_10tree_ts4_30m_20231204.csv (burned samples)
  * control_south_sierra_FRAP_2km_buffer_300pt_5_fire_year_10tree_ts4_30m_20231204.csv (control samples)

CSV files with samples from USFS wildfire perimeters with fire severity information (burned) and 2-km buffers (controls). Each time series sample for the data set is in a row of the each CSV file  The data includes Evapotranspiration (AET, mm/yr), annual precipitation (ppt, mm/yr), annual temperature (tmax, C), climate normal precipitation (clm_precip_sum, mm/yr), climate normal temperature (clm_temp_mean, C), ADS dieback (tpa, dead trees/acre), NDMI (unit less), Tree Cover (TRE, % cover), Shrub Cover (SHR, % cover),  elevation (meters), latitude, longitude, forty-eight month standardized precipitation index (SPI48, unit less), LANDFIRE Existing Vegetation Type (unitless), most recent fire year in 2010 (fire_year_2010), most recent fire year in 2019 (fire_year_2019), most recent fire severity in 2010 (fire_sev_2010), Column number for each sample (system.index, unitless), Stratification Layer (stratlayer, unitless) for each grid cell. There are multiple columns for each variable with with pre-fixes from X1_ (1985) to X35_ (2019) to indicate years. The data is used in the following script: fig3_fig5_fig_s2_table_2_table_s2_fire_sev_recovery_ts_30m.r to create Figures 3, 5, S2, and Tables 2, S2, ..    
  * fire_south_sierra_USFS_sevfire_600pt_5_fire_year_20tree_ts4_30m_20231204.csv (burned samples)
  * control_south_sierra_sev_2km_buffer_600pt_5_fire_year_20tree_ts4_30m_20231204.csv (control samples)

  
CSV files of annual data from 10 eddy covariance tower locations used to create Figure S4. Each file is for one of the 10 eddy covariance sites. Each file contains the following variables: wYEAR (water year), Evapotranspiration (ET, mm/yr), n_days (number of days with data), and ID (site description).


CSV file of annual NDVI (unit less), year, Site (FluxNext Site ID), Pixel # (Number of 9 upwind Landsat pixels from tower) for each of the 10 eddy covariance sites used to create Figure S4.
  * UCIupwind_pixels_NDVI_met_30m.csv

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
  * SRTM Digital Elevation Model: 
  * Solar Iradiance
  * Eddy Covariance Data: https://www.ess.uci.edu/~california/

## Code/Software
The code shared with this submission were written in R 4.3.3 and run using RStudio.
The code requires the tidyverse, sf, RSQlite, rFIA, RSToolbox, patchwork, ggpubr, kableExtra, and gstat packages.

R script used to create Figure 1 of the manuscript.
  * fig1_map.r
  
R script used to create Figure 2 of the manuscript. 
  * fig2_time_series.r
  
R script used to create Figure 3 of the manuscript, and Figures S2, S3, S5, S8, S10, S12 and S19. 
  * fig3_sfigs_grids.r
  
R script used to create Figure 4 of the manuscript, Figure S6, and Tables S3 to S5. 
  * fig4_sfig_FIA_analysis.r
  
R script used to create Figure 5 of the manuscript, Tables S1, S2, and S6, and Figures S14, S15, S18, and S20.
  * fig5_correlation_simple.r
  
R script used to create Figures S1, S6, S7, S9, S11, and S13. 
  * sfig1_maps.r
  
R script used to create Figure S4.
  * sfig3_ndvi_flux_tower
  
R script used to create Figures S16, S17.
  * sfig4_spatial_autocorrelation