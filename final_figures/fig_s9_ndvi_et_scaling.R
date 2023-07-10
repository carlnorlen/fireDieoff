# Hemes et al 2023
# PNAS

    ######## PREP environment ######
# data_read='.../DRYAD/Data/'
# FRAP_path = '.../CalFIRE_FRAP/FRAP_shp/firep18_1.shp' # FRAP polygons: https://frap.fire.ca.gov/mapping/gis-data
# USFS_path = '.../USFS/VegBurnSeverity18_1_shp/VegBurnSeverity18.shp' # USFS polygons: https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/fseprd596279.zip

# Load packages
packages <- c("dplyr", "ggplot2", "tidyr", "stringi","varhandle","zoo","lubridate","reshape2","sf","elevatr",
              "broom","tidyverse","vroom","scales","data.table",'yardstick','patchwork','mgcv','mgcViz','RColorBrewer','fs','readxl');
# lapply(packages, library, character.only = TRUE);
# rm('packages')
library(readxl)
install.packages(p,repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# library(scales)
#Home Computer directories
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')
dir_in <- "D:\\Fire_Dieoff\\FluxSites"
data <- read.csv(file.path(dir_in, 'UCIupwind_pixels_byMonthGPPgf_30m.csv'), header = TRUE, na.strings = "NaN")
### Fig SX, SX: GPP-NIRv model figs ###
    # 0.) Ingest GEE NIRv and GPP data
GEE_GPP <- vroom(file=paste0(dir_in,'\UCIupwind_pixels_byMonthGPPgf_30m.csv'),   
  col_select=list(-'system:index',-'.geo')) %>%
  mutate(site_ID=Site,date=ymd(date),
         GPP_CAsites = na_if(GPP_CAsites, 0), # exported zeroes are missing NIRv data
         GPP_CAsites_Kcorr = na_if(GPP_CAsites_Kcorr,0),
         GPP_CAsites_Tcorr = na_if(GPP_CAsites_Tcorr,0),
         GPP_CAsites_raw = na_if(GPP_CAsites_raw,0),
         GPP_CAsites_raw_gf = na_if(GPP_CAsites_raw_gf,0),
         NIRv_monthly = na_if(NIRv_monthly,0),
         NIRv_monthly_gf = na_if(NIRv_monthly_gf,0),
         GPPgf_CAsites = na_if(GPPgf_CAsites,0)) %>%
  dplyr::select(-Site) %>%
  filter(!site_ID%in%c('US-SCd'))

# take mean of 9 pixels footprint for each site
GEE_GPP_mean <- GEE_GPP %>% group_by(site_ID,date) %>% # mean and std
  summarise_all("mean") %>%
  dplyr::select(-`Pixel #`)

readxl
    # 1.) Ingest flux data
    UCI_tower_good <- read_excel(paste0(dir_in,'\\Monthly_towerdata3.xlsx'),sheet='All sites good4') %>% # new gC/m2/day
      dplyr::select(Site,GEEfill,`Mean date`) %>%
      mutate(site_ID = as.factor(Site), date=floor_date(as.Date(`Mean date`),unit='month'), Days_Month = days_in_month(date),
             GPP_gC_m2_d=GEEfill*-1) %>%
      dplyr::select(site_ID,GPP_gC_m2_d,date,Days_Month) %>%
      mutate(GPP_gC_m2_d = case_when(GPP_gC_m2_d<=0 ~ 0,
                                     GPP_gC_m2_d>0 ~ GPP_gC_m2_d)) %>% 
      filter(!site_ID%in%c('US-SCd'))
summary(UCI_tower_good)
      # combine
    GEE_GPP_tower_all <- left_join(GEE_GPP_mean,UCI_tower_good,by=c("date", "site_ID"))

  # plot scaling factors
    figSX_scalingfactors <- ggplot(GEE_GPP) + geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Tcorr,color='T scalar'),alpha=0.5) + 
      geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr,color='K scalar'),alpha=0.5) + 
      geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr*GPP_CAsites_Tcorr,color='Combined scalar')) + 
      geom_hline(yintercept=1,color='black') +
      facet_wrap(~site_ID,scales='free') +
      labs(color='GPP correction',x='month',y='correction scalar')
    
    # ANNUAL
    GEE_GPP_tower_annual <- GEE_GPP_tower_all %>% group_by(site_ID,year=year(date)) %>%
      summarize(GPP_CAscale_annual = sum(GPPgf_CAsites*Days_Month,na.rm=T), 
                GPP_CAscale_annual_n = n(),
                GPP_UCIsites_annual = sum(GPP_gC_m2_d*Days_Month,na.rm=T),
                GPP_UCIsites_annual_n = n())
  
    GPPmod <- lm(GPP_UCIsites_annual ~ GPP_CAscale_annual, GEE_GPP_tower_annual)
    summary(GPPmod)
    GPPmodRMSE <- sqrt(mean(GPPmod$residuals^2))
    GPPmodR2 <- summary(GPPmod)$r.squared
    
    figSX_scaled_observedGPP <- ggplot(filter(GEE_GPP_tower_annual,GPP_UCIsites_annual_n>=12&GPP_CAscale_annual_n>=11)) +
      geom_point(aes(GPP_UCIsites_annual,GPP_CAscale_annual,color=site_ID)) +
      geom_smooth(aes(GPP_UCIsites_annual,GPP_CAscale_annual,color='linear model'),method='lm') +
      geom_abline(intercept = 0, slope = 1) +
      labs(x=expression(Observed~GPP~(gC~m^-2~year^-1)), y=expression(Scaled~GPP~(gC~m^-2~year^-1)),
           #title='Scaled GPP (w T and K scalar correction) vs. measured GPP, UCI sites',
           subtitle=expression(RMSE==134.01))

