#Created by: Carl A. Norlen
#Date Created: August, 9, 2023
#Date Updated: August 9, 2023
#Purpose: Create figures to show ET-NDVI scaling relationships and correction factors

    ######## PREP environment ######
# data_read='.../DRYAD/Data/'
# FRAP_path = '.../CalFIRE_FRAP/FRAP_shp/firep18_1.shp' # FRAP polygons: https://frap.fire.ca.gov/mapping/gis-data
# USFS_path = '.../USFS/VegBurnSeverity18_1_shp/VegBurnSeverity18.shp' # USFS polygons: https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/fseprd596279.zip

# Load packages
p <- c("dplyr", "ggplot2", "tidyr", "stringi","varhandle","zoo","lubridate","reshape2","sf","elevatr",
              "broom","tidyverse","vroom","scales","data.table",'yardstick','patchwork','mgcv','mgcViz','RColorBrewer','fs','readxl');
# lapply(packages, library, character.only = TRUE);
# rm('packages')
# library(readxl)
# install.packages(p,repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# library(scales)
#Home Computer directories
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')
dir_in <- "D:\\Fire_Dieoff\\FluxSites"
data <- read.csv(file.path(dir_in, 'UCIupwind_pixels_NDVI_met_30m_export.csv'), header = TRUE, na.strings = "NaN")
data %>% summary()
### Fig SX, SX: GPP-NIRv model figs ###
    # 0.) Ingest GEE NIRv and GPP data
GEE_NDVI <- data %>% 
            dplyr::select(-c('system.index','.geo')) %>%
  mutate(site_ID=Site,date=as.Date(paste0(as.character(year_month), '01'), format='%Y%m%d'),
         NDVI_mean = na_if(NDVI_mean, 0)#, # exported zeroes are missing NIRv data
         # GPP_CAsites_Kcorr = na_if(GPP_CAsites_Kcorr,0),
         # GPP_CAsites_Tcorr = na_if(GPP_CAsites_Tcorr,0),
         # GPP_CAsites_raw = na_if(GPP_CAsites_raw,0),
         # GPP_CAsites_raw_gf = na_if(GPP_CAsites_raw_gf,0),
         # NIRv_monthly = na_if(NIRv_monthly,0),
         # NIRv_monthly_gf = na_if(NIRv_monthly_gf,0),
         # GPPgf_CAsites = na_if(GPPgf_CAsites,0)
         ) %>%
  dplyr::select(-Site) #%>%
  #filter(!site_ID%in%c('US-SCd'))
# GEE_NDVI
# # take mean of 9 pixels footprint for each site
GEE_NDVI_mean <- GEE_NDVI %>% group_by(site_ID,date) %>% # mean and std
  summarise_all("mean") %>%
  dplyr::select(-`Pixel..`)

# readxl
    # 1.) Ingest flux data
    UCI_tower_good <- read_excel(paste0(dir_in,'\\Monthly_towerdata3.xlsx'),sheet='All sites good4') %>% # new gC/m2/day
      # dplyr::select(Site,Efill.1,`Mean date`) %>%
      mutate(site_ID = as.factor(Site), date=floor_date(as.Date(`Mean date`),unit='month'), Days_Month = days_in_month(date),
      ET_mm_d=Efill.1) %>%
      #dplyr::select(site_ID,ET_mm_d,date,Days_Month) %>%
      mutate(ET_mm_d = case_when(ET_mm_d<=0 ~ 0,
                                     ET_mm_d>0 ~ ET_mm_d)) #%>% 
      #filter(!site_ID%in%c('US-SCd'))

#Fix the column name
UCI_tower_good$ET.predict <- UCI_tower_good$'Predicted ET'

summary(UCI_tower_good)
# summary(UCI_tower_good)
# p1 <- ggplot(data = UCI_tower_good, mapping = aes(x = ET_mm_d, y = ET.predict)) + 
#       geom_abline(slope = 1, linewidth = 2, color = 'blue') +
#       geom_point(alpha = 0.5, size = 2) + 
#       theme_bw() +
#       geom_smooth(method = 'lm', formula = y ~ x, color = 'black', linewidth = 2, linetype = 'dashed') + 
#       stat_cor(mapping = aes(label = after_stat(rr.label))) +
#       xlab(expression('Observed ET (mm day'^-1*')')) + ylab(expression('Predicted ET (mm day'^-1*')'))
#       
# p1      
# combine
GEE_ET_tower_all <- left_join(GEE_NDVI_mean,UCI_tower_good,by=c("date", "site_ID"))
# GEE_ET_tower_all %>% summary()
GEE_ET_tower_all$year <- format(GEE_ET_tower_all$date, '%Y')
GEE_ET_tower_all$month <- format(GEE_ET_tower_all$date, '%m')
GEE_ET_tower_all
  # plot scaling factors
    # figSX_scalingfactors <- ggplot(GEE_GPP) + geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Tcorr,color='T scalar'),alpha=0.5) + 
    #   geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr,color='K scalar'),alpha=0.5) + 
    #   geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr*GPP_CAsites_Tcorr,color='Combined scalar')) + 
    #   geom_hline(yintercept=1,color='black') +
    #   facet_wrap(~site_ID,scales='free') +
    #   labs(color='GPP correction',x='month',y='correction scalar')
    
    # ANNUAL
    GEE_ET_tower_annual <- GEE_ET_tower_all %>% group_by(site_ID,year) %>%
      summarize(ET_UCIsites_annual = sum(ET_mm_d*Days_Month,na.rm=T),
                ET.predict.annual = sum(ET.predict*Days_Month,na.rm=T),
                ET_UCIsites_monthly_n = n(),
                NDVI_mean = NDVI_mean)
    GEE_ET_tower_annual
  
    # ETmod <- lm(ET_UCIsites_annual ~ ET_UCIsites_annual, GEE_ET_tower_annual)
    # summary(ETmod)
    # ETmodRMSE <- sqrt(mean(ETmod$residuals^2))
    # ETmodR2 <- summary(ETmod)$r.squared
 
#This isn't quite working yet   
figSX_scaled_observedET <- ggplot(data = GEE_ET_tower_annual %>% filter(year >= 2006 & year <= 2018 & site_ID != 'US-SCg'), 
                           aes(x = ET_UCIsites_annual, y = ET.predict.annual)) +
                           geom_point(alpha = 0.5, size = 2, mapping = aes(color = site_ID)) +
                           geom_abline(slope = 1, linewidth = 2, color = 'blue') +
                           guides(color=guide_legend(title="Site ID")) +                        
                           theme_bw() +                         
                           theme(legend.position = c(0.8, 0.3),
                                 axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
                                 axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +  
                           geom_smooth(method = 'lm', formula = y ~ x, color = 'black', linewidth = 2, linetype = 'dashed') + 
                           stat_cor(mapping = aes(label = after_stat(rr.label))) +
                           xlab(expression('Observed ET (mm year'^-1*')')) + ylab(expression('Predicted ET (mm year'^-1*')'))
figSX_scaled_observedET

ggsave(filename = 'figS2_ET_prediction_testing.png', height=16, width= 16, units = 'cm', dpi=900)
