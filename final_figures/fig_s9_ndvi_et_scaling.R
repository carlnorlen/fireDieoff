#Created by: Carl A. Norlen
#Date Created: August, 9, 2023
#Date Updated: August 15, 2023
#Purpose: Create figures to show ET-NDVI scaling relationships and correction factors

    ######## PREP environment ######
# data_read='.../DRYAD/Data/'
# FRAP_path = '.../CalFIRE_FRAP/FRAP_shp/firep18_1.shp' # FRAP polygons: https://frap.fire.ca.gov/mapping/gis-data
# USFS_path = '.../USFS/VegBurnSeverity18_1_shp/VegBurnSeverity18.shp' # USFS polygons: https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/fseprd596279.zip

# Load packages
p <- c("dplyr", "ggplot2", "tidyr", "stringi","varhandle","zoo","lubridate","reshape2","sf","elevatr", "ggpubr",  
              "broom","tidyverse","vroom","scales","data.table",'yardstick','patchwork','mgcv','mgcViz','RColorBrewer','fs','readxl');
# lapply(packages, library, character.only = TRUE);
# rm('packages')
# library(ggpubr)
# library(ggpmisc)
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
      ET_mm_d=Efill.1) #%>%
      #dplyr::select(site_ID,ET_mm_d,date,Days_Month) %>%
      # mutate(ET_mm_d = case_when(ET_mm_d<=0 ~ 0,
      #                                ET_mm_d>0 ~ ET_mm_d)) #%>% 
      #filter(!site_ID%in%c('US-SCd'))

#Fix the column name
UCI_tower_good$ET.GEE.predict <- UCI_tower_good$'Predicted E from predicted GEE'
UCI_tower_good$ET.predict <- UCI_tower_good$'Predicted ET'
UCI_tower_good$soil.bucket.2dsd <- UCI_tower_good$'Bucket 2 DSD'
UCI_tower_good$et.mes.div.et.pred <- UCI_tower_good$'MeasureE/Predicted E'

# summary(UCI_tower_good)
# summary(GEE_NDVI)
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
GEE_ET_tower_all %>% summary()

#Calculate the predicted NDVI
GEE_ET_tower_all$ET.NDVI.predict  <- 4.1760 * (GEE_ET_tower_all$NDVI_mean)^0.9925
GEE_ET_tower_all$et.mes.div.et.pred.ndvi <- GEE_ET_tower_all$ET_mm_d /  GEE_ET_tower_all$ET.NDVI.predict

#Create ET scalars
GEE_ET_tower_all$ET.soil.moisture.scalar <- 0.69 + (0.00035 * GEE_ET_tower_all$soil.bucket.2dsd)
GEE_ET_tower_all$ET.solar.rad.scalar <- 0.02 + (0.0029 * GEE_ET_tower_all$srad)
GEE_ET_tower_all$ET.temp.scalar <- 0.74 + (0.0053 * GEE_ET_tower_all$tmean)

#Do final ET prediction
GEE_ET_tower_all$ET.predict.scalars <- GEE_ET_tower_all$ET.NDVI.predict * GEE_ET_tower_all$ET.soil.moisture.scalar * GEE_ET_tower_all$ET.solar.rad.scalar * GEE_ET_tower_all$ET.temp.scalar
GEE_ET_tower_all$ET.predict.final <- 0.71 + 1.2 * GEE_ET_tower_all$ET.predict.scalars
# summary(GEE_ET_tower_all)
  # plot scaling factors
    # figSX_scalingfactors <- ggplot(GEE_GPP) + geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Tcorr,color='T scalar'),alpha=0.5) + 
    #   geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr,color='K scalar'),alpha=0.5) + 
    #   geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr*GPP_CAsites_Tcorr,color='Combined scalar')) + 
    #   geom_hline(yintercept=1,color='black') +
    #   facet_wrap(~site_ID,scales='free') +
    #   labs(color='GPP correction',x='month',y='correction scalar')
# summary(GEE_ET_tower_all)
# GEE_ET_tower_all %>% select(site_ID) %>% unique()

# Create an exponential model.
# Estimate the rest parameters using a linear model
model.data <- GEE_ET_tower_all %>% filter('Bucket 2 DSD' > 10 & site_ID != 'US-SCg' & 
                                            case_when(site_ID %in% c('US-CZ1', 'US-SCs', 'US-SCw', 'US-SCd', 'US-SCc') ~ 
                                                        month %in% c('03', '04', '05'),
                                                        site_ID %in% c('US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCf') ~ month %in% c('06', '07', '08')))

model.0 <- lm(ET_mm_d ~ NDVI_mean, data=model.data)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Create the model starting parameters
start <- list(alpha = alpha.0, beta = beta.0)

#Create the exponential fit between ET and NDVI
nlsFit <-
  nls(formula = ET_mm_d~alpha*NDVI_mean^(beta),
      start = start,
      data = model.data)
nlsFit
# 
# nlsFitR2 <- summary(nlsFit)$r.squared
# nlsFitR2
#Figure from Kyles Plots
# plot scaling factors
#Create scaling plots
#Maximum ET from NDVI Equation
p1a <- ggplot(data = GEE_ET_tower_all %>% filter('Bucket 2 DSD' > 10 & site_ID != 'US-SCg' & 
                                                 case_when(site_ID %in% c('US-CZ1', 'US-SCs', 'US-SCw', 'US-SCd', 'US-SCc') ~ 
                                                           month %in% c('03', '04', '05'), 
                                                           site_ID %in% c('US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCf') ~ month %in% c('06', '07', '08'))), 
       mapping = aes(x = NDVI_mean, y = ET_mm_d)) + 
  geom_smooth(linetype = 'dashed',
              method = nls, method.args = list(formula = y ~ alpha*x ^(beta), start = start), se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95, mapping = aes(label = after_stat(rr.label)), #, expression('ET = 4.1760 * NDVI'^'0.9925'), sep = "~`,`~")),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  annotate(geom = "text", x = 0.31, y = 5.5, size = 3.5, label = expression(italic(y)*' = 4.1760 * '*italic(x)^'0.9925'), parse = TRUE) +
  # stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
       geom_point(alpha = 0.5) +
       theme_bw() +
       ylab(expression('Observed ET (mm day'^-1*')')) +
       xlab('NDVI')
p1a

# glimpse(GEE_ET_tower_all)
# summary(GEE_ET_tower_all)
#Measured / Observed ET by Soil moisture correction
#Am I using the correct ET predict? This is the final prediction
p1b <- ggplot(data = GEE_ET_tower_all %>% filter(ET.predict > 1 & site_ID != 'US-SCg'), #%>% filter('Bucket 2 DSD' >= 10 & 
                                                   # case_when(site_ID %in% c('US-CZ1', 'US-SCs', 'US-SCw', 'US-SCd', 'US-SCc') ~ 
                                                   #             month %in% c('03', '04', '05'), 
                                                   #           site_ID %in% c('US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCf') ~ month %in% c('06', '07', '08'))), 
              mapping = aes(x = soil.bucket.2dsd, y = et.mes.div.et.pred.ndvi)) + 
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95, mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  ylab(expression('Predicted ET / Observed ET')) +
  xlab('Soil Moisture (mm)')
p1b

p1c <- ggplot(data = GEE_ET_tower_all  %>% filter(ET.predict > 1 & site_ID != 'US-SCg'), #%>% filter('Bucket 2 DSD' >= 10 & 
              # case_when(site_ID %in% c('US-CZ1', 'US-SCs', 'US-SCw', 'US-SCd', 'US-SCc') ~ 
              #             month %in% c('03', '04', '05'), 
              #           site_ID %in% c('US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCf') ~ month %in% c('06', '07', '08'))), 
              mapping = aes(x = srad, y = et.mes.div.et.pred.ndvi)) + 
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95,mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  ylab(expression('Predicted ET / Observed ET')) +
  xlab(expression('Solar Radiation (W m'^-2*')'))
p1c

p1d <- ggplot(data = GEE_ET_tower_all  %>% filter(ET.predict > 1 & site_ID != 'US-SCg'), #%>% filter('Bucket 2 DSD' >= 10 & 
              # case_when(site_ID %in% c('US-CZ1', 'US-SCs', 'US-SCw', 'US-SCd', 'US-SCc') ~ 
              #             month %in% c('03', '04', '05'), 
              #           site_ID %in% c('US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCf') ~ month %in% c('06', '07', '08'))), 
              mapping = aes(x = tmean, y = et.mes.div.et.pred.ndvi)) + 
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95,mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  ylab(expression('Predicted ET / Observed ET')) +
  xlab('Temperature (C)')
p1d

p1e <- ggplot(data = GEE_ET_tower_all  %>% filter(!is.na(ET_mm_d) & site_ID != 'US-SCg'), #%>% filter('Bucket 2 DSD' >= 10 & 
              # case_when(site_ID %in% c('US-CZ1', 'US-SCs', 'US-SCw', 'US-SCd', 'US-SCc') ~ 
              #             month %in% c('03', '04', '05'), 
              #           site_ID %in% c('US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCf') ~ month %in% c('06', '07', '08'))), 
              mapping = aes(x = ET.predict.scalars, y = ET.NDVI.predict)) + 
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95,mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  ylab(expression('Observed ET (mm day'^-1*')')) +
  xlab(expression('Predicted ET with Scalars (mm day'^-1*')'))
p1e


f1 <- ggarrange(p1a, p1b, p1c, p1d, p1e, nrow=2, ncol = 3, common.legend = FALSE, align = "hv", labels = c('a', 'b', 'c', 'd', 'e'))

ggsave(filename = 'figS2_ET_prediction_equations.png', height=16, width= 24, units = 'cm', dpi=900)

# figS1_scalingfactors <- ggplot(data = GEE_ET_tower_all) + geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Tcorr,color='T scalar'),alpha=0.5) + 
#   geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr,color='K scalar'),alpha=0.5) + 
#   geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr*GPP_CAsites_Tcorr,color='Combined scalar')) + 
#   geom_hline(yintercept=1,color='black') +
#   facet_wrap(~site_ID,scales='free') +
#   labs(color='GPP correction',x='month',y='correction scalar')

# ANNUAL
# GEE_GPP_tower_annual <- GEE_GPP_tower_all %>% group_by(site_ID,year=year(date)) %>%
#   summarize(GPP_CAscale_annual = sum(GPPgf_CAsites*Days_Month,na.rm=T), 
#             GPP_CAscale_annual_n = n(),
#             GPP_UCIsites_annual = sum(GPP_gC_m2_d*Days_Month,na.rm=T),
#             GPP_UCIsites_annual_n = n())
# 
# GPPmod <- lm(GPP_UCIsites_annual ~ GPP_CAscale_annual, GEE_GPP_tower_annual)
# summary(GPPmod)
# GPPmodRMSE <- sqrt(mean(GPPmod$residuals^2))
# GPPmodR2 <- summary(GPPmod)$r.squared

# ANNUAL
GEE_ET_tower_annual <- GEE_ET_tower_all %>% group_by(site_ID,year) %>%
      summarize(ET_UCIsites_annual = sum(ET_mm_d*Days_Month,na.rm=T),
                ET.predict.annual = sum(ET.predict.final*Days_Month,na.rm=T),
                ET_UCIsites_n = n())
    GEE_ET_tower_annual
  
ETmod <- lm(ET_UCIsites_annual ~ ET.predict.annual, GEE_ET_tower_annual)
summary(ETmod)
ETmodRMSE <- sqrt(mean(ETmod$residuals^2))
ETmodRMSE
ETmodR2 <- summary(ETmod)$r.squared
ETmodR2 

#Add Text labels
#Rsq for the model
rsq.et.mod <- data.frame(
  label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =round(ETmodR2, digits =2)))))
  ),
  x = 100,
  y = 1200
)
#RMSE for the model
rmse.et.mod <- data.frame(
  label = c(as.character(as.expression(substitute(italic(RMSE)~"="~r2, list(r2 =round(ETmodRMSE,digits=2)))))
  ),
  x = 100,
  y = 1100
)


#Overall Model predictions  
figS2_scaled_observedET <- ggplot(data = GEE_ET_tower_annual %>% filter(year >= 2006 & year <= 2018 & site_ID != 'US-SCg'), 
                           aes(x = ET_UCIsites_annual, y = ET.predict.annual)) +
                           geom_point(alpha = 0.5, size = 2, mapping = aes(color = site_ID)) +
                           geom_abline(slope = 1, linewidth = 2, color = 'blue') +
                           guides(color=guide_legend(title="Site ID")) +    
                           geom_text(data = rsq.et.mod, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
                           geom_text(data = rmse.et.mod, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
                           theme_bw() +                         
                           theme(legend.position = c(0.8, 0.3),
                                 axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
                                 axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12)) +  
                           geom_smooth(method = 'lm', formula = y ~ x, color = 'black', linewidth = 2, linetype = 'dashed') + 
                           # stat_cor(mapping = aes(label = after_stat(rr.label))) +
                           xlim(0,1200) + ylim(0,1200) + 
                           xlab(expression('Observed ET (mm year'^-1*')')) + ylab(expression('Predicted ET (mm year'^-1*')'))
figS2_scaled_observedET

ggsave(filename = 'figS3_ET_prediction_testing.png', height=16, width= 16, units = 'cm', dpi=900)
