#Created by: Carl A. Norlen
#Date Created: August, 9, 2023
#Date Updated: August 15, 2023
#Purpose: Create figures to show ET-NDVI scaling relationships and correction factors

#List of packages
p <- c("dplyr", "ggplot2", "tidyr", "stringi","varhandle","zoo","lubridate","reshape2","sf","elevatr", "ggpubr",  
              "broom","tidyverse","vroom","scales","data.table",'yardstick','patchwork','mgcv','mgcViz','RColorBrewer','fs','readxl');

# Load packages
lapply(p,require,character.only=TRUE)

#Home Computer directories
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/30m_test')
dir_in <- "D:\\Fire_Dieoff\\FluxSites"
data <- read.csv(file.path(dir_in, 'UCIupwind_pixels_NDVI_met_30m_export.csv'), header = TRUE, na.strings = "NaN")
data %>% summary()

### Fig SX, SX: GPP-NIRv model figs ###
    # 0.) Ingest GEE NIRv and GPP data
GEE_NDVI <- data %>% 
            dplyr::select(-c('system.index','.geo')) %>%
  mutate(site_ID=Site,date=as.Date(paste0(as.character(year_month), '01'), format='%Y%m%d'),
         NDVI_mean = na_if(NDVI_mean, 0)
         ) %>%
  dplyr::select(-Site) 

# GEE_NDVI
# # take mean of 9 pixels footprint for each site
GEE_NDVI_mean <- GEE_NDVI %>% group_by(site_ID,date) %>% # mean and std
  summarise_all("mean") %>%
  dplyr::select(-`Pixel..`)

# readxl
    # 1.) Ingest flux data
UCI_tower_good <- read.csv(paste0(dir_in,'\\Monthly_towerdata3_publish.csv')) %>% 
      mutate(site_ID = as.factor(Site), date=floor_date(as.Date(`Mean date`),unit='month'), Days_Month = days_in_month(date),
      ET_mm_d=Efill.1) 

#Fix the column name
UCI_tower_good$ET.predict <- UCI_tower_good$'Predicted ET'
UCI_tower_good$soil.bucket.2dsd <- UCI_tower_good$'Bucket 2 DSD'
UCI_tower_good$et.mes.div.et.pred <- UCI_tower_good$'MeasureE/Predicted E'

#Combine the data
GEE_ET_tower_all <- left_join(GEE_NDVI_mean,UCI_tower_good,by=c("date", "site_ID"))

#Add year and month to the data
GEE_ET_tower_all$year <- format(GEE_ET_tower_all$date, '%Y')
GEE_ET_tower_all$month <- format(GEE_ET_tower_all$date, '%m')

#summarize the data
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
GEE_ET_tower_all$ET.predict.final <- 1.8 * GEE_ET_tower_all$ET.predict.scalars

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

#Create scaling plots
#Maximum ET from NDVI Equation
p1a <- ggplot(data = GEE_ET_tower_all %>% filter('Bucket 2 DSD' > 10 & site_ID != 'US-SCg' & 
                                                 case_when(site_ID %in% c('US-CZ1', 'US-SCs', 'US-SCw', 'US-SCd', 'US-SCc') ~ 
                                                           month %in% c('03', '04', '05'), 
                                                           site_ID %in% c('US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCf') ~ month %in% c('06', '07', '08'))), 
       mapping = aes(x = NDVI_mean, y = ET_mm_d)) + 
  geom_point(alpha = 0.5, mapping = aes(color = site_ID)) +
  geom_smooth(linetype = 'dashed',
              method = nls, method.args = list(formula = y ~ alpha*x ^(beta), start = start), se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95, mapping = aes(label = after_stat(rr.label)), #, expression('ET = 4.1760 * NDVI'^'0.9925'), sep = "~`,`~")),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  annotate(geom = "text", x = 0.31, y = 5.5, size = 3.5, label = expression(italic(y)*' = 4.1760 * '*italic(x)^'0.9925'), parse = TRUE) +
       theme_bw() +
       theme(legend.position = 'none') +
       ylab(expression('Observed ET (mm day'^-1*')')) +
       xlab('NDVI')
p1a

#Dry Season Drawdown versus Measured / Observed ET by Soil moisture correction
p1b <- ggplot(data = GEE_ET_tower_all %>% filter(ET.predict > 1 & site_ID != 'US-SCg'), 
              mapping = aes(x = soil.bucket.2dsd, y = et.mes.div.et.pred.ndvi)) + 
  geom_point(alpha = 0.5, mapping = aes(color = site_ID)) +
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95, mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
  
  theme_bw() +
  theme(legend.position = 'none') +
  ylab(expression('Predicted ET / Observed ET')) +
  xlab('Soil Moisture (mm)')
p1b

#Solar Radiation versus Measured / Observed ET by Soil moisture correction
p1c <- ggplot(data = GEE_ET_tower_all  %>% filter(ET.predict > 1 & site_ID != 'US-SCg'), 
              mapping = aes(x = srad, y = et.mes.div.et.pred.ndvi)) + 
  geom_point(alpha = 0.5, mapping = aes(color = site_ID)) +
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95,mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
  
  theme_bw() +
  theme(legend.position = 'none') +
  ylab(expression('Predicted ET / Observed ET')) +
  xlab(expression('Solar Radiation (W m'^-2*')'))
p1c

#Temperature versus Measured / Observed ET by Soil moisture correction
p1d <- ggplot(data = GEE_ET_tower_all  %>% filter(ET.predict > 1 & site_ID != 'US-SCg'),  
              mapping = aes(x = tmean, y = et.mes.div.et.pred.ndvi)) + 
  geom_point(alpha = 0.5, mapping = aes(color = site_ID)) +
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y.npc = 0.95,mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = 0.85, size = 3.5) +
  
  theme_bw() +
  theme(legend.position = 'none') +
  ylab(expression('Predicted ET / Observed ET')) +
  xlab('Temperature (C)')
p1d

#Calculate prediction correction factor
p1e <- ggplot(data = GEE_ET_tower_all  %>% filter(!is.na(ET_mm_d) & site_ID != 'US-SCg'),
              mapping = aes(x = ET.predict.scalars, y = ET.NDVI.predict)) + 
  geom_point(alpha = 0.5, mapping = aes(color = site_ID)) +
  geom_smooth(linetype = 'dashed',
              method = 'lm', formula = y ~ 0 + x, se=FALSE, color = 'black', linewidth = 2) +
  stat_cor(label.x.npc = 0.1, label.y = 5, mapping = aes(label = paste(..rr.label..)),
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  stat_regline_equation(label.x.npc = 0.1, label.y = 4.5, size = 3.5, formula = y ~ 0 + x) +
  theme_bw() +
  theme(legend.position = 'none') +
  ylab(expression('Observed ET (mm day'^-1*')')) +
  xlab(expression('Predicted ET with Scalars (mm day'^-1*')'))
p1e

#Blank panel to create legend
p1f <- ggplot(data = GEE_ET_tower_all  %>% filter(!is.na(ET_mm_d) & site_ID != 'US-SCg'), 
               mapping = aes(x = ET.predict.scalars, y = ET.NDVI.predict)) + 
  geom_point(alpha = 0.5, mapping = aes(color = site_ID))+
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  labs(color = 'Site ID') +
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size =  12),
        legend.title = element_text(size = 15, face = "bold"))+
  guides(colour = guide_legend(override.aes = list(size=8), ncol = 2, nrow = 5))
p1f
f1 <- ggarrange(p1a, p1b, p1c, p1d, p1e, p1f, nrow=3, ncol = 2, common.legend = FALSE, align = "hv", labels = c('a', 'b', 'c', 'd', 'e', ''))
f1

ggsave(filename = 'figS2_ET_prediction_equations.png', height=24, width= 16, units = 'cm', dpi=900)
ggsave(filename = 'figS2_ET_prediction_equations.svg', height=24, width= 16, units = 'cm', dpi=900)

# ANNUAL
GEE_ET_tower_annual <- GEE_ET_tower_all %>% group_by(site_ID,year) %>%
      summarize(ET_UCIsites_annual = sum(ET_mm_d*Days_Month,na.rm=T),
                ET.predict.annual = sum(ET.predict.final*Days_Month,na.rm=T),
                ET_UCIsites_n = n())
    GEE_ET_tower_annual
  
#Create the model
ETmod <- lm(ET_UCIsites_annual ~ ET.predict.annual, GEE_ET_tower_annual)

#Calclulate RSME
ETmodRMSE <- sqrt(mean(ETmod$residuals^2))
ETmodRMSE

#Calculate R-squared
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
                           xlim(0,1200) + ylim(0,1200) + 
                           xlab(expression('Observed ET (mm year'^-1*')')) + ylab(expression('Predicted ET (mm year'^-1*')'))

ggsave(filename = 'figS3_ET_prediction_testing.png', height=16, width= 16, units = 'cm', dpi=900)
