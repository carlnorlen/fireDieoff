#Author: Carl Norlen
#Date Created: December 6, 2021
#Date Updated: December 20, 2021
#Purpose: Explore pixel sampling data.

# cd /C/Users/Carl/mystuff/fireDieoff/severity
# cd /C/Users/can02/fireDieoff/severity
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/severity')

#The data directory
dir_in <- "D:\\Large_Files\\CECS\\Stand_Age"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_fire_year_fire_severity_12132021_30m.csv"), header = TRUE, na.strings = "NaN")
# summary(pixel.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))
summary(pixel.data)

#Convert missing TPA data to NAs
pixel.data[pixel.data$tpa_max == -9999,]$tpa_max <- NA

#Convert to trees per hectare
pixel.data$tpa_max <- pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
pixel.data$date <- as.Date(pixel.data$date)
pixel.data$vi.year <- format(pixel.data$date , '%Y')
pixel.data$fire.year <- pixel.data$fire_year
# head(pixel.data)
pixel.data$stand.age <- as.numeric(pixel.data$vi.year) - as.numeric(pixel.data$fire.year) 

# summary(pixel.data %>% filter(bin == 13))

#Create a GAM to predict NDMI by stand.age
ndmi.gam <- gam(data = filter(pixel.data, vi.year <= 2012 & stand.age >= 0), #fire.year >= 1919 & 
                formula = NDMI ~ s(stand.age, bs = "cs", k = 5) + s(clm_precip_sum, k = 3) + s(clm_temp_mean, k = 3) + s(latitude, k = 3) + s(fire_sev_last, k = 3))
# summary(ndmi.gam)
#Add a new column
pixel.data$NDMI.predict <- NA
pixel.data$NDMI.predict[pixel.data$stand.age <= 0] <- filter(pixel.data, stand.age <= 0)$NDMI
pixel.data$NDMI.predict[pixel.data$stand.age > 0] <- predict(newdata = filter(pixel.data, stand.age > 0), object = ndmi.gam) 

#Calculate the Quintiles of precip climate normals
precip.q <- as.data.frame(unname(quantile(pixel.data$clm_precip_sum, prob = seq(0,1, 1/5))))
# precip.q
colnames(precip.q) <- 'Precip'
precip.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
precip.q

# precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric()

#Plot a histogram with precip quartiles.
ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = clm_precip_sum)) +  
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.4) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.8) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black')

ggsave(filename = 'Fig1_Precip_Quartiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)


#Bin data by precip climatology
pixel.data <- pixel.data %>% mutate(precip.control = case_when(
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.8) %>% dplyr::select(Precip) %>% as.numeric() ~ '> 80 %',
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric() & 
    clm_precip_sum < precip.q %>% filter(Quartile == 0.8) %>% dplyr::select(Precip) %>% as.numeric() ~ '60 to 80 %',
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.4) %>% dplyr::select(Precip) %>% as.numeric() & 
    clm_precip_sum < precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric() ~ '40 to 60 %',
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric()  & 
    clm_precip_sum < precip.q %>% filter(Quartile == 0.4) %>% dplyr::select(Precip) %>% as.numeric() ~ '20 to 40 %',
  clm_precip_sum < precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric() ~ '0 to 20%'))


#Calculate the Quintiles of temperature climate normals
temp.q <- as.data.frame(unname(quantile(pixel.data$clm_temp_mean, prob = seq(0,1, 1/5))))
# precip.q
colnames(temp.q) <- 'Temp'
temp.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# temp.q

#Histogram Plot of temperature quantiles
ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = clm_temp_mean)) +
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.2) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.4) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.6) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.8) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black')

ggsave(filename = 'Fig2_Temp_Quartiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Create temperature bins for analysis with quantiles
pixel.data <- pixel.data %>% mutate(temp.control = case_when(
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.8) %>% dplyr::select(Temp) %>% as.numeric() ~ '> 80 %',
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.6) %>% dplyr::select(Temp) %>% as.numeric() & 
    clm_temp_mean < temp.q %>% filter(Quartile == 0.8) %>% dplyr::select(Temp) %>% as.numeric() ~ '60 to 80 %',
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.4) %>% dplyr::select(Temp) %>% as.numeric() & 
    clm_temp_mean < temp.q %>% filter(Quartile == 0.6) %>% dplyr::select(Temp) %>% as.numeric() ~ '40 to 60 %',
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.2) %>% dplyr::select(Temp) %>% as.numeric()  & 
    clm_temp_mean < temp.q %>% filter(Quartile == 0.4) %>% dplyr::select(Temp) %>% as.numeric() ~ '20 to 40 %',
  clm_temp_mean < temp.q %>% filter(Quartile == 0.2) %>% dplyr::select(Temp) %>% as.numeric() ~ '0 to 20%'))

#Calculate the Quintiles of elevation
elev.q <- as.data.frame(unname(quantile(pixel.data$elevation, prob = seq(0,1, 1/5))))
# precip.q
colnames(elev.q) <- 'elevation'
elev.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# temp.q
elev.q
#Histogram Plot of temperature quantiles
ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = elevation)) +
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.2) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.4) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.6) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.8) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black')

ggsave(filename = 'Fig3_Elevation_Quintiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Bin data by elevation. Bins are quintiles.
pixel.data <- pixel.data %>% mutate(elevation.control = case_when(
  elevation >= elev.q %>% filter(Quartile == 0.8) %>% dplyr::select(elevation) %>% as.numeric() ~ '> 80 %',
  elevation >= elev.q %>% filter(Quartile == 0.6) %>% dplyr::select(elevation) %>% as.numeric() & 
    elevation < elev.q %>% filter(Quartile == 0.8) %>% dplyr::select(elevation) %>% as.numeric() ~ '60 to 80 %',
  elevation >= elev.q %>% filter(Quartile == 0.4) %>% dplyr::select(elevation) %>% as.numeric() & 
    elevation < elev.q %>% filter(Quartile == 0.6) %>% dplyr::select(elevation) %>% as.numeric() ~ '40 to 60 %',
  elevation >= elev.q %>% filter(Quartile == 0.2) %>% dplyr::select(elevation) %>% as.numeric()  & 
    elevation < elev.q %>% filter(Quartile == 0.4) %>% dplyr::select(elevation) %>% as.numeric() ~ '20 to 40 %',
  elevation < elev.q %>% filter(Quartile == 0.2) %>% dplyr::select(elevation) %>% as.numeric() ~ '0 to 20%'))
# pixel.data
#Bin data by elevation
# pixel.data <- pixel.data %>% mutate(year.control = case_when(fire.year > 2000 ~ '2001-2020',
# 															 fire.year > 1980 & fire.year <= 2000 ~ '1981-2000',
# 															 fire.year > 1960 & fire.year <= 1980 ~ '1961-1980',
# 															 fire.year > 1940 & fire.year <= 1960 ~ '1941-1960',
# 															 fire.year > 1920 & fire.year <= 1940 ~ '1921-1940',
# 															 fire.year >= 1900 & fire.year <= 1920 ~ '1900-1920')) # end function
# summary(pixel.data)

#New bins based on stand age
pixel.data <- pixel.data %>% mutate(age.bin = case_when(
  stand.age >= -32 & stand.age < -20 ~ '-32 to -21',
  stand.age >= -20 & stand.age < -10 ~ '-20 to -11',
  stand.age >= -10 & stand.age < 0 ~ '-1 to -10',
  stand.age >= 0 & stand.age <= 10 ~ '0 to 10',
  stand.age > 10 & stand.age <= 20 ~ '11 to 20',
  stand.age > 20 & stand.age <= 30 ~ '21 to 30',
  stand.age > 30 & stand.age <= 40 ~ '31 to 34'))

# pixel.data
#Stand Age years
p6 <- ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = date)) + facet_wrap(~age.bin)
p6

ggsave(filename = 'Fig6_Stand_age_bins.png', height=12.5, width= 16, units = 'cm', dpi=900)

#Create new name for data bins
#Bin names will need to be updated
pixel.data <- pixel.data %>% mutate(year.bin = case_when(
  fire_year_last >= 1981 & fire_year_last <= 1990 ~ '1984-1990', 
  fire_year_last >= 1991 & fire_year_last <= 2000 ~ '1991-2000',
  fire_year_last >= 2001 & fire_year_last <= 2010 ~ '2001-2010', 
  fire_year_last >= 2011 & fire_year_last <= 2020 ~'2011-2017')) # end function
# summary(pixel.data)
# 
# pixel.data %>% filter(fire_sev_last == 1)

#Fire Severity Bins
pixel.data <- pixel.data %>% mutate(sev.bin = case_when(
                                    fire_sev_last == '0' ~ 'No Fire',
                                    fire_sev_last == '1' ~ 'Lowest', 
                                    fire_sev_last == '2' ~ 'Low',
                                    fire_sev_last == '3' ~ 'Mid',
                                    fire_sev_last == '4' ~ 'High',
                                    fire_sev_last == '255' ~ 'Masked')) # end function
pixel.data
#Make the bin lables in the correct order
pixel.data$elevation.control = with(pixel.data, factor(elevation.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$temp.control = with(pixel.data, factor(temp.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$precip.control = with(pixel.data, factor(precip.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))

#Make the years bin lables in the correct order
pixel.data$sev.bin = with(pixel.data, factor(sev.bin, levels = c('Masked', 'Lowest', 'Low','Mid', 'High')))

#Burn Severity Bin
pixel.data$age.bin = with(pixel.data, factor(age.bin, levels = c('-32 to -21', '-20 to -11', '-10 to -1','0 to 10', '11 to 20', '21 to 30', '31 to 34')))

#Fire Year Bins
pixel.data$year.bin = with(pixel.data, factor(year.bin, levels = c('2011-2017','2001-2010','1991-2000','1984-1990')))

#Calculate dNDMI based on predictions
pixel.data$dNDMI <- pixel.data$NDMI - pixel.data$NDMI.predict
summary(pixel.data)
#Exploratory figure of NDMI Time Series by stand age with a GAM fit )
p1 <- ggplot(data = filter(pixel.data, vi.year <= 2012 & stand.age >= -5 & !is.na(NDMI) & fire_sev_last != 255), mapping = aes(x = stand.age, y = NDMI)) + geom_bin2d() +
  geom_smooth(data = filter(pixel.data, vi.year <= 2012 & stand.age >= 0 & !is.na(NDMI) & fire_sev_last != 255), method = 'gam', formula = y ~ s(x, bs = "cs", k = 5), se = FALSE, mapping = aes(color = sev.bin)) +
  geom_vline(xintercept = 0) + xlab('Year') +
  scale_fill_gradient2(limits = c(0,800), breaks = c(200,400,600), midpoint = 400, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent')
p1

ggsave(filename = 'Fig1_NDMI_Chrono_Sequence_filtered.png', height=12.5, width= 16, units = 'cm', dpi=900)

#Time series of NDMI
p2 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(NDMI) & fire.year <= 2010 & fire_sev_last != 255), mapping = aes(x = date, y = NDMI)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age > 0 & fire.year <= 2010 & !is.na(NDMI) & fire_sev_last != 255) %>%
              group_by(date, year.bin, sev.bin) %>%
              summarize(NDMI.mean = mean(NDMI)), mapping = aes(x = date, y = NDMI.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,120), breaks = c(30,60,90), midpoint = 60, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_grid(factor(sev.bin, levels = c("Lowest", "Low", "Mid", "High")) ~ year.bin) + ylab('NDMI') + xlab('Year') + 
  ylim(c(-0.4, 0.4)) + theme_bw()
p2

ggsave(filename = 'Fig2_NDMI_fire_year_time_series.png', height=18, width= 20, units = 'cm', dpi=900)

#Time series of NDMI (Predict)
p3 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(NDMI.predict) & fire.year <= 2010 & fire_sev_last != 255), mapping = aes(x = date, y = NDMI.predict)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age > 0 & fire.year <= 2010 & !is.na(NDMI.predict) & fire_sev_last != 255) %>%
              group_by(date, year.bin, sev.bin) %>%
              summarize(NDMI.predict.mean = mean(NDMI.predict)), mapping = aes(x = date, y = NDMI.predict.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,120), breaks = c(30,60,90), midpoint = 60, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_grid(factor(sev.bin, levels = c("Lowest", "Low", "Mid", "High")) ~ year.bin) + ylab('NDMI (Predict)') + xlab('Year') + 
  ylim(c(-0.4, 0.4)) + theme_bw()
p3

ggsave(filename = 'Fig3_NDMI_predict_fire_year_time_series.png', height=18, width= 20, units = 'cm', dpi=900)

#Time series of dNDMI
p4 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(dNDMI) & fire.year <= 2010 & fire_sev_last != 255), mapping = aes(x = date, y = dNDMI)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age > 0 & fire.year <= 2010 & !is.na(dNDMI) & fire_sev_last != 255) %>%
              group_by(date, year.bin, sev.bin) %>%
              summarize(dNDMI.mean = mean(dNDMI)), mapping = aes(x = date, y = dNDMI.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,120), breaks = c(30,60,90), midpoint = 60, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_grid(factor(sev.bin, levels = c("Lowest", "Low", "Mid", "High")) ~ year.bin) + ylab('dNDMI') + xlab('Year') + 
  ylim(c(-0.4, 0.4)) + theme_bw()
p4

ggsave(filename = 'Fig4_dNDMI_fire_year_time_series.png', height=18, width= 20, units = 'cm', dpi=900)

#Figure of Water Stress separated by fire years
p5 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(Water_Stress) & fire.year <= 2010 & fire_sev_last != 255), mapping = aes(x = date, y = Water_Stress)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age > 0 & fire.year <= 2010 & !is.na(Water_Stress) & fire_sev_last != 255) %>%
              group_by(date, year.bin, sev.bin) %>%
              summarize(Water_Stress.mean = mean(Water_Stress)), mapping = aes(x = date, y = Water_Stress.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,500), breaks = c(125,250,375), midpoint = 250, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_grid(factor(sev.bin, levels = c("Lowest", "Low", "Mid", "High")) ~ year.bin) + ylab(expression('Water Stress (mm yr'^-1*')')) + xlab('Year') + 
  ylim(-650, 40) + theme_bw()
p5

ggsave(filename = 'Fig5_Water_Stress_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of Soil Moisture separated by fire years
p6 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(Soil_Moisture) & fire.year <= 2010 & fire_sev_last != 255), mapping = aes(x = date, y = Soil_Moisture)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age > 0 & fire.year <= 2010 & !is.na(Soil_Moisture) & fire_sev_last != 255) %>%
              group_by(date, year.bin, sev.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture)), mapping = aes(x = date, y = Soil_Moisture.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,140), breaks = c(35,70,105), midpoint = 70, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_grid(factor(sev.bin, levels = c("Lowest", "Low", "Mid", "High")) ~ year.bin) + ylab('Soil Moisture (mm)') + xlab('Year') + 
  theme_bw()
p6

ggsave(filename = 'Fig6_Soil_Moisture_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of Biomass separated by fire years
p7 <- ggplot(data = filter(pixel.data, stand.age >= 0 & fire.year <= 2010  & fire_sev_last != 255), mapping = aes(x = date, y = emapr_biomass)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age > 0 & fire.year <= 2010  & fire_sev_last != 255) %>%
              group_by(date, year.bin, sev.bin) %>%
              summarize(emapr_biomass.mean = mean(emapr_biomass)), mapping = aes(x = date, y = emapr_biomass.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,300), breaks = c(75,150,225), midpoint = 150, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_grid(sev.bin ~ year.bin) + ylab(expression('Biomass (Mg ha'^-1*')')) + xlab('Year') + 
  theme_bw()
p7

ggsave(filename = 'Fig7_Biomass_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

summary(pixel.data)

#Figure of Dead Trees per acre separated by fire years with time series
p8 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(tpa_max) & fire.year <= 2010 & fire_sev_last != 255), mapping = aes(x = date, y = tpa_max)) + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year <= 2010 & !is.na(tpa_max) & fire_sev_last != 255) %>%
              group_by(date, sev.bin, year.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max)), mapping = aes(x = date, y = tpa_max.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,700), breaks = c(200,400,600), midpoint = 350,
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_grid(sev.bin ~ year.bin) + ylab(expression('Die-off (trees ha'^-1*')')) + xlab('Year') + 
  theme_bw()
p8

ggsave(filename = 'Fig8_ADS_dieoff_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)
