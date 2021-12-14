#Author: Carl Norlen
#Date Created: June 23, 2021
#Date Updated: November 22, 2021
#Purpose: Explore pixel sampling data.

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample')

#The data directory
dir_in <- "D:\\Large_Files\\CECS\\Stand_Age"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_stand_age_no_fire_history_mask_12092021_30m.csv"), header = TRUE, na.strings = "NaN")
summary(pixel.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))

#Convert missing TPA data to NAs
pixel.data[pixel.data$tpa_max == -9999,]$tpa_max <- NA

#Convert to trees per hectare
pixel.data$tpa_max <- pixel.data$tpa_max * 2.47105

# summary(pixel.data)
#Make the dates into date time format for R
pixel.data$date <- as.Date(pixel.data$date)
pixel.data$vi.year <- format(pixel.data$date , '%Y')
pixel.data$fire.year <- pixel.data$fire_year
# head(pixel.data)
pixel.data$stand.age <- as.numeric(pixel.data$vi.year) - as.numeric(pixel.data$fire.year) 
.
# summary(pixel.data %>% filter(bin == 13))

#Create a GAM to predict NDMI by stand.age
ndmi.gam <- gam(data = filter(pixel.data, vi.year <= 2012 & stand.age > 0), #fire.year >= 1919 & 
                formula = NDMI ~ s(stand.age, bs = "cs", k = 5) + s(clm_precip_sum, k = 3) + s(clm_temp_mean, k = 3) + s(latitude, k = 3))
summary(ndmi.gam)
#Add a new column
pixel.data$NDMI.predict <- NA
pixel.data$NDMI.predict[pixel.data$stand.age <= 0] <- filter(pixel.data, stand.age <= 0)$NDMI
pixel.data$NDMI.predict[pixel.data$stand.age > 0] <- predict(newdata = filter(pixel.data, stand.age > 0), object = ndmi.gam) #, header = TRUE, na.strings = "NaN")

#Calculate the Quintiles of precip climate normals
precip.q <- as.data.frame(unname(quantile(pixel.data$clm_precip_sum, prob = seq(0,1, 1/5))))
# precip.q
colnames(precip.q) <- 'Precip'
precip.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
precip.q

# precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric()

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
pixel.data
#Bin data by elevation
# pixel.data <- pixel.data %>% mutate(year.control = case_when(fire.year > 2000 ~ '2001-2020',
# 															 fire.year > 1980 & fire.year <= 2000 ~ '1981-2000',
# 															 fire.year > 1960 & fire.year <= 1980 ~ '1961-1980',
# 															 fire.year > 1940 & fire.year <= 1960 ~ '1941-1960',
# 															 fire.year > 1920 & fire.year <= 1940 ~ '1921-1940',
# 															 fire.year >= 1900 & fire.year <= 1920 ~ '1900-1920')) # end function

#New bins based on stand age
pixel.data <- pixel.data %>% mutate(age.bin = case_when(
  stand.age <= 0 ~ '-33-0',
  stand.age > 0 & stand.age <= 10 ~ '1-10',
  stand.age > 10 & stand.age <= 20 ~ '11-20',
  stand.age > 20 & stand.age <= 30 ~ '21-30',
  stand.age > 30 & stand.age <= 40 ~ '31-40',
  stand.age > 40 & stand.age <= 50 ~ '41-50',
  stand.age > 50 & stand.age <= 60 ~ '51-60',
  stand.age > 60 & stand.age <= 70 ~ '61-70',
  stand.age > 70 & stand.age <= 80 ~ '71-80',
  stand.age > 80 & stand.age <= 90 ~ '81-90',
  stand.age > 90  ~ '91+'))

ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = date)) + facet_wrap(~age.bin)

#Create new name for data bins
#Bin names will need to be updated
pixel.data <- pixel.data %>% mutate(year.bin = case_when(
														 # bin >= 1 ~ '1900',
														 # bin == 2 ~ '1909-1910',
														 bin >= 1911 & bin <= 1920 ~ '1911-1920',
														 bin >= 1921 & bin <= 1930 ~ '1921-1930',
														 bin >= 1931 & bin <= 1940 ~ '1931-1940',
														 bin >= 1941 & bin <= 1950 ~ '1941-1950',
														 bin >= 1951 & bin <= 1960 ~ '1951-1960',
														 bin >= 1961 & bin <= 1970 ~ '1961-1970',
														 bin >= 1971 & bin <= 1980 ~ '1971-1980',
														 bin >= 1981 & bin <= 1990 ~ '1981-1990', 
														 bin >= 1991 & bin <= 2000 ~ '1991-2000',
														 bin >= 2001 & bin <= 2010 ~ '2001-2010', 
														 bin >= 2011 & bin <= 2020 ~'2011-2018')) # end function

#Update this to be a stand age bin, calculated for fire year relative to 2015
pixel.data <- pixel.data %>% mutate(stand.age.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  bin >= 1921 & bin <= 1930 ~ '85-94',
  bin >= 1931 & bin <= 1940 ~ '75-84',
  bin >= 1941 & bin <= 1950 ~ '65-74',
  bin >= 1951 & bin <= 1960 ~ '55-64',
  bin >= 1961 & bin <= 1970 ~ '45-54',
  bin >= 1971 & bin <= 1980 ~ '35-44',
  bin >= 1981 & bin <= 1990 ~ '25-34', 
  bin >= 1991 & bin <= 2000 ~ '15-24',
  bin >= 2001 & bin <= 2010 ~ '5-14', 
  bin >= 2011 & bin <= 2020 ~'0-4'))


summary(pixel.data)
# pixel.data$dNDMI <- group_by
# pixel.data
#Make the bin lables in the correct order
pixel.data$elevation.control = with(pixel.data, factor(elevation.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$temp.control = with(pixel.data, factor(temp.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$precip.control = with(pixel.data, factor(precip.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))

#Make the years bin lables in the correct order
pixel.data$age.bin = with(pixel.data, factor(age.bin, levels = c('-33-0','1-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70','71-80', '81-90', '91+')))

#Fire Year Bins
pixel.data$year.bin = with(pixel.data, factor(year.bin, levels = c('2011-2018','2001-2010','1991-2000','1981-1990','1971-1980',
                                                                   '1961-1970','1951-1960','1941-1950','1931-1940','1921-1930', 
                                                                   '1911-1920'))) #,'1909-1910','1900')))

#Fire Year Bins
pixel.data$stand.age.bin = with(pixel.data, factor(stand.age.bin, levels = c('0-4','5-14','15-24','25-34','35-44',
                                                                        '45-54','55-64','65-74','75-84','85-94','95-104')))
                                                                   # '1911-1920','1909-1910','1900')))

#Calculate dNDMI based on predictions
pixel.data$dNDMI <- pixel.data$NDMI - pixel.data$NDMI.predict

#NDMI Time Series separated by temperature climatology
p5 <- ggplot(data = filter(pixel.data, stand.age > 0), mapping = aes(x = date, y = NDMI)) + geom_bin2d(alpha = 0.4) + 
      geom_smooth(se = TRUE, color = 'black') + facet_wrap(~ temp.control, ncol = 5)
p5
ggsave(filename = 'Fig5_NDMI_Time_Series_by_temp.png', height=12.5, width= 20, units = 'cm', dpi=900)

#NDMI Time Series separated by percip climatology
p6 <- ggplot(data = filter(pixel.data, stand.age > 0), mapping = aes(x = date, y = NDMI)) + geom_bin2d(alpha = 0.4) + 
      geom_smooth(se = TRUE, color = 'black') + facet_wrap(~ precip.control, ncol = 5)
p6
ggsave(filename = 'Fig6_NDMI_Time_Series_by_precip.png', height=12.5, width= 20, units = 'cm', dpi=900)

#NDMI Time Series separated by elevation 
p7 <- ggplot(data = filter(pixel.data, stand.age > 0), mapping = aes(x = date, y = NDMI)) + geom_bin2d(alpha = 0.4) + 
      geom_smooth(se = TRUE, color = 'black') + facet_wrap(~ elevation.control, ncol = 5)
p7
ggsave(filename = 'Fig7_NDMI_Time_Series_by_elevation.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Stand Age fire year test
p8 <- ggplot(data = filter(pixel.data, stand.age > 0), mapping = aes(x = date, y = stand.age)) +
  geom_bin2d(alpha = 0.4) + geom_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = 'black') +  facet_wrap(~ year.bin, ncol = 5)
p8

ggsave(filename = 'Fig8_stand_age_test.png', height=12.5, width= 20, units = 'cm', dpi=900)

# print(temp.q %>% filter == 0.2)
# temp.q[2,1]
# elev.q[5,1]
# elev.q
#Filtering levels based on climate
#& (clm_temp_mean > temp.q[2,1] & elevation < elev.q[5,1] & clm_precip_sum > precip.q[2,1])
#Exploratory figure of NDMI Time Series by stand age with a GAM fit
p9 <- ggplot(data = filter(pixel.data, vi.year <= 2012 & stand.age >= -15), mapping = aes(x = stand.age, y = NDMI)) + geom_bin2d() + #& clm_temp_mean != temp.q & elevation != elev.q & clm_precip_sum != precip.q
  geom_smooth(data = filter(pixel.data, vi.year <= 2012 & stand.age >= 0), method = 'gam', formula = y ~ s(x, bs = "cs", k = 5), se = FALSE, color = 'black') + #, mapping = aes(color = precip.control)
  xlab('Years Since Fire') +
  geom_vline(xintercept = 0) +
  scale_fill_gradient2(limits = c(10,1200), breaks = c(10,300,600,900), midpoint = 600, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent')
p9

ggsave(filename = 'Fig9_NDMI_Chrono_Sequence_filtered.png', height=12.5, width= 16, units = 'cm', dpi=900)

#Exploratory figure of NDMI Time Series by stand age with a GAM fit
p10 <- ggplot(data = filter(pixel.data, vi.year <= 2012 & stand.age >= -15), mapping = aes(x = stand.age, y = NDMI)) + geom_bin2d() +
  geom_smooth(data = filter(pixel.data, vi.year <= 2012 & stand.age > 0), method = 'gam', formula = y ~ s(x, bs = "cs", k = 5), se = FALSE, mapping = aes(color = precip.control)) + #, mapping = aes(color = precip.control)
  scale_color_grey(name = "Precip Normal \n(Quintiles)") + xlab('Years Since Fire') +
  geom_vline(xintercept = 0) +
  scale_fill_gradient2(limits = c(10,1200), breaks = c(10,300,600,900), midpoint = 600, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent')
p10

ggsave(filename = 'Fig10_NDMI_Chrono_Sequence_filtered.png', height=12.5, width= 16, units = 'cm', dpi=900)

#Exploratory figure of NDMI Time Series by stand age with a GAM fit
p11 <- ggplot(data = filter(pixel.data, vi.year <= 2012 & stand.age >= -15), mapping = aes(x = stand.age, y = NDMI)) + geom_bin2d() +
  geom_smooth(data = filter(pixel.data, vi.year <= 2012 & stand.age > 0), method = 'gam', formula = y ~ s(x, bs = "cs", k = 5), se = FALSE, mapping = aes(color = temp.control)) + #, mapping = aes(color = precip.control)
  scale_color_grey(name = "Temp Normal \n(Quintiles)") + xlab('Years Since Fire') +
  geom_vline(xintercept = 0) +
  scale_fill_gradient2(limits = c(10,1200), breaks = c(10,300,600,900), midpoint = 600, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent')

p11

ggsave(filename = 'Fig11_NDMI_Chrono_Sequence_filtered.png', height=12.5, width= 16, units = 'cm', dpi=900)

#Exploratory figure of NDMI Time Series by stand age with a GAM fit with elevation
p12 <- ggplot(data = filter(pixel.data, vi.year <= 2012 & stand.age >= -15), mapping = aes(x = stand.age, y = NDMI)) + geom_bin2d() +
  geom_smooth(data = filter(pixel.data, vi.year <= 2012 & stand.age > 0), method = 'gam', formula = y ~ s(x, bs = "cs", k = 5), se = FALSE, mapping = aes(color = elevation.control)) + #, mapping = aes(color = precip.control)
  scale_color_grey(name = "Elevation \n(Quintiles)") + xlab('Years Since Fire') +
  geom_vline(xintercept = 0) +
  scale_fill_gradient2(limits = c(10,1200), breaks = c(10,300,600,900), midpoint = 600, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent')

p12

ggsave(filename = 'Fig12_NDMI_Chrono_Sequence_filtered_elevation.png', height=12.5, width= 16, units = 'cm', dpi=900)

#NDMI (Predict) Time Series
p17 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(NDMI.predict) & fire.year <= 2010), mapping = aes(x = date, y = NDMI.predict)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year <= 2010 & !is.na(NDMI.predict)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(NDMI.predict.mean = mean(NDMI.predict)), mapping = aes(x = date, y = NDMI.predict.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,400), breaks = c(0,100,200,300), midpoint = 200, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab('NDMI (Predicted)') + xlab('Year') + 
  theme_bw() #+ ylim(c(-0.3, 0.6)) 
p17

ggsave(filename = 'Fig17_NDMI_predict_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#NDMI Time Series
p18 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(NDMI) & fire.year <= 2010), mapping = aes(x = date, y = NDMI)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year <= 2010 & !is.na(NDMI)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(NDMI.mean = mean(NDMI)), mapping = aes(x = date, y = NDMI.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(10,300), breaks = c(10,100,200), midpoint = 150, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab('NDMI') + xlab('Year') + 
  theme_bw() + ylim(c(-0.3, 0.6)) 
p18

ggsave(filename = 'Fig18_NDMI_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of dNDMI separated by fire years
p19 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(dNDMI) & fire.year <= 2010), mapping = aes(x = date, y = dNDMI)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year <= 2010 & !is.na(dNDMI)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(dNDMI.mean = mean(dNDMI)), mapping = aes(x = date, y = dNDMI.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(10,200), breaks = c(10,100), midpoint = 100, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab('dNDMI') + xlab('Year') + 
  ylim(c(-0.4, 0.4)) + theme_bw()
p19

ggsave(filename = 'Fig19_dNDMI_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Fire year compared to latitude
p20 <- ggplot(data = filter(pixel.data, fire.year >= 1911), mapping = aes(x = fire.year, y = latitude)) + 
  geom_bin2d(alpha = 0.8)

p20

ggsave(filename = 'Fig20_fire_year_latitude.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of dNDMI separated by fire years with time series
#This one isn't working for some reason.
p21 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(dNDMI) & fire.year >= 1911 & fire.year <= 2010), mapping = aes(x = date, y = dNDMI)) + 
  geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  # geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year >= 1911 & fire.year <= 2010 & !is.na(dNDMI)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(dNDMI.mean = mean(dNDMI)), mapping = aes(x = date, y = dNDMI.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(10,200), breaks = c(10,100), midpoint = 100, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab('dNDMI') + xlab('Year') + 
  ylim(c(-0.4, 0.4)) + theme_bw()
p21

ggsave(filename = 'Fig21_dNDMI_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of Water Stress separated by fire years
p22 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(Water_Stress) & fire.year >= 1911 & fire.year <= 2010), mapping = aes(x = date, y = Water_Stress)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year >= 1911 & fire.year <= 2010 & !is.na(Water_Stress)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(Water_Stress.mean = mean(Water_Stress)), mapping = aes(x = date, y = Water_Stress.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,2000), breaks = c(500,1000,1500), midpoint = 1000, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab(expression('Water Stress (mm yr'^-1*')')) + xlab('Year') + 
  ylim(-650, 40) + theme_bw()
p22

ggsave(filename = 'Fig22_Water_Stress_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of Soil Moisture separated by fire years
p23 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(Soil_Moisture) & fire.year >= 1911 & fire.year <= 2010), mapping = aes(x = date, y = Soil_Moisture)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year >= 1911 & fire.year <= 2010 & !is.na(Soil_Moisture)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture)), mapping = aes(x = date, y = Soil_Moisture.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,300), breaks = c(100,200), midpoint = 150, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab('Soil Moisture (mm)') + xlab('Year') + 
  theme_bw()
p23

ggsave(filename = 'Fig23_Soil_Moisture_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

# summary(pixel.data)

#Figure of Biomass separated by fire years
p24 <- ggplot(data = filter(pixel.data, stand.age >= 0 & fire.year >= 1911 & fire.year <= 2010), mapping = aes(x = date, y = emapr_biomass)) + 
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year >= 1911 & fire.year <= 2010) %>%
              group_by(date, stand.age.bin) %>%
              summarize(emapr_biomass.mean = mean(emapr_biomass)), mapping = aes(x = date, y = emapr_biomass.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,300), breaks = c(75,150,225), midpoint = 150, 
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab(expression('Biomass (Mg ha'^-1*')')) + xlab('Year') + 
  theme_bw()
p24

ggsave(filename = 'Fig24_Biomass_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of Dead Trees per acre separated by fire years with time series
p25 <- ggplot(data = filter(pixel.data, stand.age >= 0 & !is.na(tpa_max) & fire.year >= 1911 & fire.year <= 2010), mapping = aes(x = date, y = tpa_max)) + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_bin2d(alpha = 0.8) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 0 & fire.year >= 1911 & fire.year <= 2010 & !is.na(tpa_max)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max)), mapping = aes(x = date, y = tpa_max.mean), 
            color = 'black', size = 1
  ) +
  scale_fill_gradient2(limits = c(0,1000), breaks = c(200,400,600,800), midpoint = 500,
                       low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  facet_wrap(~ stand.age.bin, ncol = 5) + ylab(expression('Die-off (trees ha'^-1*')')) + xlab('Year') + 
  theme_bw()
p25

ggsave(filename = 'Fig25_ADS_dieoff_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)
