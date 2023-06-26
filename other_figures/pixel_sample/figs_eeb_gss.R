#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: June 27, 2022
#Purpose: Create figures for EEB GSS presentation

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
setwd('C:/Users/can02/mystuff/fireDieoff/pixel_sample')

#The data directory
dir_in <- "D:\\Large_Files\\CECS\\Stand_Age"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
# pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_stand_age_no_fire_history_mask_01242022_30m.csv"), header = TRUE, na.strings = "NaN")
pixel.data <- read.csv(file.path(fire_in, "fraprx_stratified_sample_0pt5pct_30m_ts4_20220623"), header = TRUE, na.strings = "NaN")
summary(pixel.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))

#Convert missing TPA data to NAs
pixel.data[pixel.data$tpa_max == -9999,]$tpa_max <- NA

#Convert to trees per hectare
pixel.data$tpa_max <- pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
pixel.data$date <- as.Date(pixel.data$date)
pixel.data$vi.year <- format(pixel.data$date , '%Y')
pixel.data$fire.year <- pixel.data$fire_year
pixel.data$stand.age <- as.numeric(pixel.data$vi.year) - as.numeric(pixel.data$fire.year) 

#Update Cover data to 100% scale
pixel.data$Tree_Cover <- pixel.data$Tree_Cover / 100
pixel.data$Shrub_Cover <- pixel.data$Shrub_Cover / 100
pixel.data$Herb_Cover <- pixel.data$Herb_Cover / 100
pixel.data$Bare_Cover <- pixel.data$Bare_Cover / 100

#Try to fix soil moisture by dividing by 10
pixel.data$Soil_Moisture <- pixel.data$Soil_Moisture / 10

#Create a GAM to predict NDMI by stand.age
# ndmi.gam <- gam(data = filter(pixel.data, vi.year <= 2012 & stand.age > 0), #fire.year >= 1919 & 
#                 formula = NDMI ~ s(stand.age, bs = "cs", k = 5) + s(clm_precip_sum, k = 3) + s(clm_temp_mean, k = 3) + s(latitude, k = 3))
# summary(ndmi.gam)
# #Add a new column
# pixel.data$NDMI.predict <- NA
# pixel.data$NDMI.predict[pixel.data$stand.age <= 0] <- filter(pixel.data, stand.age <= 0)$NDMI
# pixel.data$NDMI.predict[pixel.data$stand.age > 0] <- predict(newdata = filter(pixel.data, stand.age > 0), object = ndmi.gam) #, header = TRUE, na.strings = "NaN")

#Calculate the Quintiles of precip climate normals
precip.q <- as.data.frame(unname(quantile(pixel.data$clm_precip_sum, prob = seq(0,1, 1/5))))
# precip.q
colnames(precip.q) <- 'Precip'
precip.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
precip.q

# precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric()

#Plot a histogram with precip quartiles.
# ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = clm_precip_sum)) +  
#   geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') + 
#   geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.4) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') +
#   geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') +
#   geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.8) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black')
# 
# ggsave(filename = 'Fig1_Precip_Quartiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)


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
# ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = clm_temp_mean)) +
#   geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.2) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') + 
#   geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.4) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') +
#   geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.6) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') +
#   geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.8) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black')
# 
# ggsave(filename = 'Fig2_Temp_Quartiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)
						
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
# ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = elevation)) +
#   geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.2) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') + 
#   geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.4) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') +
#   geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.6) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') +
#   geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.8) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black')
# 
# ggsave(filename = 'Fig3_Elevation_Quintiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)

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

#New bins based on stand age
# pixel.data <- pixel.data %>% mutate(age.bin = case_when(
#   stand.age <= 0 ~ '-33-0',
#   stand.age > 0 & stand.age <= 10 ~ '1-10',
#   stand.age > 10 & stand.age <= 20 ~ '11-20',
#   stand.age > 20 & stand.age <= 30 ~ '21-30',
#   stand.age > 30 & stand.age <= 40 ~ '31-40',
#   stand.age > 40 & stand.age <= 50 ~ '41-50',
#   stand.age > 50 & stand.age <= 60 ~ '51-60',
#   stand.age > 60 & stand.age <= 70 ~ '61-70',
#   stand.age > 70 & stand.age <= 80 ~ '71-80',
#   stand.age > 80 & stand.age <= 90 ~ '81-90',
#   stand.age > 90  ~ '91+'))
# 
# ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = date)) + facet_wrap(~age.bin)

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
# pixel.data <- pixel.data %>% mutate(stand.age.bin = case_when(
#   # bin >= 1 ~ '1900',
#   # bin == 2 ~ '1909-1910',
#   bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
#   bin >= 1921 & bin <= 1930 ~ '85-94',
#   bin >= 1931 & bin <= 1940 ~ '75-84',
#   bin >= 1941 & bin <= 1950 ~ '65-74',
#   bin >= 1951 & bin <= 1960 ~ '55-64',
#   bin >= 1961 & bin <= 1970 ~ '45-54',
#   bin >= 1971 & bin <= 1980 ~ '35-44',
#   bin >= 1981 & bin <= 1990 ~ '25-34', 
#   bin >= 1991 & bin <= 2000 ~ '15-24',
#   bin >= 2001 & bin <= 2010 ~ '5-14', 
#   bin >= 2011 & bin <= 2020 ~'0-4'))

pixel.data <- pixel.data %>% mutate(stand.age.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  bin >= 1911 & bin <=  1934 ~ '1911-1934',#'81-95',
  # bin >= 1936 & bin <= 1950 ~ '65-79',
  # bin >= 1951 & bin <= 1965 ~ '50-64',
  # bin >= 1951 & bin <= 1960 ~ '55-64',
  bin >= 1935 & bin <= 1959 ~ '1935-1959',#'56-80',
  # bin >= 1971 & bin <= 1980 ~ '35-44',
  bin >= 1960 & bin <= 1984 ~ '1960-1984',#'31-55', 
  # bin >= 1991 & bin <= 2000 ~ '15-24',
  bin >= 1985 & bin <= 2010 ~ '1985-2010',#'5-30', 
  bin >= 2011 & bin <= 2020 ~ '2011-2020'))#'0-4'))


summary(pixel.data)
# pixel.data$dNDMI <- group_by
# pixel.data
#Make the bin lables in the correct order
pixel.data$elevation.control = with(pixel.data, factor(elevation.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$temp.control = with(pixel.data, factor(temp.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$precip.control = with(pixel.data, factor(precip.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))

#Make the years bin lables in the correct order
# pixel.data$age.bin = with(pixel.data, factor(age.bin, levels = c('-33-0','1-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70','71-80', '81-90', '91+')))

#Fire Year Bins
pixel.data$year.bin = with(pixel.data, factor(year.bin, levels = c('2011-2018','2001-2010','1991-2000','1981-1990','1971-1980',
                                                                   '1961-1970','1951-1960','1941-1950','1931-1940','1921-1930', 
                                                                   '1911-1920'))) #,'1909-1910','1900')))

#Fire Year Bins
# pixel.data$stand.age.bin = with(pixel.data, factor(stand.age.bin, levels = c('0-4','5-14','15-24','25-34','35-44',
#                                                                         '45-54','55-64','65-74','75-84','85-94','95-104')))
                                                                   # '1911-1920','1909-1910','1900')))
pixel.data$stand.age.bin = with(pixel.data, factor(stand.age.bin, levels = c('2011-2020', '1985-2010', '1960-1984', '1935-1959', '1911-1934')))#c('0-4','5-30','31-55','56-80',
                                                                             #'81-95')))

#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills
#Figure of mean Cover changes by stand age
p1 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -25 & stand.age <= 94 & !is.na(Shrub_Cover) & vi.year <= 2012) %>%
              group_by(stand.age) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub'), size = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -25 & stand.age <= 94 & !is.na(Shrub_Cover) & vi.year <= 2012) %>%
                group_by(stand.age) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, fill = "Shrub"), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -25 & stand.age <= 94 & !is.na(Tree_Cover) & vi.year <= 2012) %>%
              group_by(stand.age) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree'), size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -25 & stand.age <= 94 & !is.na(Tree_Cover) & vi.year <= 2012) %>%
                group_by(stand.age) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, fill = "Tree"), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -25 & stand.age <= 94 & !is.na(Herb_Cover) & vi.year <= 2012) %>%
              group_by(stand.age) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb'), size = 1) + 
  #Herb Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -25 & stand.age <= 94 & !is.na(Herb_Cover) & vi.year <= 2012) %>%
                group_by(stand.age) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, fill = "Herb"), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -25 & stand.age <= 94 & !is.na(Bare_Cover) & vi.year <= 2012) %>% 
              group_by(stand.age) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare'), size = 1) + 
  #Bare Cover 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age >= -25 & stand.age <= 94 & !is.na(Bare_Cover) & vi.year <= 2012) %>%
                group_by(stand.age) %>%
                summarize(Bare_Cover.mean = mean(Bare_Cover),
                          Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
              mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
  x = stand.age, fill = "Bare"), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p1

#Save the data
ggsave(filename = 'Fig1_veg_cover_stand_age.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Figure of Dead Trees per acre separated by fire years with time series
p2 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 2 & fire.year >= 1911 & fire.year <= 2010 & !is.na(tpa_max)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max)), mapping = aes(x = date, y = tpa_max.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age >= 2 & fire.year >= 1911 & fire.year <= 2010 & !is.na(tpa_max)) %>%
                group_by(date, stand.age.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
  #Do the Formating
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') 
p2
# atop()

# ggsave(filename = 'Fig25_ADS_dieoff_fire_year_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)


#Create the 
p3 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 2 & fire.year >= 1911 & !is.na(Tree_Cover) & fire.year <= 2010) %>%
              group_by(date, stand.age.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = date, y = Tree_Cover.mean, color = stand.age.bin, linetype = stand.age.bin), 
              size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age >= 2 & fire.year >= 1911 & fire.year <= 2010 & !is.na(Tree_Cover)) %>%
                group_by(date, stand.age.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
  #Do the Formating
scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Tree Cover (%)')) + xlab('Year')
p3

f1 <- ggarrange(p2, p3, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f1
#Save the data
ggsave(filename = 'Fig2_dieoff_tree_cover_stand_age_time_series.png', height=12, width= 14, units = 'cm', dpi=900)

#Create a water stress time series figure
p4 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 2 & fire.year >= 1911 & fire.year <= 2010 & !is.na(AET)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(AET.mean = mean(AET)), mapping = aes(x = date, y = AET.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1
  ) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age >= 2 & fire.year >= 1911 & fire.year <= 2010 & !is.na(AET)) %>%
                group_by(date, stand.age.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Water Use (AET; mm yr'^-1*')')) + xlab('Year') 
p4

#Create the Soil Moisture Panel
p5 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 2 & fire.year >= 1911 & !is.na(Soil_Moisture) & fire.year <= 2010) %>%
              group_by(date, stand.age.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture)), mapping = aes(x = date, y = Soil_Moisture.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1) + 
  #Soil Moisture 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age >= 2 & fire.year >= 1911 & fire.year <= 2010 & !is.na(Soil_Moisture)) %>%
                group_by(date, stand.age.bin) %>%
                summarize(Soil_Moisture.mean = mean(Soil_Moisture),
                          Soil_Moisture.sd = sd(Soil_Moisture), Soil_Moisture.n = n()),
              mapping = aes(ymin=Soil_Moisture.mean - 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            ymax=Soil_Moisture.mean + 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  guides(color = guide_legend(), linetype = guide_legend()) +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Soil Moisture (mm)')) + xlab('Year')
p5

#Create the Water Stress Panel
p6 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(stand.age >= 5 & fire.year >= 1911 & !is.na(Water_Stress) & fire.year <= 2010) %>%
              group_by(date, stand.age.bin) %>%
              summarize(Water_Stress.mean = mean(Water_Stress)), mapping = aes(x = date, y = Water_Stress.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age >= 2 & fire.year >= 1911 & fire.year <= 2010 & !is.na(Water_Stress)) %>%
                group_by(date, stand.age.bin) %>%
                summarize(Water_Stress.mean = mean(Water_Stress),
                          Water_Stress.sd = sd(Water_Stress), Water_Stress.n = n()),
              mapping = aes(ymin=Water_Stress.mean - 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            ymax=Water_Stress.mean + 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.35), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Water Stress (mm)')) + xlab('Year')
p6

f2 <- ggarrange(p4, p5, p6, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(1, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f2
#Save the data
ggsave(filename = 'Fig3_water_stress_stand_age_time_series.png', height=16, width= 16, units = 'cm', dpi=900)
 
