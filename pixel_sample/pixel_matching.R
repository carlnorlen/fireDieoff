#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: October 28, 2022
#Purpose: Calculate the mahalanobis distance between two data sets.

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'MatchIt', 'optmatch', 'Matching', 'rgenoud', 'rdist')
# install.packages('rdist',repo='https://cran.r-project.org/')

# install.packages(c('Matching', 'rgenoud'),repo='https://cran.r-project.org/')
# library('rdist')
lapply(p,require,character.only=TRUE)
# require('MatchIt')
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/pixel_sample')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
# pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_stand_age_no_fire_history_mask_01242022_30m.csv"), header = TRUE, na.strings = "NaN")
# pixel.data <- read.csv(file.path(dir_in, "fraprx_ecoregion_stratified_sample_100pts_30m_ts8_20220713.csv"), header = TRUE, na.strings = "NaN")
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_100pts_100elev_200precip_5temp_ts8_300m_20221027.csv"), header = TRUE, na.strings = "NaN")
# list.files(fire_in)
control.data <- read.csv(file.path(dir_in, "control_south_sierra_no_FRAP_undist_100pts_100elev_200precip_5temp_ts8_300m_20221025.csv"), header = TRUE, na.strings = "NaN")

summary(sample.data)

`%notin%` <- Negate(`%in%`)

#Convert data to long format
# rx.data <- sample.data %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
#                pivot_longer(cols = X10_AET_mean:X9_tpa_max_mode, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")
#add some columns (Treatment = 1, and Control = 0)
rx.data$treat <- 1
# sample.data$fire.year <- sample.data$perimeter_year
# summary(sample.data)
#Control data
# control.data <- control.data %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
  pivot_longer(cols = X10_AET_mean:X9_tpa_max_mode, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")
#Add some columns
control.data$treat <- 0
# control.data$perimeter_year <- NA
# control.data$fire.year <- control.data$perimeter_year
# control.data$FIRE_NAME <- NA
# control.data$FIRE_NUM <- NA
control.data$fire_type_2010 <- -9999
control.data$fire_year_2010 <- -9999
control.data$fire_count_2010 <- -9999
control.data$fire_type_2019 <- -9999
control.data$fire_year_2019 <- -9999
control.data$fire_year_2019 <- -9999
control.data$fire_count_2019 <- -9999
control.data$fire_type_2020 <- -9999
control.data$fire_year_2020 <- -9999
control.data$fire_count_2020 <- -9999


#Combine the data together
rx.pixel.data <- rbind(rx.data, control.data)
# summary(all.data)

# all.data <- as.data.frame(all.data)
#Sample Data assignment
`%notin%` <- Negate(`%in%`)

#Convert data to long format
rx.pixel.data <- rx.pixel.data %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
  pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

rx.pixel.data$year <- as.numeric(rx.pixel.data$year) + 1984 

#Convert missing TPA data to NAs
rx.pixel.data[rx.pixel.data$tpa_max < 0,]$tpa_max <- NA

#Convert fire data -9999 to NAs
rx.pixel.data[rx.pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
rx.pixel.data[rx.pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
rx.pixel.data[rx.pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
rx.pixel.data[rx.pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
rx.pixel.data[rx.pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
rx.pixel.data[rx.pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA

#Convert to trees per hectare
rx.pixel.data$tpa_max <- rx.pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
rx.pixel.data$date <- as.Date(as.character(rx.pixel.data$year), format = '%Y')
# rx.pixel.data$vi.year <- format(rx.pixel.data$date , '%Y')
rx.pixel.data$vi.year <- rx.pixel.data$year
#Use the FRAP fire perimeter year
rx.pixel.data$fire.year <- rx.pixel.data$fire_year_2019
rx.pixel.data$stand.age <- as.numeric(rx.pixel.data$year) - as.numeric(rx.pixel.data$fire.year) 

#Update Cover data to 100% scale
rx.pixel.data$Tree_Cover <- rx.pixel.data$Tree_Cover / 100
rx.pixel.data$Shrub_Cover <- rx.pixel.data$Shrub_Cover / 100
rx.pixel.data$Herb_Cover <- rx.pixel.data$Herb_Cover / 100
rx.pixel.data$Bare_Cover <- rx.pixel.data$Bare_Cover / 100

#Convert the SPI48 scale back to decimal
rx.pixel.data$SPI48 <- rx.pixel.data$SPI48 / 100

#Try to fix soil moisture by dividing by 10
rx.pixel.data$Soil_Moisture <- rx.pixel.data$Soil_Moisture / 10

#Rename ppt and Water Stress
rx.pixel.data$Water_Stress <- rx.pixel.data$Water_Stress
rx.pixel.data$ppt <- rx.pixel.data$ppt
rx.pixel.data$AET <- rx.pixel.data$AET
rx.pixel.data$GPP <- rx.pixel.data$GPP
rx.pixel.data$elevation <- rx.pixel.data$elevation
rx.pixel.data$PrET <- rx.pixel.data$ppt - rx.pixel.data$AET

#Fire Year Bins
#' summary(all.data)
#' all.data <- all.data %>% mutate(stand.age.bin = case_when(
#'   # bin >= 1 ~ '1900',
#'   # bin == 2 ~ '1909-1910',
#'   # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
#'   is.na(fire.year) ~ 'No Fire',
#'   fire.year >= 1910 & fire.year <=  1934 ~ '1910-1934',#'81-95',
#'   # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
#'   # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
#'   # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
#'   fire.year >= 1935 & fire.year <= 1959 ~ '1935-1959',#'56-80',
#'   # fire.year >= 1971 & fire.year <= 1980 ~ '35-44',
#'   fire.year >= 1960 & fire.year <= 1984 ~ '1960-1984',#'31-55', 
#'   # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
#'   fire.year >= 1985 & fire.year <= 2010 ~ '1985-2010',
#'   # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
#'   fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
#'   fire.year >= 2019 ~ '2019-2020'))#'0-4'))
#' 
#' all.data <- all.data %>% mutate(fire.year.bin = case_when(
#'   # bin >= 1 ~ '1900',
#'   # bin == 2 ~ '1909-1910',
#'   # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
#'   is.na(fire.year) ~ 'No Fire',
#'   fire.year <  1920 ~ '1900-1919',#'81-95',
#'   # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
#'   # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
#'   # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
#'   fire.year >= 1920 & fire.year <= 1949 ~ '1920-1949',#'56-80',
#'   fire.year >= 1950 & fire.year <= 1969 ~ '1950-1969',#'56-80',
#'   # fire.year >= 1971 & fire.year <= 1980 ~ '35-44',
#'   ##fire.year >= 1999 & fire.year <= 2001 ~ '1999-2001',#'31-55', 
#'   # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
#'   fire.year >= 1970 & fire.year <= 1985 ~ '1970-1985',
#'   fire.year >= 1986 & fire.year <= 2000 ~ '1986-2000',
#'   fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
#'   fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
#'   fire.year >= 2019 ~ '2019-2020'))#'0-4'))
#' 
#' summary(all.data)
#' 
#' all.data$stand.age.bin = with(all.data, factor(stand.age.bin, levels = c('2019-2020', '2011-2018', '1985-2010', '1960-1984', '1935-1959', '1910-1934', 'No Fire')))#c('0-4','5-30','31-55','56-80',
#'                                                                              #'81-95')))
#' 
#' all.data$fire.year.bin = with(all.data, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', '2001-2010', '1986-2000', '1970-1985', '1950-1969', '1920-1949', '1900-1919', 'No Fire')))#c('0-4','5-30','31-55','56-80',
#' 
#' summary(all.data %>% select(lf_evt_2001_mode))
summary(rx.pixel.data)

# test.data <- rx.pixel.data %>% dplyr::select(clm_precip_sum, clm_temp_mean, elevation, treat)
summary(test.data)

rx.match <- rx.pixel.data %>% filter(treat == 1) %>% dplyr::select(clm_precip_sum, clm_temp_mean, elevation) # 1 means prescribed burn
control.match <- rx.pixel.data %>% filter(treat == 0) %>% dplyr::select(clm_precip_sum, clm_temp_mean, elevation) # 0 means control

#Calculate mahalanobis distance
# rx.pixel.data$mahalanobis <- mahalanobis(test.data, colMeans(test.data), cov(test.data))
# rx.pixel.data$pvalue <- pchisq(rx.pixel.data$mahalanobis, df=2, lower.tail=FALSE)
# rx.pixel.data %>% summary()
# geom_hist

#Cdist isn't work, I might have to try another way.
cdist(data.matrix(rx.match), data.matrix(control.match), metric = 'euclidean', p = 2)

ggplot(data = rx.pixel.data) + geom_histogram(mapping = aes(x = mahalanobis))
#Match the control (2) and sample (1) pixels
#Nearest neighbor doesn't work well
#' nn.match <- matchit(treat ~ clm_precip_sum + clm_temp_mean + elevation, data = rx.pixel.data,
#'         method = "nearest", distance = "glm", replacement = TRUE)
#' nn.match
#' #Test out the matches
#' plot(summary(nn.match))
#' plot(nn.match, type = "jitter", interactive = FALSE)
#' plot(nn.match, type = "qq", interactive = FALSE)
#' 
#' nn.mdata <- match.data(nn.match)
#' summary(nn.mdata)
#' nn.mdata$stand.age.bin = with(nn.mdata, factor(stand.age.bin, levels = c('2019-2020', '2011-2018', '1985-2010', '1960-1984', '1935-1959', '1910-1934', 'No Fire')))#c('0-4','5-30','31-55','56-80',
#' #'81-95')))
#' 
#' nn.mdata$fire.year.bin = with(nn.mdata, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', '2001-2010', '1986-2000', '1970-1985', '1950-1969', '1920-1949', '1900-1919', 'No Fire')))#c('0-4','5-30','31-55','56-80',

# p14 <- ggplot() +
#   #Data Summary
#   geom_point(data = nn.mdata %>% dplyr::filter() %>% 
#                dplyr::group_by(system.index) %>% 
#                summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), Water_Stress = Water_Stress[vi.year == 2015], subclass = subclass[vi.year == 2015]),
#               mapping = aes(x = Water_Stress, y = dTree, color = subclass), size = 2) + 
#   # geom_errorbar(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 2 & fire.year <= 2010 & fire.year >= 1920) %>% 
#   #                 dplyr::group_by(system.index) %>% 
#   #                 summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), stand.age.grp = stand.age.grp[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
#   #               mapping = aes(x = stand.age.grp * 10, y = dTree), stat = 'summary') + theme_bw() +
#   geom_smooth(data = nn.mdata %>% dplyr::filter() %>% 
#                dplyr::group_by(system.index) %>% 
#                summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), Water_Stress = Water_Stress[vi.year == 2015], subclass = subclass[vi.year == 2015]),
#              mapping = aes(x = Water_Stress, y = dTree, color = subclass), size = 2, method = 'lm') + 
#   # geom_errorbar(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 2 & fire.year <= 2010 & fire.year >= 1920) %>% 
#   #                 dplyr::group_by(system.index) %>% 
#   #                 summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), stand.age.grp = stand.age.grp[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
#   #               mapping = aes(x = stand.age.grp * 10, y = dTree), stat = 'summary') + theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   xlab('Water_Stress (mm)') + ylab('Change in Tree Cover (%)')
# p14
# #Try optimal match
# gen.match <- matchit(treat ~ clm_precip_sum_mean + clm_temp_mean_mean + elevation, data = all.data,
#                     method = "genetic", distance = "glm", replacement = FALSE, pop.size = )
# 
# #Test out the matches
# plot(summary(gen.match))
# plot(gen.match, type = "jitter", interactive = FALSE)
# plot(gen.match, type = "qq", interactive = FALSE)
# 
# summary(opt.match)
# ?matchit()
# all.data %>% dplyr::select(lft_evt_2001_mode)
# #Try full match
# full.match <- matchit(treat ~ clm_precip_sum_mean + clm_temp_mean_mean + elevation + lf_evt_2001_mode, data = all.data,
#                      method = "full", distance = "glm", replacement = FALSE, exact = ~ lf_evt_2001_mode, link = "probit" )
