#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: March 14, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('zoo'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(zoo)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')

#The data directory
# dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the Wildfire data
frap.fire.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_wildfire_500pt_100elev_5tree_ts8_300m_20230314.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
frap.fire.data$treatment <- 'Disturb'

#Add the Wildfire buffer data
frap.control.data <- read.csv(file.path(dir_in, "control_south_sierra_FRAP_2km_buffer_500pt_100elev_5tree_ts16_300m_20230314.csv"), header = TRUE, na.strings = "NaN")

#Add Fire Columns
frap.control.data$fire_count_2010 <- -9999
frap.control.data$fire_type_2019 <- -9999
frap.control.data$fire_year_2019 <- -9999
frap.control.data$fire_year_2019 <- -9999
frap.control.data$fire_count_2019 <- -9999
frap.control.data$fire_type_2020 <- -9999
frap.control.data$fire_year_2020 <- -9999
frap.control.data$fire_count_2020 <- -9999

#Add the treatment Column
frap.control.data$treatment <- 'Control' 

#Combine the data together
frap.pixel.data <- rbind(frap.fire.data, frap.control.data)

#Add the Rx fire data
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_500pt_100elev_5tree_ts8_300m_20230314.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
rx.data$treatment <- 'Disturb'

#Add teh Rx fire buffer data
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_2km_buffer_500pt_100elev_5tree_ts16_300m_20230314.csv"), header = TRUE, na.strings = "NaN")

#Add Fire Columns
rx.control.data$fire_count_2010 <- -9999
rx.control.data$fire_type_2019 <- -9999
rx.control.data$fire_year_2019 <- -9999
rx.control.data$fire_year_2019 <- -9999
rx.control.data$fire_count_2019 <- -9999
rx.control.data$fire_type_2020 <- -9999
rx.control.data$fire_year_2020 <- -9999
rx.control.data$fire_count_2020 <- -9999

#Add the treatment column
rx.control.data$treatment <- 'Control' #Try making this 1-km versus, 2-km

#Combine the data together
rx.pixel.data <- rbind(rx.data, rx.control.data)
# pixel.data <- rbind(combine.data, control.data.2km)
summary(rx.pixel.data)

#Combine the wildfire and Rx fire data together
pixel.data <- combine(frap.pixel.data, rx.pixel.data)

summary(pixel.data)

`%notin%` <- Negate(`%in%`)

#Convert data to long format
pixel.data <- pixel.data %>% 
  pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

pixel.data$year <- as.numeric(pixel.data$year) + 1984 

#Convert missing TPA data to NAs
pixel.data[pixel.data$tpa_max < 0,]$tpa_max <- NA

#Convert fire data -9999 to NAs
pixel.data[pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
pixel.data[pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
pixel.data[pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
pixel.data[pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
pixel.data[pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
pixel.data[pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA

#Convert to trees per hectare
pixel.data$tpa_max <- pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
pixel.data$date <- as.Date(as.character(pixel.data$year), format = '%Y')
# pixel.data$vi.year <- format(pixel.data$date , '%Y')
pixel.data$vi.year <- pixel.data$year
#Use the FRAP fire perimeter year (use fire year 2010)
pixel.data$fire.year <- pixel.data$fire_year_2010
pixel.data$stand.age <- as.numeric(pixel.data$year) - as.numeric(pixel.data$fire.year) 

#Update Cover data to 100% scale
pixel.data$Tree_Cover <- pixel.data$Tree_Cover / 100
pixel.data$Shrub_Cover <- pixel.data$Shrub_Cover / 100
pixel.data$Herb_Cover <- pixel.data$Herb_Cover / 100
pixel.data$Bare_Cover <- pixel.data$Bare_Cover / 100

#Convert the SPI48 scale back to decimal
pixel.data$SPI48 <- pixel.data$SPI48 / 100

#Try to fix soil moisture by dividing by 10
pixel.data$Soil_Moisture <- pixel.data$Soil_Moisture / 10

#Calculate Pr-ET
pixel.data$PrET <- pixel.data$ppt - pixel.data$AET

pixel.data %>% summary()

pixel.data <- pixel.data %>% mutate(fire.year.bin = case_when(
  treatment == 'Control' | fire.year < 1980 ~ 'Control',
  fire.year >= 1980 & fire.year <= 2010 ~ 'Disturb',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
  fire.year >= 2019 ~ '2019-2020'))#'0-4'))

pixel.data <- pixel.data %>% mutate(fire.type.bin = case_when(
  fire_type_2010 == 1 ~ 'Wildfire',
  fire_type_2010 == 2 ~ 'Rxfire'
))

summary(pixel.data)

pixel.data$fire.year.bin = with(pixel.data, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', 'Control', 'Disturb')))#

#Recode the veg type data
pixel.data$veg_name <- recode(.x=pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
                              '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
                              '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')


#Select strat categories for fire treatments
frap.strat <- pixel.data %>% filter(fire.type.bin == 'Wildfire' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n() /35) %>% filter(n >= 20) %>% pull(stratlayer) 
rx.strat <- pixel.data %>% filter(fire.type.bin == 'Rxfire' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n() /35) %>% filter(n >= 5) %>% pull(stratlayer)

#Tree Cover versus Elevation versus Latitude
p1 <- ggplot() +
  #Data Summary
  geom_bin2d(data = pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2013, 2014)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = Tree_Cover, group = Tree_Cover), binwidth = c(500, 0.25)) + 
  theme_bw() +
  scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(fire.year.bin ~ fire.type.bin) +
  ylab('Latitude')
p1

# pixel.data %>% summary()
p2<- ggplot() +
  #Data Summary
  geom_bin2d(data = pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2017, 2018)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = dTree, group = dTree), binwidth = c(500, 0.25)) +  
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-10, 5), midpoint = 0, na.value = 'transparent') +  #  
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(fire.year.bin ~ fire.type.bin) +
  ylab('Latitude')
p2

#ADS die-off
p3 <- ggplot() +
  geom_bin2d(data = pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2013, 2014, 2015, 2016, 2017, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015], elevation = elevation[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = tpa_max, group = tpa_max), binwidth = c(500, 0.25)) + 
  scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6))  + facet_grid(fire.year.bin ~ fire.type.bin) +
  ylab('Latitude') + xlab('Elevation (m)')
p3             

p4<- ggplot() +
  #Data Summary
  geom_bin2d(data = pixel.data %>% 
               #Did a filter by elevation (less than or equal to 3000), but one by pixel sample would probably be better.
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2018, 2019)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], count = sum(elevation[vi.year == 2015]), n = n(), SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation), binwidth = c(500, 0.25)) +  
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  # scale_fill_gradient2(name = "Die-off (% Tree Cover)", limits = c(-50, 20), midpoint = 0, low = "red", mid = "white", high = "blue", na.value = 'transparent') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(fire.year.bin ~ fire.type.bin) +
  ylab('Latitude') + xlab('Elevation (m)')
p4

f1 <- ggarrange(p1, p2, p3, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f1
#Save the data
ggsave(filename = 'Fig2a_frap_wildfire_dieoff_tree_cover_geographic_distribution.png', height=24, width= 14, units = 'cm', dpi=900)

#Figure 5: Bar Chats, this could be for statistics
p11 <- ggplot() +
  #Calculate the Mean
  stat_summary(data = pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
                         RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.type.bin, fill = fire.year.bin, y = dTree), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  #Calculate the Standard Error
  stat_summary(data = pixel.data %>% 
                  filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>% 
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
                            RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                            Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = fire.type.bin, color = fire.year.bin, y = dTree), 
                fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.8, 0.75), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('dTree (%)')
p11

#RdTree Plot
p12 <- ggplot() +
  #Data Summary
  stat_summary(data = pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
                         RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.type.bin, fill = fire.year.bin, y = RdTree * 100), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.data %>% 
                  filter(fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>% 
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
                            RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = fire.type.bin, color = fire.year.bin, y = RdTree * 100), 
               fun.data = mean_se, geom = "errorbar", position = 'dodge', size = 1) + 
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('Relative dTree (%)')
p12

#ADS die-off
p13 <- ggplot() +
  #Create bars and error bars
  stat_summary(data = pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = fire.type.bin, y = tpa_max, fill = fire.year.bin), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.data %>% 
                  filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>% 
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
                mapping = aes(x = fire.type.bin, y = tpa_max, color = fire.year.bin), 
               fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Mortality (trees/ha)') 
p13

# ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)

#Pre-Die-off Tree Cover
p14 <- ggplot() +
  #Create the Error Bars
  stat_summary(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)])),
               mapping = aes(x = fire.type.bin, y = Tree_Cover, fill = fire.year.bin), 
               fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)])),
               mapping = aes(x = fire.type.bin, y = Tree_Cover, color = fire.year.bin), 
               fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Tree Cover (%)')
p14

#Water Stress
p15 <- ggplot() +
  #Create the Error Bars
  stat_summary(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
               mapping = aes(x = fire.type.bin, y = Water_Stress, fill = fire.year.bin), 
               fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
               mapping = aes(x = fire.type.bin, y = Water_Stress, color = fire.year.bin), 
               fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Fire Year') + ylab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) 
p15

#Combine the Panels
f4 <- ggarrange(p11, p12, p13, p14, p15,  ncol = 1, nrow = 5, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
f4

ggsave(filename = 'Fig5a_wild_fire_bar_chart_comparison.png', height=24, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

# pixel.data %>% 
#   filter(!is.na(tpa_max) & tpa_max > 0 & fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
#   filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
#                    fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
#   dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
#   summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]) %>%
#   ungroup() %>%
#   group_by(fire.type.bin, system.index) %>%
#   summarize(dADS.mean = mean((tpa_max[fire.year.bin == 'Disturb'] - tpa_max[fire.year.bin == 'Control'])/ tpa_max[fire.year.bin == 'Control']))

#Figure 12: Bar Chats, this could be for statistics
#ADS die-off
p16 <- ggplot() +
  #Create bars and error bars
  geom_point(data = pixel.data %>% 
                 filter(!is.na(tpa_max) & tpa_max > 0 & fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dADS.mean = (mean(tpa_max[fire.year.bin == 'Disturb']) - mean(tpa_max[fire.year.bin == 'Control']))/ mean(tpa_max[fire.year.bin == 'Control'])),
               mapping = aes(x = fire.type.bin, y = dADS.mean * 100), position = position_dodge(width = 0.5)) +  
  geom_errorbar(data = pixel.data %>%
                 filter(!is.na(tpa_max) & tpa_max > 0 & fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dADS.mean = (mean(tpa_max[fire.year.bin == 'Disturb']) - mean(tpa_max[fire.year.bin == 'Control']))/ mean(tpa_max[fire.year.bin == 'Control']),
          dADS.sd = (sd(tpa_max[fire.year.bin == 'Disturb'])^2 + sd(tpa_max[fire.year.bin == 'Control'])^2) / sd(tpa_max[fire.year.bin == 'Control']), 
          dADS.n = n()),
mapping = aes(ymin=dADS.mean*100 - 1.96*(dADS.sd*100 / dADS.n),
              ymax=dADS.mean*100 + 1.96*(dADS.sd*100 / dADS.n),
              x = fire.type.bin), position = position_dodge(width = 0.1)) +
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Difference in Mortality (%)') 
p16

# ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)

#Pre-Die-off Tree Cover
p17 <- ggplot() +
  #Create the Error Bars
  geom_point(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)])) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dTree.mean = (mean(Tree_Cover[fire.year.bin == 'Disturb']) - mean(Tree_Cover[fire.year.bin == 'Control']))/ mean(Tree_Cover[fire.year.bin == 'Control'])),
               mapping = aes(x = fire.type.bin, y = dTree.mean * 100), position = position_dodge(width = 0.5)) +  
  geom_errorbar(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)]))%>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dTree.mean = (mean(Tree_Cover[fire.year.bin == 'Disturb']) - mean(Tree_Cover[fire.year.bin == 'Control']))/ mean(Tree_Cover[fire.year.bin == 'Control']),
                           dTree.sd = (sd(Tree_Cover[fire.year.bin == 'Disturb'])^2 + sd(Tree_Cover[fire.year.bin == 'Control'])^2) / sd(Tree_Cover[fire.year.bin == 'Control']), 
                           dTree.n = n()),
               mapping = aes(ymin=dTree.mean*100 - 1.96*(dTree.sd*100 / dTree.n),
                             ymax=dTree.mean*100 + 1.96*(dTree.sd*100 / dTree.n),
                             x = fire.type.bin), position = position_dodge(width = 0.1)) +
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Change in Tree Cover (%)')
p17

#Water Stress
p18 <- ggplot() +
  #Create the Error Bars
  geom_point(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(#tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])) %>%
               ungroup() %>%
               group_by(fire.type.bin) %>%
               summarize(dWater_Stress.mean = (mean(Water_Stress[fire.year.bin == 'Disturb']) - mean(Water_Stress[fire.year.bin == 'Control']))/ mean(Water_Stress[fire.year.bin == 'Control'])),
             mapping = aes(x = fire.type.bin, y = dWater_Stress.mean * 100), position = position_dodge(width = 0.5)) + 
  geom_errorbar(data = pixel.data %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>%
                 summarize(#tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dWater_Stress.mean = (mean(Water_Stress[fire.year.bin == 'Disturb']) - mean(Water_Stress[fire.year.bin == 'Control']))/ mean(Water_Stress[fire.year.bin == 'Control']),
                           dWater_Stress.sd = (sd(Water_Stress[fire.year.bin == 'Disturb'])^2 + sd(Water_Stress[fire.year.bin == 'Control'])^2) / sd(Water_Stress[fire.year.bin == 'Control']), 
                           dWater_Stress.n = n()),
               mapping = aes(ymin=dWater_Stress.mean*100 - 1.96*(dWater_Stress.sd*100 / dWater_Stress.n),
                             ymax=dWater_Stress.mean*100 + 1.96*(dWater_Stress.sd*100 / dWater_Stress.n),
                             x = fire.type.bin), position = position_dodge(width = 0.1)) +
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Fire Type') + ylab('Change in Water Stress (%)') 
p18

#Combine the Panels
f5 <- ggarrange(p16, p17, p18,  ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
f5

ggsave(filename = 'Fig12a_wild_fire_bar_chart_comparison.png', height=24, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills
p19 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                               fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire perimeter and fier year by pixel match 
              group_by(stand.age, fire.year.bin, fire.type.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub', linetype = fire.year.bin), size = 1) +
  #Shrub Cover 95% CI
  geom_errorbar(data = pixel.data %>% 
                filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                   fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  group_by(stand.age, fire.year.bin, fire.type.bin) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, color = "Shrub",  linetype = fire.year.bin), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                               fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, fire.year.bin, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree',  linetype = fire.year.bin), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.data %>% 
                filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                   fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, fire.year.bin, fire.type.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, color = "Tree",  linetype = fire.year.bin), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                               fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, fire.year.bin, fire.type.bin) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb',  linetype = fire.year.bin), size = 1) + 
  #Herb Cover 95% CI
  geom_errorbar(data = pixel.data %>% 
                filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                   fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, fire.year.bin, fire.type.bin) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, color = "Herb",  linetype = fire.year.bin), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                               fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, fire.year.bin, fire.type.bin) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare',  linetype = fire.year.bin), size = 1) + 
  #Bare Cover 95% CI
  geom_errorbar(data = pixel.data %>%
                filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                   fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, fire.year.bin, fire.type.bin) %>%
                summarize(Bare_Cover.mean = mean(Bare_Cover),
                          Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
              mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            x = stand.age, color = "Bare",  linetype = fire.year.bin), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') + facet_grid(.~ fire.type.bin) +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p19

#Save the data
ggsave(filename = 'Fig8a_frap_stand_age_veg_cover.png', height=18, width= 20, units = 'cm', dpi=900)

p20 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                               fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover[fire.year.bin == 'Disturb']) - mean(Tree_Cover[fire.year.bin == 'Control'])), 
            mapping = aes(x = stand.age, y = Tree_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.data %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                   fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Tree_Cover.mean = mean(Tree_Cover[fire.year.bin == 'Disturb']) - mean(Tree_Cover[fire.year.bin == 'Control']),
                            Tree_Cover.sd = sd(Tree_Cover[fire.year.bin == 'Disturb'])^2 + sd(Tree_Cover[fire.year.bin == 'Control'])^2, 
                            Tree_Cover.n = n()),
                mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / Tree_Cover.n),
                              ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / Tree_Cover.n),
                              x = stand.age)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + facet_grid(.~ fire.type.bin) +
  guides(fill = "none") +
  ylab(expression('Tree Reduction (%)')) + xlab('Years Since Fire')
p20

#Save the data
ggsave(filename = 'Fig9a_frap_stand_age_tree_cover.png', height=12, width= 20, units = 'cm', dpi=900)

#AET change with wildfire (FRAP)
p21 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#Create a Tree Cover line
geom_line(data = pixel.data %>%
            filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
            filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                             fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
            # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
            # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
            # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
            group_by(stand.age, fire.type.bin) %>%
            summarize(AET.mean = mean(AET[fire.year.bin == 'Disturb']) - mean(AET[fire.year.bin == 'Control'])), 
          mapping = aes(x = stand.age, y = AET.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.data %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                   fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(AET.mean = mean(AET[fire.year.bin == 'Disturb']) - mean(AET[fire.year.bin == 'Control']),
                            AET.sd = sd(AET[fire.year.bin == 'Disturb'])^2 + sd(AET[fire.year.bin == 'Control'])^2, 
                            AET.n = n()),
                mapping = aes(ymin=AET.mean - 1.96*(AET.sd / AET.n),
                              ymax=AET.mean + 1.96*(AET.sd / AET.n),
                              x = stand.age)) +
theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + facet_grid(.~ fire.type.bin) +
  guides(fill = "none") +
  ylab(expression('AET Reduction (mm yr'^-1*')')) + xlab('Years Since Fire')
p21

#Save the data
ggsave(filename = 'Fig10a_frap_stand_age_AET.png', height=18, width= 20, units = 'cm', dpi=900)

#Pr-ET change with wildfire (FRAP)
p22 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                               fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, fire.type.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover[fire.year.bin == 'Disturb']) - mean(Shrub_Cover[fire.year.bin == 'Control'])), 
            mapping = aes(x = stand.age, y = Shrub_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.data %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                   fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Shrub_Cover.mean = mean(Shrub_Cover[fire.year.bin == 'Disturb']) - mean(Shrub_Cover[fire.year.bin == 'Control']),
                            Shrub_Cover.sd = sd(Shrub_Cover[fire.year.bin == 'Disturb'])^2 + sd(Shrub_Cover[fire.year.bin == 'Control'])^2, 
                            Shrub_Cover.n = n()),
                mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / Shrub_Cover.n),
                              ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / Shrub_Cover.n),
                              x = stand.age)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + facet_grid(.~ fire.type.bin) +
  guides(fill = "none") +
  ylab(expression('Shrub Change (%)')) + xlab('Years Since Fire')
p22

#Save the data
ggsave(filename = 'Fig11a_frap_stand_age_shrub.png', height=18, width= 20, units = 'cm', dpi=900)

pixel.data %>% summary()

#Do stand age versus die-off
p23 <- ggplot(data = pixel.data %>% filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                                 fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                dplyr::group_by(system.index, fire.year.bin, fire.type.bin) %>% 
                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
                          tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE),
                          Water_Stress = Water_Stress[vi.year == 2015], stand.age = stand.age[vi.year == 2015]),
              mapping = aes(x = stand.age, y = tpa_max)) + 
  facet_grid(.~ fire.type.bin) + theme_bw() +
  geom_point(mapping = aes(color = fire.year.bin), size = 1) + 
  geom_smooth(mapping = aes(color = fire.year.bin), method = 'lm') +
  stat_cor(mapping = aes(color = fire.year.bin)) +
  xlab('Years Since Fire') + ylab(expression('Die-off (trees ha'^-1*')'))
p23

#Save the data
ggsave(filename = 'Fig7a_frap_rx_stand_age_dieoff.png', height=12, width= 24, units = 'cm', dpi=900)
