#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: March 22, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'purrr')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('zoo'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(zoo)
#Set the working directory
# setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')
setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/landsat')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the Wildfire data
frap.fire.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_wildfire_500pt_200mm_5tree_ts8_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
frap.fire.data$treatment <- 'Disturb'

#Add the Wildfire buffer data
frap.control.data <- read.csv(file.path(dir_in, "control_south_sierra_FRAP_2km_buffer_500pt_200mm_5tree_ts16_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

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
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_500pt_200mm_5tree_ts8_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
rx.data$treatment <- 'Disturb'

#Add teh Rx fire buffer data
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_2km_buffer_500pt_200mm_5tree_ts16_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

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

#Combine all the data together
pixel.data <- rbind(frap.pixel.data, rx.pixel.data)

`%notin%` <- Negate(`%in%`)
# summary(pixel.data)
#Convert fire data -9999 to NAs
# pixel.data[pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
# pixel.data[pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
pixel.data[pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
pixel.data[pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
pixel.data[pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
pixel.data[pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA

#Use the FRAP fire perimeter year (use fire year 2010)
pixel.data$fire.year <- pixel.data$fire_year_2010

#Add the Fire types
pixel.data <- pixel.data %>% mutate(fire.type.bin = case_when(
  fire_type_2010 == 1 ~ 'Wildfire',
  fire_type_2010 == 2 ~ 'Rxfire'
))
# 
# summary(pixel.data)
#Make treatment a factor
pixel.data$treatment = with(pixel.data, factor(treatment, levels = c('Control', 'Disturb')))#

#Recode the veg type data
# pixel.data$veg_name <- recode(.x=pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
#                               '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
#                               '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

#Select strat categories for fire treatments
#Select strat categories for fire treatments
frap.disturb <- pixel.data %>% filter(fire.type.bin == 'Wildfire' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n()) 
rx.disturb <- pixel.data %>% filter(fire.type.bin == 'Rxfire' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n()) 
# print(frap.strat)

frap.control <- pixel.data %>% filter(fire.type.bin == 'Wildfire' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n())
rx.control <- pixel.data %>% filter(fire.type.bin == 'Rxfire' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n()) 

#Get final stratlayers and numbers to sample from each
frap.strat <- inner_join(frap.disturb, frap.control, by = 'stratlayer') %>% 
  group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) 

rx.strat <- inner_join(rx.disturb, rx.control, by = 'stratlayer') %>% #Inner Join the disturb and control data sets
  group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) #Take the minimum of the number of pixels as the sample number

# rx.strat %>% pull(n)

#Set the random number seed
set.seed(4561)
# colnames(pixel.data %>% dplyr::select(-'AET':'tpa_max'))
#Sample the prescribed fire control pixels
rx.sample <- pixel.data %>%
  filter(treatment == 'Control' & fire.type.bin == 'Rxfire' & stratlayer %in% (rx.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (rx.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work, .y = n
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the Wildfire Control control pixels
frap.sample <- pixel.data %>%
  filter(treatment == 'Control' & fire.type.bin == 'Wildfire' & stratlayer %in% (frap.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (frap.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the moderate severity control pixels

#Make sure the stratlayer bins match with the sampled control bins
#Make sure the stratlayer bins match with the sampled control bins
#Make sure the stratlayer disturb bins match with the sampled control bins
rx.disturb <- pixel.data %>%
  filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire' & stratlayer %in% (rx.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (rx.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work, .y = n
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the Wildfire Disturb pixels
frap.disturb <- pixel.data %>%
  filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire' & stratlayer %in% (frap.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (frap.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data                                                                                                                 fire.type.bin == 'Wildfire' ~ stratlayer %in% (frap.strat %>% pull(stratlayer))))

#Combine the sampled data back together
pixel.sample <- rbind(frap.disturb, rx.disturb, rx.sample, frap.sample)

#Convert data to long format
#This should be moved later
pixel.sample <- pixel.sample %>% 
  pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

#Convert the year outputs to actual years
pixel.sample$year <- as.numeric(pixel.sample$year) + 1984 

#Convert missing TPA data to NAs
pixel.sample[pixel.sample$tpa_max < 0,]$tpa_max <- NA

#Convert to trees per hectare
pixel.sample$tpa_max <- pixel.sample$tpa_max * 2.47105

#Make the dates into date time format for R
pixel.sample$date <- as.Date(as.character(pixel.sample$year), format = '%Y')
pixel.sample$vi.year <- pixel.sample$year
pixel.sample$stand.age <- as.numeric(pixel.sample$year) - as.numeric(pixel.sample$fire.year) 

#Update Cover data to 100% scale
pixel.sample$Tree_Cover.2 <- pixel.sample$Tree_Cover / 100
pixel.sample$Shrub_Cover.2 <- pixel.sample$Shrub_Cover / 100
pixel.sample$Herb_Cover.2 <- pixel.sample$Herb_Cover / 100
pixel.sample$Bare_Cover.2 <- pixel.sample$Bare_Cover / 100
# pixel.sample$Tree_Cover.2 <- pixel.sample$Tree_Cover

#Rename Montana Tree Cover
pixel.sample$Tree_Cover <- pixel.sample$TRE
pixel.sample$Shrub_Cover <- pixel.sample$SHR
pixel.sample$Herb_Cover <- pixel.sample$AFG + pixel.sample$PFG
pixel.sample$Bare_Cover <- pixel.sample$BGR 

#Convert the SPI48 scale back to decimal
pixel.sample$SPI48 <- pixel.sample$SPI48 / 100

#Try to fix soil moisture by dividing by 10
pixel.sample$Soil_Moisture <- pixel.sample$Soil_Moisture / 10

#Calculate Pr-ET
pixel.sample$PrET <- pixel.sample$ppt - pixel.sample$AET

#Separate the data
pixel.sample <- pixel.sample %>% mutate(std.year.bin = case_when(
  # fire.year < 1980 ~ '< 1980',
  fire.year >= 1984 & fire.year <= 1990 ~ '1984-1990',
  fire.year >= 1991 & fire.year <= 1995 ~ '1991-1995',
  fire.year >= 1996 & fire.year <= 2000 ~ '1996-2000',
  fire.year >= 2001 & fire.year <= 2005 ~ '2001-2005',
  fire.year >= 2006 & fire.year <= 2010 ~ '2006-2010'))

pixel.sample$std.year.bin = with(pixel.sample, factor(std.year.bin, levels = c('2006-2010', '2001-2005','1996-2000', '1991-1995','1984-1990')))#
# summary(pixel.sample)
#Tree Cover versus Elevation versus Latitude
#Tree Cover versus Elevation versus Latitude
p1 <- ggplot() +
  #Data Summary
  geom_bin2d(data = pixel.sample %>% #sev.bin != 'Unchanged' & 
               filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
                         Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2011, 2012)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = Water_Stress, y = Tree_Cover, fill = dTree, group = dTree), binwidth = c(500, 10)) + 
  theme_bw() +
  scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-80, 25), midpoint = 0, na.value = 'transparent') +
  # scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ fire.type.bin) +
  xlab('Tree Cover (%)')
p1

p2 <- ggplot() +
  #Data Summary
  geom_bin2d(data = pixel.sample %>% #sev.bin != 'Unchanged' & 
               filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]), 
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                         tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE),
                         Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2011, 2012)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = Water_Stress, y = Tree_Cover, fill = tpa_max, group = tpa_max), binwidth = c(500, 10)) + 
  theme_bw() +
  scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
  # scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ fire.type.bin) +
  xlab('Water Stress') + ylab('Tree Cover (%)')
p2

f1 <- ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f1
#Save the data
ggsave(filename = 'Fig2a_frap_fire_dieoff_tree_cover_fireyear_geographic_distribution.png', height=20, width= 24, units = 'cm', dpi=900)

# p1 <- ggplot() +
#   #Data Summary
#   geom_bin2d(data = pixel.sample %>% 
#                filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
#                filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
#                                 fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#                          Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2013, 2014)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(y = latitude, x = elevation, fill = Tree_Cover, group = Tree_Cover), binwidth = c(500, 0.25)) + 
#   theme_bw() +
#   scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
#   # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ fire.type.bin) +
#   ylab('Latitude')
# p1
# 
# # pixel.data %>% summary()
# p2<- ggplot() +
#   #Data Summary
#   geom_bin2d(data = pixel.sample %>% 
#                filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
#                filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
#                                 fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
#                          Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2017, 2018)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(y = latitude, x = elevation, fill = dTree, group = dTree), binwidth = c(500, 0.25)) +  
#   theme_bw() +
#   # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-10, 5), midpoint = 0, na.value = 'transparent') +  #  
#   theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ fire.type.bin) +
#   ylab('Latitude')
# p2
# 
# #ADS die-off
# p3 <- ggplot() +
#   geom_bin2d(data = pixel.sample %>% 
#                filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
#                filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
#                                 fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>%
#                summarize(tpa_max = max(tpa_max[vi.year %in% c(2013, 2014, 2015, 2016, 2017, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015], elevation = elevation[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(y = latitude, x = elevation, fill = tpa_max, group = tpa_max), binwidth = c(500, 0.25)) + 
#   scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6))  + facet_grid(treatment ~ fire.type.bin) +
#   ylab('Latitude') + xlab('Elevation (m)')
# p3             
# 
# p4<- ggplot() +
#   #Data Summary
#   geom_bin2d(data = pixel.sample %>% 
#                #Did a filter by elevation (less than or equal to 3000), but one by pixel sample would probably be better.
#                filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
#                filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
#                                 fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
#                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#                          Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2018, 2019)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], count = sum(elevation[vi.year == 2015]), n = n(), SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(y = latitude, x = elevation), binwidth = c(500, 0.25)) +  
#   theme_bw() +
#   # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   # scale_fill_gradient2(name = "Die-off (% Tree Cover)", limits = c(-50, 20), midpoint = 0, low = "red", mid = "white", high = "blue", na.value = 'transparent') +
#   theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ fire.type.bin) +
#   ylab('Latitude') + xlab('Elevation (m)')
# p4
# 
# f1 <- ggarrange(p1, p2, p3, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
# f1
#Save the data
ggsave(filename = 'Fig2a_frap_wildfire_dieoff_tree_cover_geographic_distribution.png', height=24, width= 14, units = 'cm', dpi=900)

#Figure 5: Bar Chats, this could be for statistics
p11 <- ggplot() +
  #Calculate the Mean
  stat_summary(data = pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                         RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.type.bin, fill = treatment, y = dTree), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  #Calculate the Standard Error
  stat_summary(data = pixel.sample %>% 
                  filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>% 
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                            RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]), 
                            Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = fire.type.bin, color = treatment, y = dTree), 
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
  stat_summary(data = pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                         RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]), Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.type.bin, fill = treatment, y = RdTree * 100), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.sample %>% 
                  filter(fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>% 
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                            RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]), Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = fire.type.bin, color = treatment, y = RdTree * 100), 
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
  stat_summary(data = pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = fire.type.bin, y = tpa_max, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.sample %>% 
                  filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>% 
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
                mapping = aes(x = fire.type.bin, y = tpa_max, color = treatment), 
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
  stat_summary(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011, 2012)])),
               mapping = aes(x = fire.type.bin, y = Tree_Cover, fill = treatment), 
               fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011, 2012)])),
               mapping = aes(x = fire.type.bin, y = Tree_Cover, color = treatment), 
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
  stat_summary(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), treatment = treatment[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
               mapping = aes(x = fire.type.bin, y = Water_Stress, fill = treatment), 
               fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), treatment = treatment[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
               mapping = aes(x = fire.type.bin, y = Water_Stress, color = treatment), 
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
# summary(pixel.sample)

#ADS die-off
p16 <- ggplot() +
  #Create bars and error bars
  geom_point(data = pixel.sample %>% 
                 filter(!is.na(tpa_max) & tpa_max > 0 & fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dADS.mean = (mean(tpa_max[treatment == 'Disturb']) - mean(tpa_max[treatment == 'Control']))/ mean(tpa_max[treatment == 'Control'])),
               mapping = aes(x = fire.type.bin, y = dADS.mean * 100), position = position_dodge(width = 0.5)) +  
  geom_errorbar(data = pixel.sample %>%
                 filter(!is.na(tpa_max) & tpa_max > 0 & fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dADS.mean = (mean(tpa_max[treatment == 'Disturb']) - mean(tpa_max[treatment == 'Control']))/ mean(tpa_max[treatment == 'Control']),
          dADS.sd = (sd(tpa_max[treatment == 'Disturb'])^2 + sd(tpa_max[treatment == 'Control'])^2) / sd(tpa_max[treatment == 'Control']), 
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
  ylab('Difference in Die-off (% ADS)') 
p16

# ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)
#Pre-Die-off Tree Cover
p17 <- ggplot() +
  #Create the Error Bars
  geom_point(data = pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
               #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
               dplyr::group_by(system.index, treatment, fire.type.bin) %>%
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)]))) %>%
               ungroup() %>%
               group_by(fire.type.bin) %>%
               summarize(dTree.mean = (mean(dTree[treatment == 'Disturb']) - mean(dTree[treatment == 'Control']))/ mean(dTree[treatment == 'Control'])),
             mapping = aes(x = fire.type.bin, y = dTree.mean * 100), position = position_dodge(width = 0.5)) +  
  geom_errorbar(data = pixel.sample %>% 
                  filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)]))) %>%
                  ungroup() %>%
                  group_by(fire.type.bin) %>%
                  summarize(dTree.mean = (mean(dTree[treatment == 'Disturb']) - mean(dTree[treatment == 'Control']))/ mean(dTree[treatment == 'Control']),
                            dTree.sd = (sd(dTree[treatment == 'Disturb'])^2 + sd(dTree[treatment == 'Control'])^2) / sd(dTree[treatment == 'Control']), 
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
  ylab('Difference in Die-off (% dTree)')
p17

#Pre-Die-off Tree Cover
p18 <- ggplot() +
  #Create the Error Bars
  geom_point(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011, 2012)])) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dTree.mean = (mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']))/ mean(Tree_Cover[treatment == 'Control'])),
               mapping = aes(x = fire.type.bin, y = dTree.mean * 100), position = position_dodge(width = 0.5)) +  
  geom_errorbar(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011, 2012)]))%>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dTree.mean = (mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']))/ mean(Tree_Cover[treatment == 'Control']),
                           dTree.sd = (sd(Tree_Cover[treatment == 'Disturb'])^2 + sd(Tree_Cover[treatment == 'Control'])^2) / sd(Tree_Cover[treatment == 'Control']), 
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
p18

#Water Stress
p19 <- ggplot() +
  #Create the Error Bars
  geom_point(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(#tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), treatment = treatment[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])) %>%
               ungroup() %>%
               group_by(fire.type.bin) %>%
               summarize(dWater_Stress.mean = (mean(Water_Stress[treatment == 'Disturb']) - mean(Water_Stress[treatment == 'Control']))/ mean(Water_Stress[treatment == 'Control'])),
             mapping = aes(x = fire.type.bin, y = dWater_Stress.mean * 100), position = position_dodge(width = 0.5)) + 
  geom_errorbar(data = pixel.sample %>% 
                 filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                 #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                 dplyr::group_by(system.index, treatment, fire.type.bin) %>%
                 summarize(#tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), treatment = treatment[vi.year == 2010], 
                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])) %>%
                 ungroup() %>%
                 group_by(fire.type.bin) %>%
                 summarize(dWater_Stress.mean = (mean(Water_Stress[treatment == 'Disturb']) - mean(Water_Stress[treatment == 'Control']))/ mean(Water_Stress[treatment == 'Control']),
                           dWater_Stress.sd = (sd(Water_Stress[treatment == 'Disturb'])^2 + sd(Water_Stress[treatment == 'Control'])^2) / sd(Water_Stress[treatment == 'Control']), 
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
p19

#Combine the Panels
f5 <- ggarrange(p16, p17, p18, p19,  ncol = 1, nrow = 4, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f5

ggsave(filename = 'Fig12a_wild_fire_bar_chart_comparison.png', height=24, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills

# summary(pixel.sample)
p20 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, treatment, fire.type.bin,std.year.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub', linetype = treatment), size = 1) +
  #Shrub Cover 95% CI
  geom_errorbar(data = pixel.sample %>% 
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, color = "Shrub",  linetype = treatment), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree',  linetype = treatment), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>% 
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, color = "Tree",  linetype = treatment), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb',  linetype = treatment), size = 1) + 
  #Herb Cover 95% CI
  geom_errorbar(data = pixel.sample %>% 
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, color = "Herb",  linetype = treatment), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare',  linetype = treatment), size = 1) + 
  #Bare Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
                summarize(Bare_Cover.mean = mean(Bare_Cover),
                          Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
              mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            x = stand.age, color = "Bare",  linetype = treatment), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') + facet_grid(std.year.bin ~ fire.type.bin) +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p20

#Save the data
ggsave(filename = 'Fig8a_frap_stand_age_veg_cover.png', height=18, width= 20, units = 'cm', dpi=900)

p21 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Tree_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']),
                            Tree_Cover.sd = sd(Tree_Cover[treatment == 'Disturb'])^2 + sd(Tree_Cover[treatment == 'Control'])^2, 
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
p21

#Save the data
ggsave(filename = 'Fig9a_frap_stand_age_tree_cover.png', height=8, width= 20, units = 'cm', dpi=900)

#AET change with wildfire (FRAP)
p22 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#Create a Tree Cover line
geom_line(data = pixel.sample %>%
            filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
            # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
            #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
            # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
            # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
            # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
            group_by(stand.age, fire.type.bin) %>%
            summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control'])), 
          mapping = aes(x = stand.age, y = AET.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control']),
                            AET.sd = sd(AET[treatment == 'Disturb'])^2 + sd(AET[treatment == 'Control'])^2, 
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
p22

#Save the data
ggsave(filename = 'Fig10a_frap_stand_age_AET.png', height=8, width= 20, units = 'cm', dpi=900)

#Pr-ET change with wildfire (FRAP)
p23 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, fire.type.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Shrub_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control']),
                            Shrub_Cover.sd = sd(Shrub_Cover[treatment == 'Disturb'])^2 + sd(Shrub_Cover[treatment == 'Control'])^2, 
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
p23

#Save the data
ggsave(filename = 'Fig11a_frap_stand_age_shrub.png', height=8, width= 20, units = 'cm', dpi=900)

# pixel.data %>% summary()

#Do stand age versus die-off
p24 <- ggplot(data = pixel.sample %>% filter(fire.year <= 2010 & fire.year >= 1980 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                          tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE),
                          Water_Stress = Water_Stress[vi.year == 2015], stand.age = stand.age[vi.year == 2015]),
              mapping = aes(x = stand.age, y = dTree)) + 
  facet_grid(.~ fire.type.bin) + theme_bw() +
  geom_point(mapping = aes(color = treatment), size = 1) + 
  geom_smooth(mapping = aes(color = treatment), method = 'lm') +
  stat_cor(mapping = aes(color = treatment)) +
  xlab('Years Since Fire') + ylab('Die-off (Relative Tree Cover %)')
p24

#Save the data
ggsave(filename = 'Fig7a_frap_rx_stand_age_dieoff.png', height=12, width= 24, units = 'cm', dpi=900)
